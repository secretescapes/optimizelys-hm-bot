module AWS.Dynamo where

import Prelude
import Common (renderErrors)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept, withExcept)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either, either)
import Data.Foreign (Foreign, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write, writeProp, readProp)
import Data.List.NonEmpty (NonEmptyList, toUnfoldable)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.String (joinWith)

foreign import data DYNAMO :: !

foreign import invokeForeign
    :: forall eff. Fn4 String
                       Foreign
                       (Error -> Eff (dynamo :: DYNAMO | eff) Unit)
                       (Foreign -> Eff (dynamo :: DYNAMO | eff) Unit)
                       (Eff (dynamo :: DYNAMO |eff) Unit)

type Dyneff eff = Eff (dynamo :: DYNAMO | eff) Unit

invoke' :: forall a b eff . (AsForeign a, IsForeign b)
    => String -> a -> (Error -> Dyneff eff) -> (b -> Dyneff eff) -> Dyneff eff
invoke' name params errcb cb = runFn4 invokeForeign name (write params) errcb (either errcb cb <<< decode)
    where
        decode :: forall f. (IsForeign f) => Foreign -> Either Error f
        decode f = runExcept $ withExcept (error <<< renderErrors) $ read f

type Dynaff eff r = Aff (dynamo :: DYNAMO | eff) r

invoke :: forall a b eff. (AsForeign a, IsForeign b) => String -> a -> Dynaff eff b
invoke methodName = makeAff <<< invoke' methodName

newtype Tablename a = Tablename String
newtype HashKey a hk = HashKey String
newtype SortKey a sk = SortKey String

data Table a hk sk = Table
    { name :: Tablename a
    , hashkey :: HashKey a hk
    , sortkey :: SortKey a sk
    }

instance tablenameAsForeign :: AsForeign (Tablename a) where
    write (Tablename s) = write s

type PutParams a =
    { tablename :: Tablename a
    , item :: a
    -- TODO add ConditionExpression, Return{Values,ConsumedCapacity,ItemCollectionMetrics}
    }
newtype PutParams' a = PutParams' (PutParams a)

instance putParamsAsForeign :: (AsForeign a) => AsForeign (PutParams' a) where
    write (PutParams' {tablename, item}) = writeObject [ writeProp "TableName" tablename
                                                       , writeProp "Item" item]

put :: forall eff a. (AsForeign a) => PutParams a -> Dynaff eff Foreign
put = invoke "put" <<< PutParams'

data Order = Ascending | Descending

instance orderAsForeign :: AsForeign Order where
    write Ascending = write true
    write Descending = write false

data AttrVal a typ = AttrVal
    { name :: String
    , value :: typ
    }

hashAttr :: forall a typ. (HashKey a typ) -> typ -> AttrVal a typ
hashAttr (HashKey name) value = AttrVal
    { name
    , value
    }

sortAttr :: forall a typ. (SortKey a typ) -> typ -> AttrVal a typ
sortAttr (SortKey name) value = AttrVal
    { name
    , value
    }

data KeyExpr a hk sk = KeyEq (HashKey a hk) (AttrVal a hk) (Maybe (SubKeyExpr a sk))

data BinaryOp = CEQ | CNEQ | CLT | CLE | CBT | CBE

showOp :: BinaryOp -> String
showOp CEQ = "="
showOp CNEQ = "<>"
showOp CLT = "<"
showOp CLE = "<="
showOp CBT = ">"
showOp CBE = ">="

data SubKeyExpr a sk
    = BinCond BinaryOp (SortKey a sk) (AttrVal a sk)
    | Between (SortKey a sk) (AttrVal a sk) (AttrVal a sk)
    | InCond (SortKey a sk) (NonEmptyList (AttrVal a sk))
    | AndCond (SubKeyExpr a sk) (SubKeyExpr a sk)

compileCondition :: forall a hk sk. KeyExpr a hk sk -> String
compileCondition (KeyEq (HashKey hkey) (AttrVal {name}) subCond)
    = hkey <> "=:" <> name <> (maybe "" doCompile subCond)
        where
            doCompile cond = " AND " <> compileSubCondition cond

compileSubCondition :: forall a sk. SubKeyExpr a sk -> String
compileSubCondition (AndCond cond1 cond2) = compileSubCondition cond1 <> " AND " <> compileSubCondition cond2
compileSubCondition (InCond (SortKey skey) attrs)
    = skey <> " IN (" <> (joinWith "," (toUnfoldable $ map (\(AttrVal av) -> ":" <> av.name) attrs)) <> ")"
compileSubCondition (Between (SortKey skey) (AttrVal lower) (AttrVal higher))
    = skey <> " BETWEEN :" <> lower.name <> " AND :" <> higher.name
compileSubCondition (BinCond binaryOp (SortKey skey) (AttrVal {name}))
    = skey <> showOp binaryOp <> ":" <> name

attrValsToForeign :: forall a hk sk
                   . (AsForeign hk, AsForeign sk)
                  => { keyAttr:: AttrVal a hk
                     , sortAttrs :: Array (AttrVal a sk) } -> Foreign
attrValsToForeign {keyAttr: AttrVal key, sortAttrs}
    = writeObject $ [ writeProp (":" <> key.name) key.value] <> sortProps
        where
            sortProps = map (\(AttrVal {name, value}) -> writeProp (":" <> name) value) sortAttrs

type QueryParams a hk sk =
    { tablename :: Tablename a
    , limit :: Maybe Int
    , order :: Order
    , keyCondition :: KeyExpr a hk sk
    , attrVals :: { keyAttr:: AttrVal a hk
                  , sortAttrs :: Array (AttrVal a sk) }
    }

defaultQuery :: forall a hk sk. (Table a hk sk) -> hk -> QueryParams a hk sk
defaultQuery (Table table) keyValue =
    { tablename : table.name
    , limit : Nothing
    , order : Ascending
    , keyCondition : KeyEq table.hashkey keyAttr Nothing
    , attrVals : {keyAttr, sortAttrs : []}
    }
    where
        keyAttr = hashAttr table.hashkey keyValue

newtype QueryParams' a hk sk = QueryParams' (QueryParams a hk sk)

instance queryParamsAsForeign :: (AsForeign hk, AsForeign sk) => AsForeign (QueryParams' a hk sk) where
    write (QueryParams' {tablename, limit, order, keyCondition, attrVals})
        = writeObject $ [ writeProp "TableName" tablename
                        , writeProp "ScanIndexForward" order
                        , writeProp "KeyConditionExpression" $ compileCondition keyCondition
                        , writeProp "ExpressionAttributeValues" $ attrValsToForeign attrVals
                        ] <> maybe [] (\l -> [writeProp "Limit" l]) limit

-- {TableName:"Test", Limit:1,ScanIndexForward:false, KeyConditionExpression:"id=:v", ExpressionAttributeValues:{":v":"foo"}}
-- TODO add ProjectionExpression, ConsistentRead, FilterExpression, ExclusiveStartKey, ReturnConsumedCapacity, ExpressionAttributeNames

data QueryResult a = QueryResult
    { items :: Array a
    , count :: Int
    , scannedCount :: Int
    , lastEvaluatedKey :: a
    }

instance queryResultIsForeign :: (IsForeign a) => IsForeign (QueryResult a) where
    read value = do
        items <- readProp "Items" value
        count <- readProp "Count" value
        scannedCount <- readProp "ScannedCount" value
        lastEvaluatedKey <- readProp "LastEvaluatedKey" value
        pure $ QueryResult
            { items
            , count
            , scannedCount
            , lastEvaluatedKey
            }

query :: forall eff a hk sk. (IsForeign a, AsForeign hk, AsForeign sk) => QueryParams a hk sk -> Dynaff eff (QueryResult a)
query = invoke "query" <<< QueryParams'



