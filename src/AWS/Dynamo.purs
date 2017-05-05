module AWS.Dynamo where

import Prelude
import Common (renderErrors)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept, withExcept)
import Data.Either (Either, either)
import Data.Foreign (Foreign, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write, writeProp)
import Data.Function.Uncurried (Fn4, runFn4)

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

type PutParams a =
    { tablename :: String
    , item :: a
    }
newtype PutParams' a = PutParams' (PutParams a)

instance putParamsAsForeign :: (AsForeign a) => AsForeign (PutParams' a) where
    write (PutParams' {tablename, item}) = writeObject [ writeProp "TableName" tablename
                                                       , writeProp "Item" item]

put :: forall eff b. (AsForeign b) => PutParams b -> Dynaff eff Foreign
put = invoke "put" <<< PutParams'





