module Main where

import Prelude
import Optimizely
import Data.DateTime.Foreign as ForeignDate
import Data.Map as Map
import AWS.Dynamo (DYNAMO, Order(..), QueryResult(..), Table, defaultQuery, put, query, tableSpec)
import AWS.Dynamo.Classes (class DynamoAttribute, class DynamoDecode, class FromDynamo, readDynamoProp, dynamoRead, dynamoReadGeneric, dynamoType)
import AWS.Lambda (LAMBDA, Context, succeed, fail)
import Control.Monad.Aff (Aff, Canceler(..), launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now, nowDateTime)
import Control.Monad.Except (runExcept, runExceptT, withExcept, ExceptT)
import Data.Array (filter)
import Data.DateTime.Locale (LocalValue(..))
import Data.Either (Either(..))
import Data.Foldable (foldMapDefaultL)
import Data.Foreign (F, Foreign, MultipleErrors, readString, renderForeignError, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, readJSON, readProp, write, writeProp)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Keys (keys)
import Data.Foreign.Null (Null(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (toUnfoldable)
import Data.Map (fromFoldable, singleton, empty)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Optimizely (Experiment(..), Project(..), Variation(..))
import Data.Optimizely.Common (Id(..))
import Data.Optimizely.Experiment (ExperimentStatus(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unit (Unit)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.Affjax.Request (class Requestable, RequestContent)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.Optimizely (OptimizelyError, listExperiments, listProjects, listVariations, H)
import Network.Optimizely.Auth (Auth(..))
import Node.Process (PROCESS, lookupEnv, exit)
import Unsafe.Coerce (unsafeCoerce)
-- import Types (InputType(..))

-- handler :: forall eff. Context -> Foreign -> Eff (lambda :: LAMBDA | eff) Unit
-- handler c d = do
--   process $ readData d
--   pure unit

--   where
--     readData :: Foreign -> Either String InputType
--     readData = foreignParseToEither <<< read

--     process :: Either String InputType -> Eff (lambda :: LAMBDA | eff) Unit
--     process (Left err) = fail c err
--     process (Right (InputType i))  = succeed c $ i.key



webhook = ""

newtype SlackMsg = SlackMsg String
derive newtype instance showSlackMsg :: Show SlackMsg
derive newtype instance eqSlackMsg :: Eq SlackMsg
derive instance newtypeSlackMsg :: Newtype SlackMsg _

instance asForeignSlackMsg :: AsForeign SlackMsg where
    write (SlackMsg s) = writeObject [writeProp "text" s]

foreignToRequest :: forall a. AsForeign a => a -> Tuple (Maybe MediaType) RequestContent
foreignToRequest x = Tuple (Just applicationJSON) $ unsafeCoerce $ unsafeStringify $ write x

instance requestableSlackMsg :: Requestable SlackMsg where
    toRequest = foreignToRequest

notify :: forall eff. String -> Aff (ajax :: AJAX | eff) (AffjaxResponse Unit)
notify msg = post webhook (SlackMsg msg)

newtype A = A
   { id :: String
   , retrievetime :: Int
   }
derive instance aGeneric :: Generic A _
instance aShow :: Show A where
    show a = genericShow a


aSchema :: Table A String Int
aSchema = tableSpec
    { name: "Test"
    , hashkey: "id"
    , sortkey: "retrievetime"
    }

opts = defaultOptions{unwrapSingleConstructors=true}

instance aIsForeign :: IsForeign A where
    read = readGeneric opts

instance aAsForeign :: AsForeign A where
    write = toForeignGeneric opts

newtype ExperimentId = ExperimentId (Id Experiment)
derive newtype instance ordExperimentId :: Ord ExperimentId
derive newtype instance showExperimentId :: Show ExperimentId
derive newtype instance isForeignExperimentId :: IsForeign ExperimentId
derive newtype instance asForeignExperimentId :: AsForeign ExperimentId
derive instance newtypeExperimentId :: Newtype ExperimentId _


newtype VariationId = VariationId (Id Variation)
derive newtype instance ordVariationId :: Ord VariationId
derive newtype instance showVariationId :: Show VariationId
derive newtype instance isForeignVariationId :: IsForeign VariationId
derive newtype instance asForeignVariationId :: AsForeign VariationId
derive instance newtypeVariationId :: Newtype VariationId _


newtype DateTime = DateTime ForeignDate.DateTime
derive newtype instance showDateTime :: Show DateTime
derive newtype instance isForeignDateTime :: IsForeign DateTime
derive newtype instance asForeignDateTime :: AsForeign DateTime
derive instance newtypeDateTime :: Newtype DateTime _

instance fromDynamoDateTime :: FromDynamo DateTime where
    dynamoRead = read

instance dynamoAttrDateTime :: DynamoAttribute DateTime where
    dynamoType _ = "S"


newtype Variations = Variations
    { keyname :: String
    , retrievetime :: DateTime
    , variationWeights :: Map ExperimentId (Map VariationId Int)
    }
derive instance genericRecord :: Generic Variations _

instance showRecord :: Show Variations where
    show = genericShow

variationsSchema :: Table Variations String DateTime
variationsSchema = tableSpec
    { name: "Variations"
    , hashkey: "keyname"
    , sortkey: "retrievetime"
    }

instance isForeignVariations :: IsForeign Variations where
    read = readGeneric opts

instance fromDynamoVariations :: FromDynamo Variations where
    dynamoRead = dynamoReadGeneric

instance variationsAsForeign :: AsForeign Variations where
    write = toForeignGeneric opts

newtype Map k v = Map (Map.Map k v)
derive instance genericMap :: Generic (Map k v) _
derive instance newtypeMap :: Newtype (Map k v) _
derive newtype instance showMap :: (Show k, Show v) => Show (Map k v)
derive newtype instance monoidMap :: Ord k => Monoid (Map k v)

readMap :: forall k v. (Ord k, IsForeign k) => (Foreign -> F v) -> Foreign -> F (Map k v)
readMap readValue obj = do
    ks <- keys obj
    map (wrap <<< fromFoldable) $ traverse readKeyValue ks
    where
        readKeyValue i  = do
            value <- readValue =<< readProp i obj
            key <- readJSON i
            pure $ Tuple key value

instance isForeignMap :: (IsForeign k, IsForeign v, Ord k) => IsForeign (Map k v) where
    read = readMap read

instance fromDynamoMap :: (IsForeign k, Ord k, DynamoAttribute v) => FromDynamo (Map k v) where
    dynamoRead = readMap readDynamoProp

instance asForeignMap :: (AsForeign k, AsForeign v) => AsForeign (Map k v) where
    write = writeObject <<< map (uncurry writeProp <<< writeKey) <<< Map.toUnfoldable <<< unwrap
        where
            writeKey (Tuple k v) = Tuple (unsafeStringify $ write k) v

instance dynamoAttrMap :: (IsForeign k, Ord k, DynamoAttribute v) => DynamoAttribute (Map k v) where
    dynamoType _ = "M"



isRunning :: Experiment -> Boolean
isRunning (Experiment {status: Running}) = true
isRunning _ = false

currentDateTime :: forall m eff. (MonadEff (now :: NOW | eff) m) => m DateTime
currentDateTime = do
  (LocalValue _ current) <- liftEff nowDateTime
  pure $ DateTime $ ForeignDate.DateTime $ current

variationKeyValue :: Variation -> Map VariationId Int
variationKeyValue (Variation {id, weight: Null (Just w)}) = wrap $ singleton (VariationId id) w
variationKeyValue _ = wrap empty

experimentKeyValue :: forall eff. Auth -> Experiment -> H eff (Tuple ExperimentId (Map VariationId Int))
experimentKeyValue tkn (Experiment {id}) = do
    variations <- listVariations id tkn
    pure $ Tuple (ExperimentId id) $ foldMapDefaultL variationKeyValue variations

retrieveVariations :: forall eff. Auth -> H eff (Map ExperimentId (Map VariationId Int))
retrieveVariations tkn = do
    projects <- listProjects tkn
    experiments <- map join $ traverse (\(Project {id}) -> listExperiments id tkn) projects
    let activeExperiments = filter isRunning experiments
    map (wrap <<< fromFoldable) $ traverse (experimentKeyValue tkn) activeExperiments


type Dynaff eff r = Aff (dynamo :: DYNAMO | eff) r

f :: forall eff. (Map ExperimentId (Map VariationId Int)) -> Dynaff (console :: CONSOLE, now :: NOW | eff) Unit
f variationWeights = do
    retrievetime <- currentDateTime

    put {tablename: variationsSchema.name, item: Variations {keyname: "variations", retrievetime, variationWeights}}
    (QueryResult rez) <- query (defaultQuery variationsSchema "variations")
    liftAff $ logShow $ rez.items
    -- (QueryResult rez) <- query (defaultQuery aSchema "foo"){limit=Just 1, order=Descending}
    -- liftAff $ logShow $ rez.items

exitWith :: forall eff a. String -> Aff (process :: PROCESS, console :: CONSOLE |eff) a
exitWith s = log s *> (liftEff $ exit 1)

exitShowing :: forall eff a b. (Show a) => a -> Aff (process :: PROCESS, console :: CONSOLE |eff) b
exitShowing = exitWith <<< show


main :: Eff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS) Unit
main = do
    maybeKey <- lookupEnv "OPTIMIZELY_KEY"
    void $ launchAff $ case maybeKey of
        Nothing -> exitWith "No OPTIMIZELY_KEY provided"
        Just key -> do
            variationWeights <- runExceptT $ retrieveVariations $ ClassicToken key
            case variationWeights of
                Left err -> exitShowing err
                Right weights -> f weights


-- main :: forall eff. Eff ( "err" :: EXCEPTION, "ajax" :: AJAX, "console" :: CONSOLE | eff) Unit
-- main = void $ launchAff $ do
--   log "here"
--   projects <- getProjects
--   logShow $ runExcept projects


