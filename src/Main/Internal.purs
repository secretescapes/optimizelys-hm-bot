module Main.Internal where

import Data.DateTime.Foreign as ForeignDate
import Data.Map as Map
import AWS.Dynamo.Classes (class DynamoAttribute, class FromDynamo, readDynamoProp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Data.DateTime.Locale (LocalValue(..))
import Data.Foreign (F, Foreign, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, readJSON, readProp, write, writeProp)
import Data.Foreign.Generic (defaultOptions)
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Optimizely (Experiment, Id, Variation)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Global.Unsafe (unsafeStringify)
import Node.Process (exit, PROCESS)
import Prelude (class Ord, class Show, (=<<), (*>), (<<<), show, ($), map, bind, pure)

exitWith :: forall eff a. String -> Aff (process :: PROCESS, console :: CONSOLE |eff) a
exitWith s = log s *> (liftEff $ exit 1)

exitShowing :: forall eff a b. (Show a) => a -> Aff (process :: PROCESS, console :: CONSOLE |eff) b
exitShowing = exitWith <<< show


currentDateTime :: forall m eff. (MonadEff (now :: NOW | eff) m) => m DateTime
currentDateTime = do
  (LocalValue _ current) <- liftEff nowDateTime
  pure $ DateTime $ ForeignDate.DateTime $ current

opts = defaultOptions{unwrapSingleConstructors=true}

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

