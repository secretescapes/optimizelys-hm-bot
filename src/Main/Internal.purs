module Main.Internal where

import Data.DateTime.Foreign as ForeignDate
import Data.List.Lazy as L
import Data.Map as Map
import AWS.Dynamo.Classes (class DynamoAttribute, class FromDynamo, readDynamoProp)
import Control.Alternative (class Applicative)
import Control.Extend (class Functor)
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Data.Array (catMaybes, head)
import Data.DateTime.Locale (LocalValue(..))
import Data.Foreign (F, Foreign, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, readJSON, readProp, write, writeProp)
import Data.Foreign.Generic (defaultOptions)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.List.Lazy.Types (Step(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Optimizely (Experiment, Id, Variation)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Global.Unsafe (unsafeStringify)
import Node.Process (exit, PROCESS)
import Prelude (class Ord, class Show, class Eq, (=<<), (*>), (<<<), show, ($), map, bind, pure, (<$>), (*), (-))

exitWith :: forall eff a. String -> Aff (process :: PROCESS, console :: CONSOLE |eff) a
exitWith s = log s *> (liftEff $ exit 1)

exitShowing :: forall eff a b. (Show a) => a -> Aff (process :: PROCESS, console :: CONSOLE |eff) b
exitShowing = exitWith <<< show


currentDateTime :: forall m eff. (MonadEff (now :: NOW | eff) m) => m DateTime
currentDateTime = do
  (LocalValue _ current) <- liftEff nowDateTime
  pure $ DateTime $ ForeignDate.DateTime $ current

opts :: Options
opts = defaultOptions{unwrapSingleConstructors=true}

newtype ExperimentId = ExperimentId (Id Experiment)
derive newtype instance eqExperimentId :: Eq ExperimentId
derive newtype instance ordExperimentId :: Ord ExperimentId
derive newtype instance showExperimentId :: Show ExperimentId
derive newtype instance isForeignExperimentId :: IsForeign ExperimentId
derive newtype instance asForeignExperimentId :: AsForeign ExperimentId
derive instance newtypeExperimentId :: Newtype ExperimentId _


newtype VariationId = VariationId (Id Variation)
derive newtype instance eqVariationId :: Eq VariationId
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


twoPows :: L.List Int
twoPows = L.iterate (_ * 2) 1

retryIncreasingly' :: forall a m. (Functor m, Applicative m) => Int -> (a -> Maybe a) -> (Int -> m a) -> m (Maybe a)
retryIncreasingly' maxattempts selector f = pickIfAny maxattempts selector <$> traverse f twoPows


pickIfAny :: forall a. Int -> (a -> Maybe a) -> L.List a -> Maybe a
pickIfAny maxattempts selector = L.head <<< L.catMaybes <<< map selector <<< L.take maxattempts

retryIncreasingly :: forall a m. (Monad m) => Int -> Step Int -> (a -> Maybe a) -> (Int -> m (Array a)) -> m (Maybe a)
retryIncreasingly 0 _ _ _ = pure Nothing
retryIncreasingly _ Nil _ _ = pure Nothing
retryIncreasingly n (Cons x xs) selector f  = do
    value <- f x
    case head $ catMaybes $ map selector value of
        Nothing -> retryIncreasingly (n-1) (L.step xs) selector f
        r@(Just _) -> pure r

--retryIncreasingly 8 (step twoPows) (\x -> if x > 33 then Just x else Nothing) (\x -> logShow x *> pure x)

retryUntil :: forall a m. Monad m => (a -> Maybe a) -> (Int -> m (Array a)) -> m (Maybe a)
retryUntil = retryIncreasingly 8 (L.step twoPows)
