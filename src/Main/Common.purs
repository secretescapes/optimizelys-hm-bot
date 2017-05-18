module Main.Common where

import AWS.Dynamo (tableSpec)
import AWS.Dynamo.Classes (class FromDynamo, dynamoReadGeneric)
import AWS.Dynamo.Internal (Table)
import Control.Category ((<<<))
import Control.Monad.Except (runExcept, withExcept)
import Data.Either (Either)
import Data.Foreign (F, MultipleErrors, renderForeignError)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Foreign.Generic (readGeneric, toForeignGeneric)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (toUnfoldable)
import Data.String (joinWith)
import Prelude (map, class Show)

import Main.Internal (DateTime, ExperimentId, VariationId, Map, opts)

foreignParseToEither :: forall a. F a -> Either String a
foreignParseToEither = runExcept <<< (withExcept renderErrors)

renderErrors :: MultipleErrors -> String
renderErrors = joinWith "\n" <<< toUnfoldable <<< map renderForeignError



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
    { name: "experiments-prod" -- TODO, wire it an env variable
    , hashkey: "keyname"
    , sortkey: "retrievetime"
    }

instance isForeignVariations :: IsForeign Variations where
    read = readGeneric opts

instance fromDynamoVariations :: FromDynamo Variations where
    dynamoRead = dynamoReadGeneric

instance variationsAsForeign :: AsForeign Variations where
    write = toForeignGeneric opts

