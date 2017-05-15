module Main.Optimizely where

import Data.Array (filter)
import Data.Foldable (foldMapDefaultL)
import Data.Foreign.Null (Null(..))
import Data.Map (singleton, empty, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Optimizely (Experiment(..), Project(..), Variation(..))
import Data.Optimizely.Experiment (ExperimentStatus(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Main.Internal (ExperimentId(..), VariationId(..), Map)
import Network.Optimizely (H, listExperiments, listProjects, listVariations)
import Network.Optimizely.Auth (Auth)
import Prelude (bind, pure, ($), map, join, (<<<))


isRunning :: Experiment -> Boolean
isRunning (Experiment {status: Running}) = true
isRunning _ = false


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

