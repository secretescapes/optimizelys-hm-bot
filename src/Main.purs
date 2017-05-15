module Main where

import Prelude
import Data.Map as Map
import AWS.Dynamo (DYNAMO, Dynaff, DynamoDBRecord(..), Order(..), QueryResult(..), StreamPayload(..), StreamEvent(..), defaultQuery, put, query)
import AWS.Lambda (LAMBDA, Context, succeed, fail)
import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (maximumBy)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Undefined (Undefined(..))
import Data.List (List, elemIndex, filter, intercalate, intersect, length, take, concatMap, catMaybes)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Optimizely.Experiment (emptyVariation)
import Data.Traversable (for)
import Data.Tuple (fst, snd, Tuple(..))
import Main.Common (variationsSchema, Variations(..), foreignParseToEither)
import Main.Internal (ExperimentId, Map(..), VariationId(..), currentDateTime, exitShowing, exitWith, retryUntil)
import Main.Optimizely (retrieveVariations)
import Main.Slack (notify)
import Network.HTTP.Affjax (AJAX)
import Network.Optimizely (updateVariation)
import Network.Optimizely.Auth (Auth(..))
import Node.Process (PROCESS, lookupEnv)

type LambdaEff eff = Eff (lambda :: LAMBDA | eff)

mkHandler :: forall a eff. (IsForeign a) => (a -> LambdaEff eff Unit) -> Context -> Foreign -> LambdaEff eff Unit
mkHandler fn context event = process $ foreignParseToEither $ read event
    where
        process :: Either String a -> Eff (lambda :: LAMBDA | eff) Unit
        process (Left err) = fail context err
        process (Right a)  = do
            fn a
            succeed context "OK"


storeWeights :: forall eff. (Map ExperimentId (Map VariationId Int)) -> Dynaff (now :: NOW | eff) Unit
storeWeights variationWeights = do
    retrievetime <- currentDateTime
    put {tablename: variationsSchema.name, item: Variations {keyname: "variations", retrievetime, variationWeights}}



getValid :: Int -> Variations -> Maybe Variations
getValid max v@(Variations {variationWeights: Map weights}) =
    if amountOfPathologicalTests <= max
        then Just v
        else Nothing
    where
        amountOfPathologicalTests = length $ filter isPathological $ Map.values weights

isPathological :: Map VariationId Int -> Boolean
isPathological (Map m)
    = case elemIndex 10000 $ Map.values m of
        Nothing -> false
        Just _ -> true

lastVariationsDumps :: forall eff. Int -> Dynaff eff (Array Variations)
lastVariationsDumps limit = do
    (QueryResult {items}) <- query (defaultQuery variationsSchema "variations"){limit=Just limit, order=Descending}
    pure items

max100s :: Int
max100s = 20

retrieveLastGoodStatus :: forall eff. Dynaff eff (Maybe Variations)
retrieveLastGoodStatus = retryUntil (getValid max100s) lastVariationsDumps


retrieveWeights' :: forall eff. Foreign -> Eff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
retrieveWeights' _ = withOptiKey $ \tkn -> do
            variationWeights <- runExceptT $ retrieveVariations tkn
            case variationWeights of
                Left err -> exitShowing err
                Right weights -> storeWeights weights

retrieveWeights :: forall eff. Context -> Foreign -> LambdaEff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
retrieveWeights = mkHandler retrieveWeights'

withOptiKey :: forall eff. (Auth -> Aff (console :: CONSOLE, process :: PROCESS  | eff) Unit)
                        -> Eff (console :: CONSOLE, err :: EXCEPTION, process :: PROCESS | eff) Unit
withOptiKey action = do
    maybeKey <- lookupEnv "OPTIMIZELY_KEY"
    void $ launchAff $ case maybeKey of
        Nothing -> exitWith "No OPTIMIZELY_KEY provided"
        Just key -> action $ ClassicToken key

adjustABTests' :: forall eff. StreamPayload Variations -> Eff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
adjustABTests' (StreamPayload events) = withOptiKey \tkn -> void $ for pathologicalVariations (flipBackWeights tkn)
    where
        records = map (\(StreamEvent {dynamodb:(DynamoDBRecord rec)}) -> rec) events
        latestImage = map _.newImage $ maximumBy (\{sequenceNumber: n1} {sequenceNumber: n2} -> compare n1 n2) records
        pathologicalVariations = maybe Nothing (\x -> flipMaybe x $ getValid max100s x) latestImage -- if no events, everything is fine, but this shouldn't happen

adjustABTests :: forall eff. Context -> Foreign -> LambdaEff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
adjustABTests = mkHandler adjustABTests'

flipMaybe :: forall a b. a -> Maybe b -> Maybe a
flipMaybe _ (Just _) = Nothing
flipMaybe x Nothing = Just x

flipBackWeights :: forall eff. Auth -> Variations -> Aff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
flipBackWeights tkn (Variations {retrievetime, variationWeights: newW}) = do
    lastGood <- retrieveLastGoodStatus
    case lastGood of
        Nothing -> log "Unable to retrieve a last-good weight status snapshot"
        Just (Variations {variationWeights: oldW}) -> updateExperiments tkn oldW $ selectNewExperiments max100s oldW newW

selectNewExperiments :: Int -> Map ExperimentId (Map VariationId Int) -> Map ExperimentId (Map VariationId Int) -> List ExperimentId
selectNewExperiments max oldWeights newWeights = take toBeFlipped canBeChanged
    where
        oldAndCorrect = map fst $ filter (not <<< isPathological <<< snd) $ Map.toList $ unwrap oldWeights
        newButWrong = map fst $ filter (isPathological <<< snd) $ Map.toList $ unwrap newWeights
        toBeFlipped = length newButWrong - max
        -- TODO check for toBeFlipped > length canBeChanged
        canBeChanged = intersect oldAndCorrect newButWrong

updateExperiments :: forall eff. Auth -> Map ExperimentId (Map VariationId Int) -> List ExperimentId -> Aff (console :: CONSOLE, now :: NOW, dynamo :: DYNAMO, ajax :: AJAX, err :: EXCEPTION, process :: PROCESS | eff) Unit
updateExperiments tkn (Map weights) experiments = do
    let foundMsg = (show $ length experiments) <> " tests over the limit of " <> show max100s <> " tests allowed to run at 100%"
        details =  (intercalate "," $ map show experiments)
    log $ "Found " <> foundMsg <> ". About to flip back: " <> details
    let variations = concatMap (Map.toList <<< unwrap) $ catMaybes $ map (flip lookup weights) experiments
    result <- runExceptT $ for variations \(Tuple (VariationId vid) weight) ->
                updateVariation vid emptyVariation{weight=Undefined (Just weight)} tkn
    case result of
        Left err -> logShow err -- TODO collect the errors, and report successes
        Right _ -> void $ notify $ "I found " <> foundMsg <> ". I flipped back: " <> details


