module Main where

import Prelude
import AWS.Dynamo (DYNAMO, Order(..), QueryResult(..), Dynaff, defaultQuery, put, query)
import AWS.Lambda (LAMBDA, Context, succeed, fail)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Network.Optimizely.Auth (Auth(..))
import Node.Process (PROCESS, lookupEnv)

import Main.Internal (Map, exitWith, exitShowing, ExperimentId, VariationId, currentDateTime)
import Main.Optimizely (retrieveVariations)
import Main.Common (variationsSchema, Variations(..))
import Main.Slack (notify)
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


f :: forall eff. (Map ExperimentId (Map VariationId Int)) -> Dynaff (console :: CONSOLE, now :: NOW | eff) Unit
f variationWeights = do
    retrievetime <- currentDateTime

    put {tablename: variationsSchema.name, item: Variations {keyname: "variations", retrievetime, variationWeights}}
    (QueryResult rez) <- query (defaultQuery variationsSchema "variations")
    liftAff $ logShow $ rez.items
    -- (QueryResult rez) <- query (defaultQuery aSchema "foo"){limit=Just 1, order=Descending}
    -- liftAff $ logShow $ rez.items


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


