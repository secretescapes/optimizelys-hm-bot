module Test.Main where

import Prelude
import AWS.Dynamo.Classes (dynamoRead, readDynamoProp)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (Except, runExcept, withExcept)
import Data.Either (Either, either)
import Data.Foreign (F)
import Data.Foreign.Class (readJSON)
import Main.Internal (ExperimentId, Map, VariationId)
import Main.Common (Variations)
import Test.Unit (test, Test, failure, success)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


type TestEffs = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR)

payload :: String
payload = """
{
  "M": {
    "1": {
      "M": {
        "2": {
          "N": "3"
        }
      }
    }
  }
}
"""


payload2 :: String
payload2 = """
{
  "keyname": {
    "S": "variations"
  },
  "retrievetime": {
    "S": "2017-05-14T15:52:06.019Z"
  },
  "variationWeights": {
    "M": {
      "1": {
        "M": {
          "2": {
            "N": "3"
          }
        }
      }
    }
  }
}
"""

main :: Eff TestEffs Unit
main = do
  runTest $ do
    test "Variations map can be deserialized" $ do
      assertExcept ((readDynamoProp =<< readJSON payload) :: F (Map ExperimentId (Map VariationId Int)))
    test "Variations Stream Event Newimage can be deserialized" $ do
      assertExcept ((dynamoRead =<< readJSON payload2) :: F Variations)


assertExcept :: forall e a eff. (Show e) => Except e a -> Test eff
assertExcept result = assertEither $ runExcept $ withExcept show result

assertEither :: forall a eff. Either String a -> Test eff
assertEither = either failure (const success)
