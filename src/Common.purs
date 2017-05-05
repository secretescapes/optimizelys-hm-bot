module Common where

import Prelude (map)
import Control.Category ((<<<))
import Control.Monad.Except (runExcept, withExcept)
import Data.List.NonEmpty (toUnfoldable)
import Data.Either (Either)
import Data.Foreign (F, MultipleErrors, renderForeignError)
import Data.String (joinWith)


foreignParseToEither :: forall a. F a -> Either String a
foreignParseToEither = runExcept <<< (withExcept renderErrors)

renderErrors :: MultipleErrors -> String
renderErrors = joinWith "\n" <<< toUnfoldable <<< map renderForeignError
