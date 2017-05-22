module AWS.Alexa where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.Function.Uncurried (Fn0)

foreign import data Alexa :: *
foreign import data ALEXA :: !

type OurAppHandler = {
    launchRequest :: Fn0 Unit
    helloWorldIntent :: Fn0 Unit
}


x :: OurAppHandler
x = {
      launchRequest: convert (\alexa  -> do
                                log "fsjfhj"
                                tell alexa "hi" )
    , helloWorldIntent: convert (\alexa -> do
                                    pure unit
                                    )

}



type AlexaHandler eff = Eff (alexa :: ALEXA | eff) Unit

foreign import tell :: forall eff. Alexa -> String -> AlexaHandler eff



foreign import convert :: forall eff. (Alexa -> AlexaHandler eff) -> Fn0 Unit
