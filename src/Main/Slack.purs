module Main.Slack where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Foreign.Class (class AsForeign, write, writeProp)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Foreign (writeObject)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.Affjax.Request (class Requestable, RequestContent)
import Unsafe.Coerce (unsafeCoerce)


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
