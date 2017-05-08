module Main where

import Prelude
import Optimizely
import AWS.Dynamo (put, Table, tableSpec, query, defaultQuery)
import AWS.Lambda (LAMBDA, Context, succeed, fail)
import Control.Monad.Aff (Aff, Canceler(..), launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept, withExcept)
import Data.Foreign (F, Foreign, MultipleErrors, renderForeignError, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write, writeProp)
import Data.Generic.Rep (class Generic)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.Affjax.Request (class Requestable, RequestContent)
import Network.HTTP.Affjax.Response (class Respondable)
import Unsafe.Coerce (unsafeCoerce)
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

newtype A = A
   { id :: String
   , retrievetime :: Int
   }
derive instance aGeneric :: Generic A _

aSchema :: Table A String Int
aSchema = tableSpec
    { name: "Test"
    , hashkey: "id"
    , sortkey: "retrievetime"
    }

opts = defaultOptions{unwrapSingleConstructors=true}

instance aIsForeign :: IsForeign A where
    read = readGeneric opts

instance aAsForeign :: AsForeign A where
    write = toForeignGeneric opts

main = launchAff $ do
    liftAff $ log "and now?"
    put {tablename: aSchema.name, item: A {id: "foo", retrievetime: 5}}
    liftAff $ log "everything fine"
    rez <- query (defaultQuery aSchema "foo")
    rez2 <- query (defaultQuery aSchema "foo")
    liftAff $ log $ unsafeStringify rez


-- main :: forall eff. Eff ( "err" :: EXCEPTION, "ajax" :: AJAX, "console" :: CONSOLE | eff) Unit
-- main = void $ launchAff $ do
--   log "here"
--   projects <- getProjects
--   logShow $ runExcept projects


