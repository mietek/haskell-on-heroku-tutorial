import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Hourglass
import Data.Proxy
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.Hourglass
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.PostgreSQL.Simple as DB


getISO8601DateTime :: IO Text
getISO8601DateTime = do
    seconds <- timeCurrent
    let iso = timePrint ISO8601_DateAndTime seconds
    return $ T.pack iso


data Note = Note
    { contents :: Text
    , dateTime :: Text
    }
  deriving (Generic, Show)

instance ToJSON Note

instance FromRow Note where
    fromRow = Note <$> field <*> field

instance ToRow Note where
    toRow note =
        [ toField $ contents note
        , toField $ dateTime note
        ]


newtype PostNote = PostNote
    { postContents :: Text
    }
  deriving Show

instance FromJSON PostNote where
    parseJSON (Object o) = PostNote <$> o .: "contents"
    parseJSON _          = mzero


getNotes :: MonadIO m => DB.Connection -> m [Note]
getNotes db =
    liftIO $ query_ db "SELECT * FROM notes"

postNote :: MonadIO m => DB.Connection -> PostNote -> m [Note]
postNote db post =
    liftIO $ do
      iso <- getISO8601DateTime
      T.putStrLn $ T.concat [iso, " ", postContents post]
      let note = Note
            { contents = postContents post
            , dateTime = iso
            }
      _ <- execute db "INSERT INTO notes VALUES (?, ?)" note
      query_ db "SELECT * FROM notes"


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody PostNote :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

server :: Text -> DB.Connection -> Server NoteAPI
server home db =
         return home
    :<|> getNotes db
    :<|> postNote db


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    url <- BS.pack <$> getEnv "DATABASE_URL"
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    db <- connectPostgreSQL url
    run port $ serve noteAPI $ server home db
