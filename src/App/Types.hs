module App.Types (

    BotColour(..),
    toColour,
    fromColour,

    Params,
    thisAddress,
    thisPort,
    thisBots,
    thisLogLevel,

    BotParams,
    botName,
    botId,
    botAddress,
    botPort,
    botColour,
    botNotify,

    GlobalState(GlobalState),
    botParams,
    oauthSenders

) where

import qualified Data.ByteString.Lazy as BL
import           Data.Default         (Default(..))
import           Control.Lens
import           Control.Concurrent   (MVar)
import           Data.Text            (Text)
import           Data.Aeson           (ToJSON(..), Value(..))
import           Data.String          (IsString)

--
-- Message colour
--
data BotColour = Yellow | Green | Red | Purple | Grey | Random deriving Show

toColour :: Text -> BotColour
toColour "yellow" = Yellow
toColour "green" = Green
toColour "red" = Red
toColour "purple" = Purple
toColour "grey" = Grey
toColour "random" = Random
toColour _ = Yellow 

fromColour :: BotColour -> Text
fromColour Yellow = "yellow"
fromColour Green = "green"
fromColour Red = "red"
fromColour Purple = "purple"
fromColour Grey = "grey"
fromColour Random = "random"

instance ToJSON BotColour where
    toJSON c = String $ fromColour c

--
-- Params for one bot
--
data BotParams = BotParams {
    _botName :: Text,
    _botColour :: BotColour,
    _botNotify :: Bool,
    _botId :: Text,
    _botAddress :: Text,
    _botPort :: Int
} deriving (Show)
makeLenses ''BotParams

instance Default BotParams where
    def = BotParams {
        _botName = "",
        _botColour = Yellow,
        _botNotify = False,
        _botId = "",
        _botAddress = "",
        _botPort = 0
    }

--
-- params for all bots plus general config
--
data Params = Params {
    _thisAddress :: Text,
    _thisPort :: Int,
    _thisBots :: [BotParams],
    _thisLogLevel :: Int
} deriving (Show)
makeLenses ''Params

instance Default Params where
    def = Params {
        _thisAddress = "",
        _thisPort = 0,
        _thisBots = def,
        _thisLogLevel = 0
    }

--
-- global application state
--
data GlobalState = GlobalState {
    _botParams :: Params,
    _oauthSenders :: MVar [(Text,BL.ByteString -> IO ())]
}
makeLenses ''GlobalState


