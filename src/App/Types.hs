module App.Types (

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

    GlobalState(GlobalState),
    botParams,
    oauthSenders

) where

import qualified Data.ByteString.Lazy as BL
import           Data.Default         (Default(..))
import           Control.Lens
import           Control.Concurrent   (MVar)
import           Data.Text            (Text)

--
-- Params for one bot
--
data BotParams = BotParams {
    _botName :: Text,
    _botId :: Text,
    _botAddress :: Text,
    _botPort :: Int
} deriving (Show)
makeLenses ''BotParams

instance Default BotParams where
    def = BotParams {
        _botName = "",
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


