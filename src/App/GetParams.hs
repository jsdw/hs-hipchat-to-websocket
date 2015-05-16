module App.GetParams (

    -- types
    Params,
    BotParams,

    -- param lenses
    thisAddress,
    thisPort,
    thisBots,
    thisLogLevel,

    botName,
    botId,
    botAddress,
    botPort,

    --get Params
    getParams

) where

import           Control.Lens
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8, decodeUtf8)
import           Text.Read            (readMaybe)
import qualified Data.ByteString      as B
import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Except as E
import           Data.Default         (Default, def)
import           Data.Monoid          ((<>))
import qualified Data.Map             as M

import           App.ParseConf        (parseConf)

--
-- Params
--
data BotParams = BotParams {
    _botName :: T.Text,
    _botId :: T.Text,
    _botAddress :: T.Text,
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

data Params = Params {
    _thisAddress :: T.Text,
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
-- extract params from bots.conf into structures declared above.
-- return Left err if error, else Right Params
--
getParams :: IO (Either String Params)
getParams = E.runExceptT $ do

    --read bots.conf as UTF8
    conf <- fmap decodeUtf8 $ E.liftIO $ B.readFile "bots.conf"

    --attempt to parse, err if fail
    (topParams, subParamList) <- case parseConf conf of
        Left err -> E.throwError err
        Right p -> return p

    --extract top level params
    tAddress <- getParam "address" topParams "'address' param not found"

    _tPort <- getParam "port" topParams "'port' param not found"
    tPort <- toInt _tPort "global port not an int"

    _tLogLevel <- getParam "loglevel" topParams "'port' param not found"
    tLogLevel <- toInt _tLogLevel "loglevel not an int"

    --extract params for each bot
    bots <- forM subParamList $ \(bId,params) -> do

        bName <- getParam "name" params ("name for bot " <> T.unpack bId <> " not found")
        bAddress <- getParam "address" params ("address for bot " <> T.unpack bId <> " not found")

        _bPort <- getParam "port" params ("port for bot " <> T.unpack bId <> " not found")
        bPort <- toInt _bPort ("port for bot " <> T.unpack bId <> " not a number")

        return $ def & botId .~ bId
                     & botName .~ bName
                     & botAddress .~ bAddress
                     & botPort .~ bPort

    return $ def & thisAddress .~ tAddress
                 & thisPort .~ tPort
                 & thisLogLevel .~ tLogLevel
                 & thisBots .~ bots

getParam str map err = case M.lookup str map of
    Just val -> return val
    Nothing -> E.throwError err

getParamDef str map def = case M.lookup str map of
    Just val -> return val
    Nothing -> return def 

toInt val err = case readMaybe (T.unpack val) of
    Just v -> return v
    Nothing -> E.throwError err







