module App.GetParams (

    -- types
    Params,
    BotParams,
    ParamFailure,

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
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Catch  (Exception, MonadThrow, throwM, toException)
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
-- A little quick error type to represent param extraction failure.
-- playing with MonadThrow to give the caller a choice in what shape
-- failure takes
--
data ParamFailure = ParamFailure String
instance Exception ParamFailure
instance Show ParamFailure where show (ParamFailure s) = "Param extraction failure: " <> s
paramError str = throwM $ toException $ ParamFailure str

--
-- extract params from bots.conf into structures declared above.
-- return Left err if error, else Right Params
--

getParams :: MonadThrow m => IO (m Params)
getParams = do

    --read bots.conf as UTF8
    conf <- fmap decodeUtf8 $ liftIO $ B.readFile "bots.conf" 
    return $ getParams' conf


getParams' :: MonadThrow m => T.Text -> m Params
getParams' conf = do

    --attempt to parse, err if fail
    (topParams, subParamList) <- case parseConf conf of
        Left err -> paramError err
        Right p -> return p

    --extract top level params
    tAddress <- getParam "address" topParams "'address' param not found"

    _tPort <- getParam "port" topParams "'port' param not found"
    tPort <- toInt _tPort "global port not an int"

    _tLogLevel <- getParamDef "loglevel" topParams "10"
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
    Nothing -> paramError err

getParamDef str map def = case M.lookup str map of
    Just val -> return val
    Nothing -> return def 

toInt val err = case readMaybe (T.unpack val) of
    Just v -> return v
    Nothing -> paramError err







