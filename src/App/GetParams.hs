module App.GetParams (

    -- exception on failure
    ParamFailure,

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
import           System.Environment   (getArgs)

import           App.ParseConf        (parseConf)
import           App.Types

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

    --where is config file?
    args <- getArgs
    let arg = case args of
            [] -> "bots.conf"
            (s:ss) -> s

    --read config file as UTF8 and extract params from it
    conf <- fmap decodeUtf8 $ liftIO $ B.readFile arg 
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

        _bColour <- getParamDef "colour" params "yellow"
        let bColour = toColour _bColour 

        _bNotify <- getParamDef "notify" params "no"
        let bNotify = case T.toLower _bNotify of
                "yes" -> True
                "true" -> True
                _ -> False

        return $ def & botId .~ bId
                     & botName .~ bName
                     & botAddress .~ bAddress
                     & botPort .~ bPort
                     & botColour .~ bColour
                     & botNotify .~ bNotify

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







