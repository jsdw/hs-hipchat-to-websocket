module App.GetParams (

    -- types
    Params,
    BotParams,

    -- param lenses
    thisAddress,
    thisPort,
    thisBots,

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
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Except as E
import           Data.Default         (Default, def)
import           Data.Monoid          ((<>))
import qualified Data.Map             as M

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
    _thisBots :: [BotParams]
} deriving (Show)
makeLenses ''Params

instance Default Params where
    def = Params {
        _thisAddress = "",
        _thisPort = 0,
        _thisBots = def
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
    (topParams, subParamList) <- case A.parseOnly parseFile conf of
        Left err -> E.throwError err
        Right p -> return p

    --extract top level params
    tAddress <- getParam "address" topParams "'address' param not found"
    _tPort <- getParam "port" topParams "'port' param not found"
    tPort <- toInt _tPort "global port not an int"

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
                 & thisBots .~ bots

getParam str map err = case M.lookup str map of
    Just val -> return val;
    Nothing -> E.throwError err

toInt val err = case readMaybe (T.unpack val) of
    Just v -> return v
    Nothing -> E.throwError err

--
-- Parsers to read bots.conf structure.
--

parseFile = do
    lineSep
    topParams <- parseParam `A.sepBy1` lineSep
    lineSep
    subParams <- parseBlock `A.sepBy1` lineSep
    lineSep
    A.endOfInput
    return (M.fromList topParams, subParams)

parseBlock = do
    id <- parseId
    lineSep
    params <- parseParam `A.sepBy1` lineSep
    return $ (id, M.fromList params)

parseId = let p = A.char '[' *> (A.takeWhile1 $ \c -> not (A.isEndOfLine c) && not (c == ']')) <* A.char ']'
          in p <?> "ID parsing (eg [name])"

lineSep = (A.skipSpace >> parseComment >> A.skipSpace) <|> A.skipSpace

parseComment = do
    A.string "#" <|> A.string ("-"<>"-")
    A.takeWhile $ not . A.isEndOfLine
    return ()

parseParam = 
    let p = do
            paramName <- A.many1 A.letter
            A.takeWhile1 $ \c -> c == ' ' || c == '\t'
            paramValue <- A.takeWhile1 $ not . A.isEndOfLine
            return (paramName, T.stripEnd paramValue)
    in p <?> "Param parsing (eg key value)"






