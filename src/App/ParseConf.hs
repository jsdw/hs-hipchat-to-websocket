module App.ParseConf (parseConf) where

import qualified Data.Text            as T
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Map             as M
import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))


parseConf = A.parseOnly parseFile

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

lineSep = do
    A.skipSpace
    A.many' (parseComment >> A.skipSpace)
    return ()

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