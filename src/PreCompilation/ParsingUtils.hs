{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module PreCompilation.ParsingUtils
(iParseMB,sParseMB,fParseMB)
where

import           Data.Text          (Text, length, unpack)
import           Prelude            hiding (length)
import           Text.Parsec        (Parsec, parse)
import qualified Text.Parsec.Number as PN
-------------------------------------------------------------------------
-- | We'll be doing more advanced error handling later
parse' :: Parsec String () a -> Text -> a
parse' p input = case parse p "" (unpack input) of
                  Left e  -> error ("PARSING ERROR for input "
                                    ++show input
                                    ++"\n("++show (length input)++") chars"
                                    ++"\n\n"++show e)
                  Right r -> r

iParse ::  Text -> Int
iParse = parse' PN.int
sParse ::  Text -> Text
sParse = id --unpack -- parse parseString
fParse ::  Text-> Double
fParse = parse' $ PN.sign <*> PN.floating2 True -- NO idea what the bool is for

iParseMB :: Text -> Maybe Int
iParseMB = fmap iParse . isNone
sParseMB :: Text -> Maybe Text
sParseMB = fmap sParse . isNone
fParseMB :: Text -> Maybe Double
fParseMB = fmap fParse . isNone


isNone :: Text -> Maybe Text
isNone "None" = Nothing
isNone x      = Just x

-- If we ever need to do real parsing...
-- import           Control.Applicative           ((<|>))
-- import qualified Text.Parsec.Char              as CH
-- import           Text.ParserCombinators.Parsec (Parser, many, noneOf, oneOf,
--                                                 optional)

-- -------------------------------------------------------------------------
-- -- Helper functions
-- -------------------
-- -- | Ignore spaces + newline
-- whitespace :: Parser ()
-- whitespace = skipMany space
--
-- brackets :: Parser a -> Parser a
-- brackets =  between (wrap $ oneOf "([") (wrap $ oneOf "])")
--
-- wrap :: Parser a -> Parser ()
-- wrap p = do whitespace
--             _<-p
--             whitespace
-- delim :: Parser ()
-- delim = wrap (CH.string "@@@")
-- --------------------
-- escape :: Parser String
-- escape = do
--     d <- CH.char '\\'
--     c <- oneOf  "\\\"\'0nrvtbf"
--     return [d, c]
--
-- nonEscape :: Parser Char
-- nonEscape = noneOf "\\\"\'\0\n\r\v\t\b\f"
--
-- character :: Parser String
-- character = fmap return nonEscape <|> escape
--
-- parseString :: Parser String
-- parseString = do
--     optional (CH.char 'u')
--     optional $ oneOf "\'\""
--     strings <- many character
--     optional $ oneOf "\'\""
--     return $ concat strings
