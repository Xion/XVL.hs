{- Contains the code for XVL format parser. -}
{-
    Example of XVL file:
        foo {
            bar
            baz = 12
            qux = { a, b, "c" }
        }
 -}
module Text.XVL.Parser () where

import Text.ParserCombinators.Parsec
import Text.XVL.Structure
import Control.Monad (liftM, liftM2)


document :: CharParser () XVLDocument
document = many item

item :: CharParser () XVLItem
item = try section <|> try keyValue <?> "section or key-value pair"

section :: CharParser () XVLItem
section = liftM2 XVLSection identifier itemsInCurly
          where itemsInCurly = optWS >> insideCurly item `sepBy` itemSep
                optWS = skipMany whitespace
                itemSep = try whitespace <|> (optWS >> many (oneOf ",;"))

keyValue :: CharParser () XVLItem
keyValue =  liftM2 XVLKeyValue identifier maybeValue
            where maybeValue = optionMaybe (eq >> value)
                  eq = skipMany whitespace >> char '=' >> skipMany whitespace

value :: CharParser () XVLValue
value = textValue <|> arrayValue <?> "text or array"

textValue :: CharParser () XVLValue
textValue = XVLText `liftM` (longTextValue <|> shortTextValue)
            where longTextValue = insideQuotes $ many (noneOf "\"")
                  shortTextValue = identifier

arrayValue :: CharParser () XVLValue            
arrayValue = XVLArray `liftM` many value


-- Common parsing functions

betweenChars open close = between (char open) (char close)
insideCurly = betweenChars '{' '}'
insideQuotes = betweenChars '"' '"'

        
-- Base building blocks

identifier :: GenParser Char st String
identifier = many1 identifierChar
identifierChar = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()-_+<>:?/\\|"

whitespace = many1 whitespaceChar
whitespaceChar = oneOf " \t\r\n"