{- Contains the code for XVL format parser. -}
{-
    Example of XVL file:
        foo {
            bar
            baz = 12
            qux = { a, b, "c" }
        }
 -}
module Text.XVL.Parser (
        parseXVL
        ) where

import Control.Monad (liftM, liftM2)
import Text.ParserCombinators.Parsec
import Text.XVL.Structure


parseXVL :: String -> Either ParseError XVLDocument
parseXVL input = parse document "(unknown)" input


-- Parser definition

document :: CharParser () XVLDocument
document = many item

item :: CharParser () XVLItem
item = try section <|> try keyValue <?> "section or key-value pair"

section :: CharParser () XVLItem
section = liftM2 XVLSection identifier itemsInCurly
          where itemsInCurly = spaces >> insideCurly item `sepBy` itemSep
                itemSep = spaces >> many (oneOf ",;") 

keyValue :: CharParser () XVLItem
keyValue =  liftM2 XVLKeyValue identifier maybeValue
            where maybeValue = optionMaybe (eq >> value)
                  eq = spaces >> char '=' >> spaces     

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

identifier :: CharParser () String
identifier = many1 identifierChar
             where identifierChar = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()-_+<>:?/\\|"
