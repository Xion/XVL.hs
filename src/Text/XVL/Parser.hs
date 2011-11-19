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

import Data.List (intercalate)
import Control.Monad (liftM, liftM2)
import Text.ParserCombinators.Parsec
import Text.XVL.Structure


parseXVL :: String -> Either ParseError XVLDocument
parseXVL = parse document "(unknown)"


-- Parser definition

document :: CharParser () XVLDocument
document = items

items :: CharParser () [XVLItem]
items = skipS >> item `sepEndBy` spacing

item :: CharParser () XVLItem
item = try section <|> try keyValue <?> "section or key-value pair"

section :: CharParser () XVLItem
section = liftM2 XVLSection identifier content
          where content = skipS >> insideCurly items

keyValue :: CharParser () XVLItem
keyValue =  liftM2 XVLKeyValue identifier maybeValue
            where maybeValue = optionMaybe (eq >> value)
                  eq = skipS >> char '=' >> skipS

value :: CharParser () XVLValue
value = textValue <|> arrayValue <?> "text or array"

textValue :: CharParser () XVLValue
textValue = XVLText `liftM` (longTextValue <|> shortTextValue)
            where longTextValue = insideQuotes $ many (noneOf "\"")
                  shortTextValue = identifier

arrayValue :: CharParser () XVLValue
arrayValue = XVLArray `liftM` insideCurly values
             where values = skipS >> value `sepEndBy` spacing


-- Common parsing functions

skipS = skipMany spacing

betweenChars open close = between (char open) (char close)
insideCurly = betweenChars '{' '}'
insideQuotes = betweenChars '"' '"'

        
-- Base building blocks

identifier :: CharParser () String
identifier = many1 identifierChar
             where identifierChar = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@$%^&*()-_+<>:?/\\|"
             
spacing :: CharParser () String
spacing = join `liftM` many1 sepElem
          where join = intercalate ""
                sepElem = comment <|> many1 (space <|> oneOf ",;")

comment :: CharParser () String
comment = between (char '#') eol $ many anyChar

-- end of line
eol :: CharParser () String
eol = try (string "\r\n") <|> string "\r" <|> string "\n"
