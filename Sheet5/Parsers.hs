import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

newtype Parser a = Parser (String -> [(String, a)])

parse' :: Parser a -> String -> [(String, a)]
parse' (Parser px) ts = px ts

produce :: a -> Parser a
produce x = Parser (\ts -> [(ts, x)])

whitespace :: Parser ()
whitespace = Parser (\ts -> [(ts, ())])

tok :: String -> Parser String
tok ts = Parser (\ts -> [([], ts)])
