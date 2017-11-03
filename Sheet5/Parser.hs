import Text.Megaparsec
import Control.Applicative
import Text.Megaparsec.String
import Text.Megaparsec.Lexer

{-
Question 1
a) string term <*> "+" <*> term
b) many (string "+" *> term) returns 0 or more of the following string
c)
-}

data MySentence = Sent MyWord [MyWord] deriving Show
data MyWord = MyWord MyLetter [MyLetter] deriving Show
data MyLetter = MyLetter Char deriving Show

sentence :: Parser MySentence
sentence = Sent <$> word <*> some (string " " *> word)

word :: Parser MyWord
word = MyWord <$> letter <*> some letter

letter :: Parser MyLetter
letter = MyLetter <$> alphaNumChar

helloParse :: Parser String
helloParse = string "hello"

manyHellos :: Parser [String]
manyHellos = many (string "hello")

someHellos :: Parser [String]
someHellos = some (string "hello")

onlySome :: Parser Char
onlySome = oneOf ['h', 'i']

none :: Parser Char
none = noneOf [',', '!']

tryParse :: Parser a -> Parser a
tryParse p = try (p)
