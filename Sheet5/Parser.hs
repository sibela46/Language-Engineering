import Yoda
import Data.Char

data Robot = Move Int Robot
           | RotateLeft Robot
           | RotateRight Robot
           | Stop deriving Show

whitespace :: Parser ()
whitespace = many (oneOf " \t,;\n") *> pure ()

tok :: String -> Parser String
tok s = whitespace *> string s <* whitespace

number :: Parser Int
number = whitespace *> (read <$> some (oneOf "0123456789")) <* whitespace

parseRobot :: Parser Robot
parseRobot = Move <$ tok "move" <*> number <*> parseRobot
           <|> RotateLeft <$ tok "rotate left" <*> parseRobot
           <|> RotateRight <$ tok "rotate right" <*> parseRobot
           <|> Stop <$ tok "stop"

data DataB = B DataB'
           | A Int
           | D deriving Show

data DataB' = Data Int DataB'
            | Empty deriving Show

parseBad :: Parser DataB
parseBad = B <$> parseBad'
        <|> A <$ tok "a" <*> number
        <|> D <$ tok ""

parseBad' :: Parser DataB'
parseBad' = Data <$> number <*> parseBad'
        <|> Empty <$ pure (Empty)
