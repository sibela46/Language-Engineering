import Text.Megaparsec
import Control.Applicative
import Data.Monoid

data Chess = Turn Move Move Chess
            | EndTurn deriving Show

data Move = Move MoveP Quant
    	    | Cslt Bool
          | End Winner deriving Show

data Quant = Prom Piece Quant
           | Chck Quant
           | Null deriving Show

data MoveP = Alg Piece Cell
           | Smh Cell Cell
           | AlgDis Piece Cell Cell
           | Tke Piece Cell deriving Show

data Winner = White
            | Black
            | Draw
            | AO deriving Show

data Cell = Cell Char Int deriving (Show, Eq)

data Piece = King
           | Queen
           | Rook
           | Knight
           | Bishop
           | Pawn deriving (Show, Eq)

newtype Parser a = Parser (String -> [(String, a)])

piece :: Parser [Piece]
piece = King <$ "king"
      <|> Queen <$ "queen"
      <|> Rook <$ "rook"
      <|> Knight <$ "knight"
      <|> Bishop <$ "bishop"
      <|> Pawn <$ "pawn"
