import Yoda
import Control.Applicative
import Data.Monoid

data Chess = Turn Move Move Chess
            | EndGame deriving Show

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

whitespace :: Parser ()
whitespace = many (oneOf " \t,;\n") *> pure ()

tok :: String -> Parser String
tok s = whitespace *> string s <* whitespace

number :: Parser Int
number = read <$> some (oneOf ['1' .. '8']) <* whitespace

piece :: Parser Piece
piece = King <$ tok "K" <* pure King
      <|> Queen <$ tok "Q" <* pure Queen
      <|> Rook <$ tok "R" <* pure Rook
      <|> Knight <$ tok "N" <* pure Knight
      <|> Bishop <$ tok "B" <* pure Bishop
      <|> Pawn <$ tok "P" <* pure Pawn

cell :: Parser Cell
cell = Cell <$> oneOf ['a' .. 'h'] <*> number

winner :: Parser Winner
winner = White <$ pure White
      <|> Black <$ pure Black
      <|> Draw <$ pure Draw
      <|> AO <$ pure AO

movep :: Parser MoveP
movep = Alg <$ tok "Alg" <*> piece <*> cell
    <|> Smh <$ tok "Smh" <*> cell <*> cell
    <|> AlgDis <$ tok "AlgDis" <*> piece <*> cell <*> cell
    <|> Tke <$ tok "Tke" <*> piece <*> cell

quant :: Parser Quant
quant = Prom <$ tok "Prom" <*> piece <*> quant
    <|> Chck <$ tok "Chck" <*> quant <*> tok "+"
    <|> Null <$ pure Null

boolean :: Parser Bool
boolean = True <$ string "true"
        <|> False <$ string "false"

move :: Parser Move
move = Move <$ tok "move" <*> movep <*> quant
    <|> Cslt <$> boolean <* tok "+"
    <|> End <$> winner

chess :: Parser Chess
chess = Turn <$ tok "turn" <*> move <*> move <*> chess
    <|> EndGame <$ pure EndGame

{-parseChess :: String -> Chess
parseChess s = case parseMaybe chess s of
                Nothing -> EndGame
                Just game -> game

type ChessBoard = [((Piece, Bool ), Cell )]

updateCB :: ChessBoard -> Chess -> ChessBoard
updateCB c EndGame = c
updateCB c (Turn m1 m2 ch) = updateCB (updateMoveCB (updateMoveCB c True m1 ) False m2 ) ch

updateMoveCB :: ChessBoard -> Bool -> Move -> ChessBoard
updateMoveCB b p (End w ) = b
updateMoveCB b p (Cslt d )
                      |not d = undefined
                      |otherwise = undefined
updateMoveCB b p (Move mp q) = f b p mp
                            where
                            f :: ChessBoard -> Bool -> MoveP -> ChessBoard
                            f b p (Alg piece (Cell ch n)) = undefined
                            f b p (Smh c1 c2 ) = mapif (\c -> (snd c) == c1 ) (\(x , y) -> (x , c2 )) (filter (\c -> (snd c) /= c2 ) b)
                            f b p (AlgDis pe c1 c2 ) = f b p (Smh c1 c2 )
                            f b p (Tke pe c1 ) = f (filter (\c -> (snd c) /= c1 ) b) p (Alg pe c1 )

mapif :: (a -> Bool ) -> (a -> a) -> [a ] -> [a ]
mapif test func xs = map (g test func) xs
                            where
                            g :: (a -> Bool ) -> (a -> a) -> (a -> a)
                            g t f x
                                  | (t x ) = f x
                                  | otherwise = x

startCB :: ChessBoard
startCB = [((Rook , True), (Cell 'a' 1)), ((Knight, True), (Cell 'b' 1)), ((Bishop, True), (Cell 'c' 1)), ((Queen, True), (Cell 'b' 2))]

showCB :: ChessBoard -> String
showCB b = "a b c d e f g h\n" ++ concat [lookfor y z b | y <- [1 .. 8], z <- ('n' : ['a' .. 'i'])]
                    where
                    lookfor :: Int -> Char -> ChessBoard -> String
                    lookfor n c bpar
                                | c == 'n' = show n ++ " |"
                                | c == 'i' = "\n"
                                | f /= [] = g (head f )
                                | otherwise = "--|"
                                  where
                                  f = filter (\cell -> (snd cell == (Cell c n))) b
                                  g :: ((Piece, Bool ), Cell ) -> String
                                  g ((King, b), c)
                                          | not b = "BK|"
                                          | otherwise = "WK|"
                                  g ((Queen, b), c)
                                          | not b = "BQ|"
                                          | otherwise = "WQ|"
                                  g ((Knight, b), c)
                                          | not b = "BN|"
                                          | otherwise = "WN|"
                                  g ((Rook , b), c)
                                          | not b = "BR|"
                                          | otherwise = "WR|"
                                  g ((Bishop, b), c)
                                          | not b = "BB|"
                                          | otherwise = "WB|"
                                  g ((Pawn, b), c)
                                          | not b = "BP|"
                                          | otherwise = "WP|"

putCB :: ChessBoard -> IO ()
putCB = putStrLn . showCB

game :: Int -> String
game 1 = "1. e2e3 e7e6"
game 2 = "1. e2e3 e7e6 2. c2c3 f8b4"
game 3 = "1. e2e3 e7e6 2. c2c3 f8b4 3. c3b4 d7d6"
game 4 = "1. d4 f5 2. Bg5 h6 3. Bf4 g5 4. Bg3 f4 5. e3 h5 6. Bd3 Rh6 7. Qxh5+ Rxh5 8. Bg6# 1-0"-}
