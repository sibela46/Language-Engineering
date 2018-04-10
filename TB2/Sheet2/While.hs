module While where
import Yoda
import Data.Foldable

type Var = String

data Aexpr = Num Int
           | Var String
           | Aexpr :+: Aexpr
           | Aexpr :*: Aexpr
           | Aexpr :-: Aexpr deriving Show

data Bexpr = T
           | F
           | Aexpr :=: Aexpr
           | Aexpr :<=: Aexpr
           | Not Bexpr
           | Bexpr :&&: Bexpr deriving Show

data Stm = Var := Aexpr
           | Skip
           | Stm :> Stm
           | If Bexpr Stm Stm
           | While Bexpr Stm
           | Decl Block Proc Stm
           | Call Var deriving Show

data Block = DV Var Aexpr Block | EmptyBlock deriving Show

data Proc = DP Var Stm Proc | EmptyProc deriving Show

chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
          <|> return x


precedence :: [Parser(a->a->a)] -> Parser a  -> Parser a
precedence ops arg = foldl build arg ops
      where build term ops = chainl term ops

aexpr :: Parser Aexpr
aexpr =  precedence [(:*:) <$ tok "*",
        (:+:) <$ tok "+" <|> (:-:) <$ tok "-"]
        $ Num <$> num
        <|> Var <$> var
        <|> tok "(" *> aexpr <* tok ")"

bexpr :: Parser Bexpr
bexpr = bexpr'
    <|> (:&&:) <$> bexpr' <* tok "&" <*> bexpr'
    <|> (:=:) <$> aexpr <* tok "=" <*> aexpr
    <|> (:<=:) <$> aexpr <* tok "<=" <*> aexpr
    <|> Not <$ tok "~" <*> bexpr'

bexpr' = T <$ tok "true"
     <|> F <$ tok "false"

stm :: Parser Stm
stm = chainl stm' ((:>) <$ tok ";")

stm' = (:=) <$> var <* tok ":=" <*> aexpr
  <|> Skip <$ tok "skip"
  <|> If <$ tok "if" <*> bexpr <* tok "then" <*> stm' <* tok "else" <*> stm'
  <|> While <$ tok "while" <*> bexpr <* tok "do" <*> stm'
  <|> Decl <$ tok "begin" <*> block <*> proc <*> stm <* tok "end"
  <|> Call <$ tok "call" <*> var
  <|> tok "(" *> stm <* tok ")"

block :: Parser Block
block = DV <$ tok "var" <*> var <* tok ":=" <*> aexpr <* tok ";" <*> block
      <|> pure EmptyBlock

proc :: Parser Proc
proc = DP <$ tok "proc" <*> var <* tok "is" <*> stm <* tok ";" <*> proc
      <|> pure EmptyProc

whitespace :: Parser ()
whitespace = () <$ many(oneOf " \t\n\r")

num :: Parser Int
num = read <$> some (oneOf ['0' .. '9']) <* whitespace

var :: Parser String
var = some (oneOf ['a' .. 'z']) <* whitespace

tok :: String -> Parser String
tok s = string s <* whitespace
