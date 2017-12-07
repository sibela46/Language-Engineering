import Data.Monoid

data State s a = State (s -> (s, a))

runState :: State s a -> s -> (s, a)
runState (State prog) = prog

execState :: State s a -> s -> s
execState prog = \s -> fst(runState prog s)

evalState :: State s a -> s -> a
evalState prog = \s -> snd (runState prog s)

instance Functor (State s) where
  fmap f s = s >>= (return.f)

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  (State prog) <*> (State p) = State (\s -> (s, let (s', x) = p s in (snd (prog s')) x))

instance Monad (State s) where
  return x = State (\s -> (s, x))
  State p >>= f = State (\s -> let (s', x) = p s in runState (f x) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\_ -> (x, ()))

add :: Int -> Int -> State Int Int
add x y = State (\s -> (s, x+y))
