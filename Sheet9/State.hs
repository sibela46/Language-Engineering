import Data.Monoid
import Control.Applicative

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
add x y = State (\s -> (s+x, x+y))

operationMlt :: Int -> Int -> State Int Int
operationMlt x y = State (\s -> (s+x, x*y))

runOp :: Int -> Int -> (Int, Int)
runOp x y = runState (add x y) 0

type MiniImp = State (Int, Int)

getX :: MiniImp Int
getX = fst <$> get

getY :: MiniImp Int
getY = snd <$> get

setX :: Int -> MiniImp ()
setX x = do (_,y) <- get
            put(x,y)

setY :: Int -> MiniImp ()
setY y = do (x,_) <- get
            put(x,y)

while :: ((Int, Int) -> Bool) -> ((Int,Int) -> (Int,Int)) -> MiniImp ()
while p f =
  do
    xy <- get
    if p xy
      then do
        put (f xy)
        while p f
    else return ()

fact :: Int -> Int
fact x =  fst $ execState (while ((>0).snd) (\(x, y)-> (x*y, y-1))) (1, x)

liftM2 :: Monad m => (a->b->c) -> m a -> m b -> m c
liftM2 f mx my = (mx >>= f)
