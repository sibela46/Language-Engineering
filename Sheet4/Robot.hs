data Robot k = Move Int k
             | Rotate Int k
             | Stop deriving (Show)

data Direction = North
                | South
                | West
                | East
                | Null deriving (Eq, Show)

instance Functor Robot where
  fmap f (Stop) = Stop
  fmap f (Move x k) = Move x (f k)
  fmap f (Rotate x k) = Rotate x (f k)

data Fix f = In(f(Fix f))

in' :: (Fix f) -> f(Fix f)
in' (In x) = x

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg.fmap(cata alg).in'

distTrav :: Fix (Robot) -> Int
distTrav = cata alg where
    alg :: Robot Int -> Int
    alg Stop = 0
    alg (Move a k) = a + k
    alg (Rotate a k) = k

changeDir :: Direction -> Int -> Direction
changeDir d x
            | (d == North && x == 90) = East
            | (d == North && x == -90) = West
            | (d == South && x == 90) = West
            | (d == South && x == -90) = East
            | (d == East && x == 90) = South
            | (d == East && x == -90) = North
            | (d == West && x == 90) = North
            | (d == West && x == -90) = South
            | otherwise = Null

distSameDir :: Fix (Robot) -> Direction -> Direction -> Int
distSameDir = cata alg where
    alg :: Robot (Direction -> Direction -> Int) -> Direction -> Direction -> Int
    alg (Move a k) d first
                        | (d == first) = a + k d first
                        | otherwise = k d first
    alg (Rotate a k) d first = k (changeDir d a) first
    alg (Stop) d first = 0

distStraight :: Fix (Robot) -> Int -> Int -> Direction -> Direction -> Float
distStraight = cata alg where
  alg :: Robot (Int -> Int -> Direction -> Direction -> Float) -> Int -> Int -> Direction -> Direction -> Float
  alg (Move a k) x y d old
                          | (d == old) = k (x+a) y d old
                          | otherwise = k x (y+a) d old
  alg (Rotate a k) x y d old = k x y (changeDir d a) d
  alg (Stop) x y d old = sqrt(fromIntegral(x*x + y*y))
