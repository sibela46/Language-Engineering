data Robot = Move Int Robot
              | Rotate Int Robot
              | Stop deriving (Show)

data Direction = North
                | South
                | West
                | East
                | Null deriving (Eq, Show)

distTrav :: Robot -> Int
distTrav (Move x y) = x + distTrav y
distTrav (Rotate x y) = distTrav y
distTrav (Stop) = 0

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

distSameDir :: Robot -> Direction -> Direction -> Int
distSameDir (Move x y) d old
                            | (d == old) = x + distSameDir y d old
                            | otherwise = distSameDir y d old
distSameDir (Rotate x y) d old = distSameDir y (changeDir d x) old
distSameDir (Stop) d old = 0

toInt :: Float -> Int
toInt = round

distStraight :: Robot -> Int -> Int -> Direction -> Direction -> Float
distStraight (Move a r) x y d old
                                | (d == old) = distStraight r (x + a) y d old
                                | otherwise = distStraight r x (y + a) d old
distStraight (Rotate a r) x y d old = distStraight r x y (changeDir d a) d
distStraight (Stop) x y d old = sqrt(fromIntegral(x*x + y*y))
