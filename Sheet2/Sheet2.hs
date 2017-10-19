data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

type Animals = Int
type Territories = Int
type Plot = (Animals, Territories)

quadrant :: Plot -> Plot -> Plot -> Plot -> Plot
quadrant (x, y) (x1, y1) (x2, y2) (x3, y3) = ((x+x1+x2+x3), (y+y1+y2+y3))

territory :: [Animals] ->Int
territory [] = 0
territory (a:as) = 1 + territory as
