data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

type Territories = Int
type NumberAnmals = Int
type Size = Int

type Plot = (Territories, NumberAnmals, Size)
type Safari = [Plot]

territory :: [Animal] -> Size -> Plot
territory as s = (1, length as, s)

quad :: Plot -> Plot -> Plot -> Plot -> Plot
quad (t1, n1, s1) (t2, n2, s2) (t3, n3, s3) (t4, n4, s4) = ((t1 + t2 + t3 + t4), (maximum[n1, n2, n3, n4]), 1)
