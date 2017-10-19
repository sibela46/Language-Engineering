data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

type Territories = Int
type NumAnimals = Int
type Plot = (Territories, NumAnimals, [Animal])

plot :: Plot
plot = (4, 10, [Tiger, Lion])

plotMax :: Plot
plotMax = (4, 15, [Tiger, Lion, Ant, Gazelle])

animals :: [Animal]
animals = [Tiger, Lion, Lion, Ant, Gazelle]

allAnimals :: [[Animal]] -> [Animal]
allAnimals [] = []
allAnimals ([]:ass) = allAnimals ass
allAnimals ((a:as):ass)
                      | a `elem` as = allAnimals (as:ass)
                      | otherwise = a : (allAnimals (as:ass))

quadrant :: Plot -> Plot -> Plot -> Plot -> Plot
quadrant (x1, y1, a1) (x2, y2, a2) (x3, y3, a3) (x4, y4, a4) = ((x1+x2+x3+x4), maximum [y1, y2, y3, y4], concat [a1, a2, a3, a4])

splitThree :: Plot -> Plot -> Plot -> Plot
splitThree (x1, y1, a1) (x2, y2, a2) (x3, y3, a3) = ((x1+x2+x3), maximum [y1, y2, y3], concat [a1, a2, a3])

numberAnimals :: [Animal] -> Int --calculates the number of animals in a list
numberAnimals [] = 0
numberAnimals (a:as) = 1 + numberAnimals as

territory :: [Animal] -> Plot --defines a territory (base case plot)
territory as = (1, numberAnimals as, as)

printAnimals :: [Animal] -> IO ()
printAnimals [] = error ""
printAnimals (a:as) = do print a
                         printAnimals as

main :: IO ()
main = do
      print ("Plot is", plot)
      print ("Quadrant is", quadrant plot plot plot plotMax)
      print ("Territory is", territory animals)
