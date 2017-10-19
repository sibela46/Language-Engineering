data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

data Safari = Plot Safari Safari Safari Safari
            | SplitThree Safari Safari Safari
            | Territory [Animal] deriving (Show)

territory :: Safari
territory = Territory animals

territory2 :: Safari
territory2 = Territory animals2

safari :: Safari
safari = Plot territory territory territory territory2

animals :: [Animal]
animals = [Lion, Tiger]

animals2 :: [Animal]
animals2 = [Gazelle, Ant, Tiger]

plot :: Safari
plot = (Territory [Tiger, Lion])

allAnimals :: Safari -> [Animal]
allAnimals (Territory as) = as
allAnimals (Plot s1 s2 s3 s4) = (allAnimals s1) ++ (allAnimals s2) ++ (allAnimals s3) ++ (allAnimals s4)
allAnimals (SplitThree s1 s2 s3) = (allAnimals s1) ++ (allAnimals s2) ++ (allAnimals s3)
