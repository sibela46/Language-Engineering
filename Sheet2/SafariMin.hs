data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

data Safari = Plot ([Safari], Float)
            | Territory [Animal] deriving (Show)

territory :: Safari
territory = Territory [Lion, Tiger]

safari :: Safari
safari = Plot ([territory, territory], 4.5)

territories :: Safari -> Int
territories (Territory as) = 1
territories (Plot((t:ts), a)) = territories t + (territories (Plot(ts, a)))

allAnimals :: Safari -> [Animal]
allAnimals (Territory as) = as
allAnimals (Plot((t:ts), a)) = allAnimals t ++ allAnimals (Plot(ts, a))

quad :: Safari -> Safari -> Safari -> Safari -> Safari
quad (Plot(as, a)) (Plot(bs, b)) (Plot(cs, c)) (Plot(ds, d)) = (Plot(as, 1))
