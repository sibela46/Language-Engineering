data Animal = Lion
            | Tiger
            | Gazelle
            | Ant deriving (Eq, Show)

type Territories = Int
type Size = Float

data Safari = PlotCore Territories [Animal] Size
            | Plots [Safari] deriving (Show)

plotCore :: Safari -- defines a plotcore
plotCore = PlotCore 1 [Lion, Tiger] 0.25

plots :: [Safari]
plots = [plotCore, plotCore, plotCore]

quad :: Safari -> Safari -> Safari -> Safari -> Safari
quad (PlotCore t1 as s1) (PlotCore t2 bs s2) (PlotCore t3 cs s3) (PlotCore t4 ds s4)
                                                                                 | (s1+s2+s3+s4) == 1 = PlotCore (t1+t2+t3+t4) (as++bs++cs++ds) 1
                                                                                 | otherwise = error "Sizes have ot sum up to 1."
territories :: Safari -> Int
territories (PlotCore t ns s) = t
territories (Plots (t : ts)) = territories t + territories (Plots ts)

allAnimals :: Safari -> [Animal]
allAnimals (PlotCore t ns s) = ns
allAnimals (Plots (t : ts)) = allAnimals t ++ allAnimals (Plots ts)

splitThree :: Safari -> Safari -> Safari -> Safari
splitThree (Plots[(PlotCore t1 as s1), (PlotCore t2 bs s2), (PlotCore t3 cs s3)]) = (PlotCore t1 as s1) (PlotCore t2 bs s2) (PlotCore t3 bs s3)
