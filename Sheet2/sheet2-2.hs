import Data.List

data Animal = Lion
        | Tiger
        | Gazelle
        | Ant
      deriving (Eq,Show)

--NTeritories :: Int
--NAnimals :: Int
--Id :: Int


--numberT :: Safari -> Int
--numberT (w,x,y,z) = (w + x + y + z)

--SHALLOW EMBEDDING
type Plot = (Int,Int)
type SPlot = (Int, Int,[Animal])
type SSafari = (SPlot, SPlot, SPlot, SPlot)

territory :: [Animal] -> Plot
territory xs = (1, length xs)

safari :: Plot -> Plot -> Plot -> Plot -> Plot
safari (t1,a1) (t2,a2) (t3,a3) (t4,a4) = (t1+t2+t3+t4,maximum[a1,a2,a3,a4])

aList :: SSafari-> [Animal]
aList ((_,_,as),(_,_,bs),(_,_,cs),(_,_,ds)) = nub(as ++ bs ++ cs ++ ds)

--DEEP EMBEDDING
data Safari' = Territory' [Animal]
            | Plots' Safari' Safari' Safari' Safari'
            | SubPlots' Safari' Safari' Safari'

territory' :: Safari' -> Int
territory' (Territory' xs) = 1
territory' (Plots' a b c d) = (territory' a) + (territory' b) + (territory' c)
                              + (territory' d)
territory' (SubPlots' a b c) = (territory' a) + (territory' b) + (territory' c)

max' :: Safari' -> Int
max' (Territory' xs) = length(xs)
max' (Plots' a b c d) = maximum[max' a,max' b,max' c,max' d]
max' (SubPlots' a b c) = maximum[max' a,max' b,max' c]

aniList :: Safari' -> [Animal]
aniList (Territory' xs) = nub xs
aniList (Plots' a b c d) = nub(aniList(a) ++ aniList(b) ++ aniList(c) ++ aniList(d))

--Question 6
data Safari'' = Territory'' [Animal]
            | Plots'' ([Safari''],Size)

type Size = Float

territory'' :: Safari'' -> Int
territory'' (Territory'' xs) = 1
territory'' (Plots'' ((a as),b)) = (territory'' (a,b))
