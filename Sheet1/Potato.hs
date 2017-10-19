type Time = Int
type Weight = Int
type Cooked = Bool
type Description = String

newtype Potato = Potato (Time, Weight, Cooked, Description) deriving (Show)

potato :: Potato
potato = Potato (0, 3, False, "potato")

peel :: Potato -> Potato
peel (Potato (t, w, c, d)) = Potato ((t + 2), w, c, "peeled " ++ d)

roast :: Potato -> Potato
roast (Potato (t, w, c, d)) = Potato ((t + 70), w, True, "roasted " ++ d)

boil :: Potato -> Potato
boil (Potato (t, w, c, d)) = Potato ((t + 25), w, True, "boiled " ++ d)

mash :: Potato -> Potato
mash (Potato (t, w, c, d)) = Potato ((t + 1), w, c, "mashed " ++ d)

stew :: Potato -> Potato
stew (Potato (t, w, c, d)) = Potato ((t + 120), w, c, "stewed " ++ d)

mix :: Potato -> Potato -> Potato
mix (Potato (t, w, c, d)) (Potato (t', w', c', d')) = Potato (t + t', w + w', c && c, d ++ " and " ++ d')
