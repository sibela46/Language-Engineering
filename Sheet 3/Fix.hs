data Tree a = Leaf
            | Branch a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf) = Leaf
  fmap f (Branch a left right) = Branch (f a) (fmap f left) (fmap f right)

data TreeF a k = LeaF
               | BranchF a k k

instance Functor (TreeF a) where
  fmap f (LeaF) = (LeaF)
  fmap f (BranchF a left right) = BranchF a (f left) (f right)

sumTree :: Tree Integer -> Integer
sumTree (Leaf) = 0
sumTree (Branch a left right) = a + sumTree left + sumTree right

data Fix f = In(f(Fix f))

in' :: (Fix f) -> f (Fix f)
in' (In x) = x

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg.fmap(cata alg).in'

sumTreeF :: Fix(TreeF Integer) -> Integer
sumTreeF = cata alg where
  alg :: TreeF Integer Integer -> Integer
  alg (LeaF) = 0
  alg (BranchF a left right) = a + left + right

numberLeaves :: Tree Integer -> Integer
numberLeaves (Leaf) = 1
numberLeaves (Branch a left right) = numberLeaves left + numberLeaves right

numberLeavesF :: Fix(TreeF Integer) -> Integer
numberLeavesF = cata alg where
  alg :: TreeF Integer Integer -> Integer
  alg (LeaF) = 1
  alg (BranchF a left right) = left + right

newTree :: Fix (TreeF Integer)
newTree = (In(BranchF 4 (In(BranchF 3 (In(LeaF)) (In(LeaF)))) (In(LeaF))))
