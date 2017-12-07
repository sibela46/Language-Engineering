data Maybe' a = Nothing'
            | Just' a

instance Monad Maybe' where
  return x = Just' x
  Just' x >>= f = f x
  Nothing' >>= f = Nothing'

data Exception e a = Throw e
                   | Continue a

instance Monad (Exception a) where
  return x = Continue x
  Continue x >>= f = f x
  Throw e >>= f = Throw e

data Tree a = Leaf a
            | Fork (Tree a) (Tree a) deriving Show

instance Functor Tree where
--  return x >>= f = f x
--  fmap f mx = mx >>= return f
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

instance Monad Tree where
  return x = Leaf x
  Leaf x >>= f = f x
  Fork l r >>= f = Fork (l >>= f) (r >>= f)

intToDoubleTree :: Int -> Tree Int
intToDoubleTree x = Leaf (2*x)

addToTree :: Int -> Int -> Tree Int
addToTree x y = Leaf (x+y)

manipTree :: Tree Int -> Tree Int
manipTree tree = tree >>= addToTree 3 >>= intToDoubleTree >>= addToTree 2

dupToTree :: a -> Tree a
dupToTree x = Fork (Leaf x) (Leaf x)

duplicate :: Tree a -> Tree a
duplicate tree = tree >>= dupToTree

complicated :: Tree Int -> Tree Int
complicated tree =  tree >>= dupToTree >>= intToDoubleTree
