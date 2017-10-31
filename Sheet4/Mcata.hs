{-# LANGUAGE RankNTypes #-}

data Fix f = In(f(Fix f))

in' :: (Fix f) -> f(Fix f)
in' (In x) = x

mcata :: Functor f => (forall x.(x -> a) -> (f x -> a)) -> Fix f -> a
mcata phi = phi(mcata phi).in'

data List a x = Empty
              | Cons a x

instance Functor (List a) where
  fmap f (Empty) = Empty
  fmap f (Cons a x) = Cons a (f x)

len_alg :: (x -> Int) -> (List b x -> Int)
len_alg f (Empty) = 0
len_alg f (Cons a x) = 1 + f x

ourLength :: Fix (List b) -> Int
ourLength = mcata len_alg

sum_alg :: (x -> Int) -> (List Int x -> Int)
sum_alg f (Empty) = 0
sum_alg f (Cons a x) = a + f x

ourSum :: Fix (List Int) -> Int
ourSum = mcata sum_alg

myList :: Fix (List Int)
myList = (In(Cons 5 (In (Cons 3 (In Empty)))))
