import Yoda
import Control.Applicative

data Maybe' a = Nothing'
              | Just' a

instance Functor Maybe' where
  fmap f (Nothing') = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure x = Nothing'
  Just' f <*> Just' x = Just' (f x)

data List a = Empty
             | Cons a (List a) deriving Show

instance Functor List where
  fmap f (Empty) = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

(<$) :: a -> List b -> List a
x <$ Empty = Cons x Empty
x <$ Cons y ys = Cons x Empty

instance Applicative List where
  pure x = Empty
  Cons f xs <*> Cons y ys = Cons (f y) (Cons f xs <*> ys)

(<*) :: List a -> List b -> List a
Empty <* Cons x xs = Empty
Cons x xs <* Empty = Cons x xs
Cons x xs <* Cons y ys = Cons x xs

(*>) :: List a -> List b -> List b
Empty *> Cons x xs = Cons x xs
Cons x xs *> Empty = Empty
Cons x xs *> Cons y ys = Cons y ys

instance Alternative List where
  empty = Empty
  Empty <|> Cons x xs = Cons x xs
  Cons x xs <|> Empty = Cons x xs
--  Cons x xs <|> Cons y ys = Cons x (Cons y (xs ++ ys))
