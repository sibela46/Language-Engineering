import Yoda
import Control.Applicative
import Prelude hiding ((++))

data Maybe' a = Nothing'
              | Just' a

instance Functor Maybe' where
  fmap f (Nothing') = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure x = Just' x
  Nothing' <*> Nothing' = Nothing'
  Nothing' <*> Just' x = Nothing'
  Just' f <*> Nothing' = Nothing'
  Just' f <*> Just' x = Just' (f x)

data List a = Empty
             | Cons a (List a) deriving Show

instance Functor List where
  fmap f (Empty) = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

(<$) :: a -> List b -> List a
x <$ Empty = Cons x Empty
x <$ Cons y ys = Cons x Empty

(++) :: List a -> List a -> List a
Empty ++ x = x
Cons x xs ++ ys = Cons x (xs ++ ys)

instance Applicative List where
  pure x = Cons x Empty
  Empty <*> x = Empty
  Cons f fs <*> xs = (fmap f xs) ++ (fs<*>xs)

(<*) :: List a -> List b -> List a
Empty <* Cons x xs = Empty
Cons x xs <* Empty = Cons x xs
Cons x xs <* Cons y ys = Cons x xs

(*>) :: List a -> List b -> List b
Cons x xs *> Empty = Empty
Empty *> Cons x xs = Cons x xs
Cons x xs *> Cons y ys = Cons y ys

instance Alternative List where
  empty = Empty
  Empty <|> x = x
  x <|> Empty = x
  (Cons x xs) <|> (Cons y ys) = (Cons x xs) ++ (Cons y ys)

liftATwo :: Applicative f => (a->b->c) -> f a -> f b -> f c
liftATwo f x y = f <$> x<*>y
