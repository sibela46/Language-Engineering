{-# LANGUAGE TypeOperators #-}

data Nondet cnt = Fail
                | Split cnt cnt

instance Functor Nondet where
  fmap f Fail = Fail
  fmap f (Split x y) = Split (f x) (f y)

data Exception e cnt = Throw e
                    | Continue cnt

instance Functor (Exception e) where
  fmap f (Throw e) = Throw e
  fmap f (Continue k) = Continue (f k)

data STATE s cnt = Get (s->cnt)
                 | Put s cnt

instance Functor (STATE s) where
  fmap f (Get g) = Get (f.g)
  fmap f (Put s k) = Put s (f k)

data Free f a = Var a
              | Con (f(Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Var v) = Var (f v)
  fmap f (Con op) = Con (fmap (fmap f) op)

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

liftM2 :: Monad m => (a->b->c) -> m a -> m b -> m c
liftM2 f ma mb = ma >>= (\a -> (mb >>= (\b -> return(f a b))))

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 id

instance Functor f => Monad (Free f) where
  return v = Var v
  Var v >>= f = f v
  Con op >>= f = Con (fmap (>>=f) op)

eval :: Functor f => (f a -> a) -> Free f a -> a
eval alg (Var v) = v
eval alg (Con op) = alg (fmap (eval alg) op)

handle :: Functor f => (a -> b) -> (f b -> b)-> Free f a-> b
handle gen alg = eval alg . fmap gen

handleNondet :: Free (Nondet) a -> [a]
handleNondet = handle gen alg where
  gen :: a -> [a]
  gen x = [x]
  alg :: Nondet [a] -> [a]
  alg Fail = []
  alg (Split x y) = x++y

handleExce :: Free (Exception e) a -> Either e a
handleExce = handle gen alg where
  gen :: a -> Either e a
  gen x = Right x
  alg :: Exception e (Either e a)-> Either e a
  alg (Throw x) = Left x
  alg (Continue k) = k

handleState :: Free (STATE s) a -> (s -> (s, a))
handleState = handle gen alg where
  gen :: a -> (s -> (s, a))
  gen x = (\s -> (s,x))
  alg :: STATE s (s -> (s,a)) -> (s -> (s, a))
  alg (Get f) = (\s -> f s s)
  alg (Put s' k) = (\s -> k s)

data (:+:) f g k = L (f k)
                 | R (g k)

infixr 5 :+:

instance (Functor f, Functor g) => Functor (f:+:g) where
  fmap h (L op) = L (fmap h op)
  fmap h (R op) = R (fmap h op)

{-instance Applicative (Exception e) where
  pure x = Continue x
  (Throw e) <*> (Continue x) = Throw e
  (Continue f) <*> (Continue y) = Continue (f y)

instance Monad (Exception e) where
  return x = Continue x
  Continue x >>= f = (f x)-}

runExceHandler :: Functor g => Free ((Exception e) :+: g) a -> Free g (Either e a)
runExceHandler = handle gen alg where
  gen :: Functor g => a -> Free g (Either e a)
  gen x = return (Right x)
  alg :: Functor g => (Exception e :+: g)(Free g (Either e a)) -> Free g (Either e a)
  alg (L (Throw e)) = return (Left e)
  alg (L (Continue k)) = k
  alg (R op) = Con op

{-runStateHandler :: Functor g => Free ((STATE s) :+: g) a -> s -> Free g ((s, a))
runStateHandler = handle gen alg where
  gen :: Functor g => a -> s ->  Free g ((s, a))
  gen x s = return ((s, x))
  alg :: Functor g => (STATE s :+: g)(Free g (s, a)) -> s -> Free g ((s, a))
  alg (L (Get f)) = (\s -> f s)-}

myCon :: Free Nondet [a] -> Free Nondet [a] -> [a]
myCon freeA freeB = concat (handleNondet freeA ++ handleNondet freeB)

runNondetHandler :: Functor g => Free (Nondet :+: g) a -> Free g ([a])
runNondetHandler = handle gen alg where
  gen :: Functor g => a -> Free g ([a])
  gen x = return [x]
  alg :: Functor g => (Nondet :+: g)(Free g ([a])) -> Free g ([a])
  alg (L (Fail)) = return []
  alg (L (Split x y)) = return (myCon x y)
  alg (R op) = Con op

data Void s

instance Functor Void where
  fmap = undefined

run :: Free (Void) a -> a
run (Var x) = x
run _ = undefined

{-runGlobalExce :: Free (Nondet :+: (Exception e) :+: Void) a -> Either e [a]
runGlobalExce = -}
