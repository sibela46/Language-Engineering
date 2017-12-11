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
