data NatF k = ZF | SF k deriving (Show)

data Fix f = In(f(Fix f))

in' :: Fix f -> f(Fix f)
in' (In x) = x

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg.fmap(cata alg).in'

instance Functor NatF where
  fmap f ZF = ZF
  fmap f (SF x) = SF (f x)

toInt :: Fix NatF -> Int
toInt = cata alg where
  alg :: NatF Int -> Int
  alg ZF = 0
  alg (SF n) = 1 + n

double :: Fix NatF -> Fix NatF
double = cata alg where
  alg :: NatF (Fix NatF) -> Fix NatF
  alg ZF = In ZF
  alg (SF n) = In (SF (In (SF n)))

power :: Fix NatF -> Fix NatF
power = cata alg where
  alg :: NatF (Fix NatF) -> Fix NatF
  alg ZF = In (SF (In ZF))
  alg (SF n) = In (SF (In ZF))* n

instance Num NatF where
  (*) ZF a = ZF
  (*) (SF) (SF) = In (SF (In (SF (In (SF (In ZF))))))
