module T22684 where

-- Example 1 from #22684
p :: (Int ~ Bool => r) -> r
p _ = undefined

q :: r
q = p _

-- Example 3 from #22684
class Category k where
  (.) :: k b c -> k a b -> k a c

data Free p a b where
  Prod :: Free p a (b, c)
  Sum  :: Free p (Either a b) c

instance Category (Free p) where
  Sum . Prod = _
