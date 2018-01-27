{-# LANGUAGE QuantifiedConstraints  #-}

-- Two simple examples that should work

module T2893 where

f :: forall b c. (Eq b, forall a. Eq a => Eq (c a)) => c b -> Bool
{-# NOINLINE f #-}
f x = x==x

g x = f [x]

data Rose f x = Rose x (f (Rose f x))

instance (Eq a, forall b. Eq b => Eq (f b))
      => Eq (Rose f a)  where
  (Rose x1 rs1) == (Rose x2 rs2)
     = x1==x2 && rs1 == rs2