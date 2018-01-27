{-# LANGUAGE QuantifiedConstraints  #-}

module T2893c where

data Rose f x = Rose x (f (Rose f x))

instance Eq a => Eq (Rose f a)  where
  (Rose x1 _) == (Rose x2 _) = x1==x2
  
-- Needs superclasses
instance (Ord a, forall b. Ord b => Ord (f b))
      => Ord (Rose f a)  where
  (Rose x1 rs1) >= (Rose x2 rs2)
     = x1 >= x2 && rs1 == rs2
  a <= b = False  -- Just to suppress the warning