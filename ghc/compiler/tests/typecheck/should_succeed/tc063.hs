module ShouldSucceed where

data X a = Tag a 

class Reps r where
 f :: r -> r -> r

instance Reps (X q) where
-- f (Tag x) (Tag y) = Tag y
 f x y = y

instance Reps Bool where
 f True True = True
 f x y = False

g x = f x x


