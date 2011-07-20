-- No ScopedTypeVariables, so (v::a) means (v:: forall a.a)

module ShouldCompile where

data T a = T a

instance Eq (T a) where
  (==) x y = let v :: a
		 v = undefined
	     in
		v

