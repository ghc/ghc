{-# LANGUAGE TypeFamilies #-}

module T12212 where

type family F a
type instance F Int = Int

foo :: a -> F a
{-# NOINLINE foo #-}
foo = undefined

-- Inferred type
-- forall a b. (Num a, F a ~# F b) => a -> b -> [F a]
f x y = [ foo x, foo y ] ++ f (x-1) y

-- Specialised call to f @ Int @ Int dNumInt <F Int ~ F Int>
g = f (3::Int) (4::Int)
