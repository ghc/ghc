{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

{-  In -ddump-simpl output we do not want to see
         case == @(F Int) lvl2_sQO eta_B0 eta_B0 of {
    where (==) is called at a known type
-}

module T26117 where

type family F a
type instance F Int = Bool

class Eq (F a) => D a b where {  dop1, dop2 :: a -> b -> b }

class C a b where { op1,op2 :: F a -> a -> b -> Int }

instance (Eq (F a), D a b) => C a [b] where
  op1 x _ _ | x==x      = 3
            | otherwise = 4
  {-# SPECIALISE instance D Int b => C Int [b] #-}
