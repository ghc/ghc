{-# LANGUAGE TypeFamilies, GADTs #-}

-- Tests record update in the presence of 
-- existentials, GADTs, type families

module Rec where

----------------- Existential
data S a where
   S1 :: { fs1 :: a, fs2 :: b } -> S a
   S2 :: { fs1 :: a } -> S a

updS s x = s { fs1=x }

------------------ GADT
data T a b where
   T1 :: { ft1 :: a, ft2 :: c, ft3 :: d } -> T a Int
   T2 :: { ft1 :: a, ft3 :: c } -> T a Int
   T3 :: T Int b

f :: T a1 b -> a2 -> T a2 b
f x v = x { ft1 = v }

------------------ Type family
data family R a
data instance R (a,b) where
   R1 :: { fr1 :: a, fr2 :: b, fr3 :: c } -> R (a,b)
   R2 :: { fr1 :: a, fr3 :: c } -> R (a,b)

updR r x = r { fr1=x }
