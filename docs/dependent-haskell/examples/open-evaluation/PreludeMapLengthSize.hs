{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module PreludeMapLengthSize (Nat(..), Size, plus, size, minSize, eqSize) where

import Control.Exception (ArithException(Underflow), throw)
import qualified GHC.TypeNats as TN
import GHC.TypeNats.Experimental (FromNatural)

-- | A Peano natural number.
data Nat = Zero | Succ Nat
    deriving Show

-- | Compatibility name for a natural count.
type Size = Nat

-- | Add two naturals by recursion on the first.
plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

-- | Interpret a built-in natural as a Peano natural in a type.
-- See Note [HaskLib Peano literals].
type family ToPeano (n :: TN.Nat) :: Nat where
    ToPeano 0 = 'Zero
    ToPeano n = 'Succ (ToPeano (n TN.- 1))

type instance FromNatural Nat n = ToPeano n

{- Note [HaskLib Peano literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FromNatural instance converts type literals when their expected kind
is Nat. The Num instance converts ordinary term literals. The
fromInteger method uses the three guards and recursive call checked by
checkedSourceLiteral in GHC.Tc.TyCl.Instance. ExperimentalDependentTypes
publishes the checked dictionary so a retained call such as plusZero 3
can use the same natural on both sides of its index.
-}

-- | Construct nonnegative runtime naturals from ordinary numeral syntax.
instance Num Nat where
    (+) = plus
    m * n = times m
      where
        times Zero = Zero
        times (Succ k) = plus n (times k)
    n - Zero = n
    Zero - Succ _ = throw Underflow
    Succ n - Succ m = n - m
    abs n = n
    signum Zero = Zero
    signum (Succ _) = Succ Zero
    fromInteger n
      | n < 0 = throw Underflow
      | n == 0 = Zero
      | otherwise = Succ (fromInteger (n - 1))

-- | Count the list constructors without inspecting the elements.
size :: [a] -> Nat
size [] = Zero
size (_ : xs) = Succ (size xs)

-- | Take the smaller count. A zero on the left determines the answer.
minSize :: Nat -> Nat -> Nat
minSize Zero _ = Zero
minSize (Succ _) Zero = Zero
minSize (Succ m) (Succ n) = Succ (minSize m n)

-- | Compare two sizes by inspecting their constructors.
eqSize :: Nat -> Nat -> Bool
eqSize Zero Zero = True
eqSize Zero (Succ _) = False
eqSize (Succ _) Zero = False
eqSize (Succ m) (Succ n) = eqSize m n
