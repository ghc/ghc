{-# LANGUAGE GHC2024, BlockArguments, MagicHash #-}

module T26216 (main) where

import Data.Kind (Type, Constraint)
import GHC.TypeNats
import GHC.Exts (proxy#)

import T26216_aux

getN :: forall (n :: Nat). SNat n -> Natural
getN s = withKnownNat s (natVal s)

type C :: forall {k}. (k -> Constraint) -> k -> Type
data C c a where { C :: c a => C c a }

know :: forall (n :: Nat). SNat n -> C KnownNat n
know s = withKnownNat s C

getC :: forall (n :: Nat). C KnownNat n -> Natural
getC C = natVal' (proxy# @n)

main :: IO ()
main = do
    let !s = mkSome $ natSing @42
        !c = withSome s $ mkSome . know
    print $ withSome s getN
    print $ withSome c getC
