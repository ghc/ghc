{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Bug where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))
import Data.Void (Void)

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
data SUnit :: () -> Type where
  SUnit :: SUnit '()

type family IsUnit (u :: ()) :: Bool where
  IsUnit '() = True

sIsUnit :: SUnit u -> SBool (IsUnit u)
sIsUnit SUnit = STrue

type family If (c :: Bool) (t :: Type) (f :: Type) :: Type where
  If True  t _ = t
  If False _ f = f

type family F (u1 :: ()) (u2 :: ()) :: Type where
  F u1 u2 =
    If (IsUnit u1) (Case u2) Int

type family Case (u :: ()) :: Type where
  Case '() = Int

g1 :: F u1 u2 :~: Char
   -> SUnit u1 -> SUnit u2
   -> Void
g1 Refl su1 su2
  | STrue <- sIsUnit su1
  = case su2 of {}

-- g1a :: F u1 u2 :~: Char -> SUnit u1 -> SUnit u2 -> Void
-- g1a r _ _ = case r of {}   -- Why does this complain about missing Refl

{- [G]  F u1 u2 ~ Char
   [W] SBool (IsUnit u1) ~ SBool True
==>
   [W] IsUnit u1 ~ True
==> fundep
    u1 ~ ()


[G] F u1 u2 ~ Char =>(fundep)  [W] If (IsUnit u1) (Case u2) Int ~ Char
                   =>(fundep)  [W] IsUnit u1 ~ True
                               [W] Case u2 ~ Char   <<--  insoluble: no relevant eqns
-}

g2 :: F u1 u2 :~: Char
   -> SUnit u1 -> SUnit u2
   -> Void
g2 Refl su1 su2
  = case sIsUnit su1 of
      STrue ->
        case su2 of {}

