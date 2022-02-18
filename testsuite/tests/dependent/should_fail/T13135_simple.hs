{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T13135_simple where

import Data.Kind (Type)
import GHC.Exts ( RuntimeRep(BoxedRep), Levity(Lifted) )

{-
NB: All the type applications and explicit quantification
aren't necessary for the error
to trigger, but they are helpful in reading this code.
-}

type Sig :: Type -> Type
data Sig a = SigFun a (Sig a)
-- SigFun :: forall (a :: Type). a -> Sig a -> Sig a

type SmartFun :: forall (t :: Type). Sig t -> Type
type family SmartFun sig = r | r -> sig where
  forall (a :: Type) (sig :: Sig Type).
    SmartFun @Type (SigFun @Type a sig) = a -> SmartFun @Type sig

smartSym :: forall (k :: Type) (sig :: Sig k). SmartFun @k sig
smartSym = error @(BoxedRep Lifted) @(SmartFun @k sig) "urk"

problem :: Int -> Bool
problem = smartSym

{-
problem arises in RHS of `problem`:

instantiate smartSym:
  kappa :: Type
  sigma :: Sig kappa
  smartSym :: SmartFun @kappa sigma

[W] w1 :: SmartFun @kappa sigma ~ Int -> Bool

injective type family says:
beta :: Sig Type
[W] w2 :: Type ~ kappa
[W] w3 :: sigma ~ SigFun @Type Int beta

then we have
inert: [W] w1 :: SmartFun @kappa sigma ~ Int -> Bool
work list: [W] w3 :: (SigFun @Type Int beta :: Sig Type) ~ (sigma :: Sig kappa)  (current item)
           [W] w2 :: Type ~ kappa

this is a hetero equality. So we get
[W] w4 :: Type ~ kappa
[W] w5 :: sigma ~ (SigFun @Type Int beta |> Sig w4)
w3 is solved
we cannot solve w5 by unification because of the coercion hole w4, so w5 becomes an inert,
  kicking out w1

next step.
inert: [W] w5 :: sigma ~ (SigFun @Type Int beta |> Sig w4)
work list: [W] w1 :: SmartFun @kappa sigma ~ Int -> Bool  (current item)
           [W] w4 :: Type ~ kappa
           [W] w2 :: Type ~ kappa

LHS of w1 is rewritten, making it become
  [W] w1 :: SmartFun @kappa (SigFun @Type Int beta |> Sig w4) ~ Int -> Bool

injective type family says:
gamma :: Sig Type
[W] w6 :: Type ~ kappa
[W] w7 :: (SigFun @Type Int gamma :: Sig Type)
            ~
          ((SigFun @Type Int beta |> Sig w4) :: Sig kappa)

next step.
inert: [W] w5 :: sigma ~ (SigFun @Type Int beta |> Sig w4)
       [W] w1 :: SmartFun @kappa (SigFun @Type int beta |> Sig w4) ~ Int -> Bool
work list: [W] w7 :: SigFun @Type Int gamma ~ (SigFun @Type Int beta |> Sig w4)  (current item)
           [W] w6 :: Type ~ kappa
           [W] w4 :: Type ~ kappa
           [W] w2 :: Type ~ kappa

we drop the cast on the work item, to get
  [W] w7 :: SigFun @Type Int gamma ~ SigFun @Type Int beta
this decomposes to
  [W] w8 :: beta ~ gamma
which gets added to the work list

next step.
inert: [W] w5 :: sigma ~ (SigFun @Type Int beta |> Sig w4)
       [W] w1 :: SmartFun @kappa (SigFun @Type int beta |> Sig w4) ~ Int -> Bool
work list: [W] w8 :: beta ~ gamma  (current item)
           [W] w6 :: Type ~ kappa
           [W] w4 :: Type ~ kappa
           [W] w2 :: Type ~ kappa

unify beta := gamma
kick out w5 and w1

next step.
inert: none
work list: [W] w5 :: sigma ~ (SigFun @Type Int beta |> Sig w4)   (current item)
           [W] w1 :: SmartFun @kappa (SigFun @Type Int beta |> Sig w4) ~ Int -> Bool
           [W] w6 :: Type ~ kappa
           [W] w4 :: Type ~ kappa
           [W] w2 :: Type ~ kappa

w5 still cannot be solved by unification, and then when we see w1 again, we spit out
new type-family injectivity equalities... and around and around we go.

-}
