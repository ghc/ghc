{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Bug where

import Data.Kind (Constraint, Type)

data D a

f :: Generic (D a) => ()
f = ()

type family
  AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF _c '[]       = ()
  AllF  c (x ': xs) = (c x, All c xs)

class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k]) where

type SListI = All Top

class Top x
instance Top x

class All SListI (Code a) => Generic a where
  type Code a :: [[Type]]


{-
  final wc = WC {wc_impl =
                   Implic {
                     TcLevel = 1
                     Skolems = k_aG9[sk:1] (a_aGa[sk:1] :: k_aG9[sk:1])
                     Given-eqs = LocalGivenEqs
                     Status = Unsolved
                     Given = $dGeneric_aGb :: Generic (D a_aGa[sk:1])
                     Wanted =
                       WC {wc_simple =
                             [W] irred_aGo {0}:: AllF
                                                   SListI (Code (D a_aGe[tau:1])) (CIrredCan(irred))
                             [W] irred_aGu {0}:: AllF
                                                   Top (Code (D a_aGe[tau:1])) (CIrredCan(irred))
                             [W] $dGeneric_aGf {0}:: Generic (D a_aGe[tau:1]) (CDictCan)
                             [W] $dAll_aGn {0}:: All SListI (Code (D a_aGe[tau:1])) (CDictCan)
                             [W] $dAll_aGv {0}:: All
                                                   Top (Code (D a_aGe[tau:1])) (CDictCan(psc))}
                     Binds = EvBindsVar<aGg>
                     the type signature for:
                       f :: forall {k} (a :: k). Generic (D a) => () }}



--------------------------
-- Given
[G] Generic (D a)
==> superclass
[G] All SListI (Code (D a))   =  All (All Top) (Code (D a))
==> superclass
[G] AllF SLIstI (Code (D a))  =  AllF (All Top) (Code (D a))
[G] SListI (Code (D a))       =  All Top (Code (D a))    {loop}

Next iteration
===>
[G] AllF Top (Code (D a))
[G] SListI (Code (D a))       = All Top (Code (D a))  (already there)

--------------------------
-- Wanted
[W] Generic (D a)
==> superclass
[W] All SListI (Code (D a))   =  All (All Top) (Code (D a))
==> superclass
[W] AllF SLIstI (Code (D a))  =  AllF (All Top) (Code (D a))
[W] SListI (Code (D a))       =  All Top (Code (D a))    {loop}

Next iteration
===>
[W] AllF Top (Code (D a))
[W] SListI (Code (D a))       = All Top (Code (D a))  (already there)

-}