{-# LANGUAGE TypeFamilies, UndecidableInstances, PolyKinds, TypeOperators, DataKinds, MultiParamTypeClasses, GADTs #-}

-- | Doc for: module TypeFamilies
module TypeFamilies where

import qualified TypeFamilies2 as TF

-- | Doc for: data X
data X
  = X   -- ^ Doc for: X
  | XX  -- ^ Doc for: XX
  | XXX -- ^ Doc for: XXX

-- | Doc for: data Y
data Y

-- | Doc for: data Z
data Z = ZA | ZB

-- | Doc for: class Test a
class Test a

-- | Doc for: instance Test X
instance Test X
-- | Doc for: instance Test Y
instance Test Y

-- | Doc for: type family Foo a
type family Foo a :: k

-- | Doc for: type instance Foo X = Y
type instance Foo X = Y
-- | Doc for: type instance Foo Y = X
type instance Foo Y = X

-- | Doc for: data family Bat a
data family Bat (a :: k) :: *

-- | Doc for: data instance Bat X
data instance Bat X
  = BatX X -- ^ Doc for: BatX X
  | BatXX { aaa :: X , bbb :: Y } -- ^ Doc for: BatXX { ... }

-- | Doc for: data instance Bat Y
data instance Bat Y = BatY Y -- ^ Doc for: BatY Y

-- | Doc for: data instance Bat Z
data instance Bat (z :: Z) where
  BatZ1 :: Z -> Bat ZA
  BatZ2 :: { batx :: X, baty :: Y } -> Bat ZB

-- | Doc for: class Assoc a
class Assoc a where
  -- | Doc for: data AssocD a
  data AssocD a :: *
  -- | Doc for: type AssocT a
  type AssocT a :: *

-- | Doc for: instance Assoc X
instance Assoc X where
  -- | Doc for: data AssocD X = AssocX
  data AssocD X = AssocX -- ^ Doc for: AssocX
  -- | Doc for: type AssocT X = Foo X
  type AssocT X = Foo X

-- | Doc for: instance Assoc Y
instance Assoc Y where
  -- | Doc for: data AssocD Y = AssocY
  data AssocD Y = AssocY -- ^ Doc for: AssocY
  -- | Doc for: type AssocT Y = Bat Y
  type AssocT Y = Bat Y

-- | Doc for: type family Bar b
type family Bar b where
  Bar X = X
  Bar y = Y

type family (<>) (a :: k) (b :: k) :: k

type instance X <> a = X
type instance Y <> a = a

type instance XXX <> XX = 'X

class (><) (a :: k) (b :: k)
instance XX >< XXX

-- | External instance

type instance TF.Foo X = Y

data instance TF.Bar Y
