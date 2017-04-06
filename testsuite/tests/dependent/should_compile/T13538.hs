{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE KindSignatures, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications     #-}
module T13538 where

import GHC.TypeLits
import Data.Proxy

-- | Synonym for a type-level snoc (injective!)
type (ns :: [k]) +: (n :: k) = GetList1 (SinkFirst (n ': ns))
infixl 5 +:



-- | A weird data type used to make `(+:)` operation injective.
--   `List k [k]` must have at least two elements.
data List1 k = L1Single k | L1Head k [k]

-- | Sink first element of a list to the end of the list
type family SinkFirst (xs :: [k]) = (ys :: List1 k) | ys -> xs where
  SinkFirst '[y]       = 'L1Single y
  -- SinkFirst (y ': x ': xs :: [Nat])
  --     = ('L1Head x (GetList1Nat (SinkFirst (y ': xs))) :: List1 Nat)
  SinkFirst (y ': x ': xs :: [k])
      = ('L1Head x (GetList1    (SinkFirst (y ': xs))) :: List1 k)

type family GetList1 (ts :: List1 k) = (rs :: [k]) | rs -> ts where
  GetList1 ('L1Single x) = '[x]
  GetList1 ('L1Head y (x ':xs)) = y ': x ': xs
type family GetList1Nat (ts :: List1 Nat) = (rs :: [Nat]) | rs -> ts where
  GetList1Nat ('L1Single x) = '[x]
  GetList1Nat ('L1Head y (x ': xs)) = y ': x ': xs

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)


ff :: Proxy k -> Proxy (as +: k) -> Proxy (k ': bs) -> Proxy (as ++ bs)
ff _ _ _ = Proxy

yy :: Proxy '[3,7,2]
yy = ff (Proxy @5) (Proxy @'[3,7,5]) (Proxy @'[5,2])
