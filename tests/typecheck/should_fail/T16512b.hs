{-# LANGUAGE TypeFamilyDependencies #-}

module T16512b where

type family G a = r | r -> a where
  G [a] = [G a]

-- this needs -XUndecidableInstances.
-- See Note [Coverage condition for injective type families] in GHC.Tc.Instance.Family
