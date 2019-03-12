-- From comment:76 in #9858
-- This exploit still works in GHC 7.10.1.
-- By Shachaf Ben-Kiki, Ørjan Johansen and Nathan van Doorn

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module T9858a where

import Data.Typeable

type E = (:~:)
type PX = Proxy (((),()) => ())
type PY = Proxy (() -> () -> ())

data family F p a b

newtype instance F a b PX = ID (a -> a)
newtype instance F a b PY = UC (a -> b)

{-# NOINLINE ecast #-}
ecast :: E p q -> f p -> f q
ecast Refl = id

supercast :: F a b PX -> F a b PY
supercast = case cast e of
    Just e' -> ecast e'
  where
    e = Refl
    e :: E PX PX

uc :: a -> b
uc = case supercast (ID id) of UC f -> f
