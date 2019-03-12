{-# LANGUAGE TypeFamilies, PolyKinds, NamedWildCards #-}

-- See #10982

module UnusedTyVarWarningsNamedWCs where

type family C a b where
  C a b = a                 -- should warn

type family C2 a b
type instance C2 a b = a    -- should warn

type family D a b where
  D a _b = a                -- should not warn

type family D2 a b
type instance D2 a _b = a   -- should not warn

type family E a b where
  E a _ = a                 -- should not warn

type family E2 a b
type instance E2 a _ = a    -- should not warn

type family X a b where
  X a a = Int               -- a is considered used, do not warn
  X a Int = Bool            -- here a is unused

type family Y a b c where
  Y a b b = a               -- b is used, do no warn

data family I a b c
data instance I a b c = IDC1 a | IDC2 c  -- should warn

data family J a b
data instance J a _b = JDC a  -- should not warn

data family K a b
data instance K a _ = KDC a   -- should not warn
