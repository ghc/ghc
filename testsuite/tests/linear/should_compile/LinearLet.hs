{-# LANGUAGE LinearTypes #-}

module LinearLet where

import GHC.Types

f :: a -> a
f x = let %1 y = x in y

f' :: a %1 -> a
f' x = let %1 y = x in y

f'' :: a %1 -> a
f'' x = let y = x in y

g :: a -> (a, a, a)
g x = let %'Many y = x in (x, x, x)

h :: a %1 -> a
h x = y
  where %1 y = x

h' :: a %1 -> a
h' x = y
  where y = x

i :: Maybe a %1 -> a
i x = let %1 !(Just y) = x in y

j :: (a, b) %1 -> (b, a)
j x = let !(y, z) = x in (z, y)

-- The non-variable pattern let binding cannot be linear because it's inferred as
-- polymorphic. See Note [Non-variable pattern bindings aren't linear] in GHC.Tc.Gen.Bind
--
-- The local let binding is inferred as polymorphic despite MonoLocalBinds
-- because it is closed. This behaviour is not very problematic (this will never
-- need to be linear), but can be a little surprising.
k :: a -> (a, Bool)
k x = let !(f, b) = ((\y -> y), b) in (f x, b)

-- let !y = (without a multiplicity annotation) uses the same code path as let y =
-- (FunBind) not that of pattern bindings (PatBind). The desugaring of a strict
-- pattern involves `seq`, with which we need to be careful when dealing with
-- linear types.
l :: a %1 -> a
l x = let !y = x in y
