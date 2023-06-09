{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module LinearLet where

import GHC.Types

-- See Note [Non-variable pattern bindings aren't linear] in GHC.Tc.Gen.Bind

-- In k, the type-checker, assigns a polymorphic type to `f`. This doesn't
-- admit a linear desugaring.
k :: a -> (a, Bool)
k x = let %1 !(f, b) = ((\y -> y), b) in (f x, b)

-- i could, in principled, be desugared linearly. Because it doesn't, in fact,
-- have polymorphic variables. But it's still inferred as an AbsBinds. It looks
-- difficult to avoid, but equally difficult to specify.
i :: (a, b) %1 -> (b, a)
i x = let !(y, z) = x in (z, y)


-- f is given a monomorphic type because of the multiplicity annotation. Because
-- the multiplicity annotation is %Many, f could be inferred to have a
-- polymorphic type: it wouldn't endanger type safety. If this behaviour
-- changes, the documentation should change to reflect the new behaviour.
j :: (Int, Bool)
j = let %Many !(Just f) = Just (\x -> x) in (f (0::Int), f True)
