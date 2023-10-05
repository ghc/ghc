{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module LinearLet where

-- Tests behaviour of linear lets in presence of -XNoMonoLocalBinds. These are a
-- little bit of a corner case, as it's recommended to use -XMonoLocalBinds with
-- -XLinearTypes (indeed LinearTypes implies MonoLocalBinds).

-- Because the multiplicity annotation on the let, `y` is given a monomorphic
-- type. Otherwise it would run afoul of Note [Non-variable pattern bindings aren't linear] in GHC.Tc.Gen.Bind
f :: Maybe a %1 -> a
f x = let %1 !(Just y) = x in y

-- Variable pattern bindings can be generalised.
g :: Bool -> Int
g b =
  case b of
  True -> f 0
  False -> f (\x -> x) 1
  where
    %1 f = (\x -> x)
