{-# LANGUAGE GADTs, RequiredTypeArguments, PatternSynonyms, TypeAbstractions #-}
module T27583g where

data T a b c where
  MkT :: forall a. forall b c -> a -> T a b c

-- Check all argument variants in a constructor pattern at once: the invisible
-- type argument `@a`, the required type argument `type b` with the herald, the
-- required type argument `c` without the herald, and the value argument `x`.
--
-- The resulting builder is $bP x = MkT _ _ x.

pattern P :: x -> T x y z
pattern P x = MkT @a (type b) c x
