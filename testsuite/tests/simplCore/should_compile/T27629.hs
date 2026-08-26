{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin=T27629Plugin #-}

-- The plugin rewrites 'poly' to the example of
-- Note [Weird special case for SpecDict]:
--
--   poly = /\x. \t. split @T (mkD @x MkT) [t]
--
-- where mkD :: forall a. T -> C T is a new top-level binding
-- (mkD = /\a. \_. $fCT), so the tyvar x is free in the dictionary
-- *expression* but not in its *type* (C T).
module T27629 (poly, split, T (..), C (..)) where

import Data.Kind (Type)

data T = MkT

class C b where
  meth :: b -> b

instance C T where
  meth x = x

split :: C b => [b] -> [b]
split [] = []
split (x : xs) = meth x : split xs

poly :: forall (x :: Type). T -> [T]
poly t = [t]
