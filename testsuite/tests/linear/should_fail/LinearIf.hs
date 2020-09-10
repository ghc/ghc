{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module LinearIf where

import Prelude (Bool(..), Char)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

f :: Bool %1 -> Char %1 -> Char %1 -> Char
f b x y = if b then x else y
  -- 'f' ought to be unrestricted in all three arguments because it desugars to
  -- > ifThenElse b x y
