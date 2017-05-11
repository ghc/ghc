{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module T13677 where

import GHC.Exts (Constraint)

data Dict a where
  Dict :: a => Dict a

foo :: Dict (Int ~ Int) => Int
foo = undefined
