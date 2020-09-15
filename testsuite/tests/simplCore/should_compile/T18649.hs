{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Test where

import Prelude

type Hidden a =
  ( ?enable :: a
  , Eq a  -- removing this "fixes" the issue
  )

{-# NOINLINE a #-}
a :: Hidden Bool => Integer -> Bool
a _ = ?enable

system :: Hidden Bool => Bool
system = a 0

topEntity :: Bool -> Bool
topEntity ena = let ?enable = ena
                in system

someVar = let ?enable = True
          in system
