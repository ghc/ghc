{-# LANGUAGE DataKinds, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -O0 -freduction-depth=500 #-}

module T13386 where

import GHC.TypeLits

type DivisibleBy x y = Help x y 0 (CmpNat x 0)

type family Help x y z b where
  Help x y z EQ = True
  Help x y z LT = False
  Help x y z GT = Help x y (z+y) (CmpNat x z)

foo :: DivisibleBy y 3 ~ True => proxy y -> ()
foo _ = ()

type N = 1002

k = foo @N undefined
