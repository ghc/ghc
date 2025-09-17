
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}

{-# OPTIONS_GHC -fplugin=T26395_Plugin #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Winaccessible-code #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}

module T26395 where

import Data.Kind
import GHC.TypeNats
import GHC.Exts ( UnliftedType )

-- This test verifies that typechecker plugins are enabled
-- when we run the solver for pattern-match checking.

type Peano :: Nat -> UnliftedType
data Peano n where
  Z :: Peano 0
  S :: Peano n -> Peano (1 + n)

test1 :: Peano n -> Peano n -> Int
test1 Z      Z    = 0
test1 (S n) (S m) = 1 + test1 n m

{-
The following test doesn't work properly due to #26401:
the pattern-match checker reports a missing equation

  Z (S _) _

but there is no invocation of the solver of the form

  [G] n ~ 0
  [G] m ~ 1 + m1
  [G] (n-m) ~ m2

for which we could report the Givens as contradictory.

test2 :: Peano n -> Peano m -> Peano (n - m) -> Int
test2  Z     Z     Z    = 0
test2 (S _) (S _)  _    = 1
test2 (S _)  Z    (S _) = 2
-}
