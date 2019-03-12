{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- #11562 reported an ASSERT error
-- It only showed up /without/ -O, and obviously
-- with a compiler built with -DDEBUG

module T11562 where

import Data.Kind

class Category (cat :: k -> k -> Type) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

data Dict :: Constraint -> Type where
  Dict :: a => Dict a

newtype C2D a b = Sub (a => Dict b)

instance Category C2D where
  id  = Sub Dict
  f . g = Sub (sub (sub Dict f) g)

sub :: a => (b => r) -> (C2D a b) -> r
sub r (Sub Dict) = r

{-
$ inplace/bin/ghc-stage2 -fforce-recomp -c C.hs -O0

WARNING: file compiler/stgSyn/CoreToStg.hs, line 250
  $fCategoryConstraint:- True False
-}
