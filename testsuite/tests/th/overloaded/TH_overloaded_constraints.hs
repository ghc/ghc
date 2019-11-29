{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_overloaded_constraints where
-- Test that constraints are collected properly from nested splices

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Functor.Identity

class C m where
  cid :: m a -> m a

class D m where
  did :: m a -> m a

cq :: (C m, Quote m) => m Exp
cq = [| 5 |]

dq :: (D m, Quote m) => m Exp
dq = [| 5 |]

top_level :: (C m, D m, Quote m) => m Exp
top_level = [| $cq + $dq |]

