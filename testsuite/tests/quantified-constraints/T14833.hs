{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module T14833 where

data Dict c where
  Dict :: c => Dict c

class    (a => b) => Implies a b
instance (a => b) => Implies a b

-- Works ok
iota1 :: (() => a) => Dict a
iota1 = Dict

iota2 :: Implies () a => Dict a
iota2 = Dict

{-
[G] Implies () a
[G] (() => a)      -- By superclass

[W] a
-}