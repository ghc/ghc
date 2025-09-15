{-# LANGUAGE GADTs, TypeOperators, PolyKinds #-}

module T16074 where

import GHC.Types

data a :~: b where Refl :: a :~: a

foo :: TYPE a :~: TYPE b
foo = Refl
