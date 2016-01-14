{-# LANGUAGE TypeFamilyDependencies, PolyKinds #-}

module T6018 where

-- these declarations use different type variables than the ones in the source
-- file but they should be accepted nevertheless

type family F d e f = (r :: k) | r -> d e f

type family FClosed (d :: *) (e :: *) (f :: *) = (r :: *) | r -> d e f where ..
