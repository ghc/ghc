{-# LANGUAGE RankNTypes, PolyKinds, TypeFamilies #-}

module ExplicitSpecificity4 where

type family F {k} (a::k) :: *
type instance F String = Int

