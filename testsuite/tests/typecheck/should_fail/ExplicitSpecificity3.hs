{-# LANGUAGE RankNTypes, PolyKinds, TypeFamilies #-}

module ExplicitSpecificity3 where

type family F {k} (a::k) :: *
type instance F String = Int

