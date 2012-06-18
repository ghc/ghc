{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}

module T6152 where

data U = Unit

data SU (a :: U) where
   SInt :: SU Unit

type family I (a :: U) :: U
type instance I Unit = Unit

type SUI a = SU (I a)

