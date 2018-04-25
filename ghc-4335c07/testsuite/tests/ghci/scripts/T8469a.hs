{-# LANGUAGE TypeFamilies #-}

module T8469a () where

type family F a
type instance F Int = Bool
