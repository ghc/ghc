{-# LANGUAGE TypeFamilies #-}

module T13092c_1 where

type family F a
type instance F Int = ()
