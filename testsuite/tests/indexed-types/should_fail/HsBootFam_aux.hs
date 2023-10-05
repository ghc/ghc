{-# LANGUAGE TypeFamilies #-}

module HsBootFam_aux where

type family F a
type instance F Int = Char
