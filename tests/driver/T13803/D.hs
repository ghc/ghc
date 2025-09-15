{-# LANGUAGE TypeFamilies #-}
module D (D) where

type family D a
type instance D Int = Int
