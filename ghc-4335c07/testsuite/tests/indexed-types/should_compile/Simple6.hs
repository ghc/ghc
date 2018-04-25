{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

import Data.IORef

data family T a
data instance T a = T

foo :: T Int -> T Char
foo T = T

type family S a
type instance S a = a

type family SMRef (m:: * -> *) :: * -> *
type instance SMRef IO = IORef 