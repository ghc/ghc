{-# LANGUAGE TypeFamilies #-}
module M2 where

data family TD a

data instance TD () = TDI
