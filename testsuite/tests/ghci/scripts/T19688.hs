{-# LANGUAGE TypeFamilies #-}
module T19688 where

data family D (a :: k)
data instance D Int = DInt
