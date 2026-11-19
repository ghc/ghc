{-# LANGUAGE ExplicitNamespaces #-}
module NativeTypeCaseHeadProvider (T(..), D(..), type C, Forget) where

data T = T
data D = C
class C a
type Forget a = a
