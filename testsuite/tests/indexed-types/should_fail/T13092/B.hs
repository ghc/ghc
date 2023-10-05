{-# LANGUAGE TypeFamilies #-}
module B (A, X) where
import A
data X
type instance A (X, b) = ()
