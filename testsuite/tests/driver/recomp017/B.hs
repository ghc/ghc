{-# LANGUAGE TypeFamilies #-}
module B where
import A
data B
type instance F (B,b) = ()
b :: () -> F (B,b)
b = id
