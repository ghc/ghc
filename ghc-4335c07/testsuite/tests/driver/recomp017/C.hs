{-# LANGUAGE TypeFamilies #-}
module C where
import A
data C
type instance F (a,C) = ()
