{-# LANGUAGE TypeFamilies #-}
module T14396a where
import T14396f
import {-# SOURCE #-} T14396(S)
type instance F Int = S
