{-# LANGUAGE TypeFamilies #-}
module T13981B where
import {-# SOURCE #-} T13981A
import T13981F
type instance F T = Int
