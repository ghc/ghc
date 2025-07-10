{-# LANGUAGE TypeFamilies #-}

module A where

import {-# SOURCE #-} B

type family F a b
type instance F a b = b

