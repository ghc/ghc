{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
module T9263a where

import T9263b
import Data.Proxy

data Void

instance PEq ('KProxy :: KProxy Void)
