{-# Language DeriveGeneric, MagicHash #-}

import GHC.Generics
import GHC.Prim

data Array a = Array (Array# a) deriving Generic1
newtype Vec a = MkVec {unVec :: Array a} deriving Generic1
