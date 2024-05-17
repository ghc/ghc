{-# LANGUAGE Haskell2010 #-}
module OrphanInstances where

import OrphanInstancesType
import OrphanInstancesClass

-- | This is an orphan instance.
instance AClass AType where
  aClass (AType n) = n
