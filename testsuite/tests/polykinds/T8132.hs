{-# LANGUAGE MagicHash #-}
import Data.Typeable

data K = K

-- This used to have a RHS but now we hide typeRep#
instance Typeable K -- where typeRep# _ = undefined
