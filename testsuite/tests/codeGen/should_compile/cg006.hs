{-# LANGUAGE MagicHash #-}
module ShouldCompile where

-- exposed a bug in the NCG in 6.4.2
import GHC.Base
class Unboxable a where
    writeUnboxable :: MutableByteArray# RealWorld -> a -> State# RealWorld -> State# RealWorld
    writeUnboxable arr a s  =  writeInt8Array# arr 0# (getTag 0) s
