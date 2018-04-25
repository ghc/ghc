{-# LANGUAGE InstanceSigs, PartialTypeSignatures #-}

module T11670 where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr

peek :: Ptr a -> IO CLong
peek ptr = peekElemOff undefined 0 :: IO _

peek2 :: Ptr a -> IO CLong
peek2 ptr = peekElemOff undefined 0 :: _ => IO _

-- castPtr :: Ptr a -> Ptr b
-- peekElemOff :: Storable a => Ptr a -> Int -> IO a
