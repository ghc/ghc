{-# LANGUAGE BangPatterns #-}
module T6078 where

import GHC.Ptr
import Foreign

byteStringSlice len = \fpbuf ip0 ipe s0 ->
     let ip1p@(Ptr ip1) = Ptr ip0 `plusPtr` len
     -- Note that the panic goes away if we use a bang-pattern as follows
     -- let !ip1p@(Ptr ip1) = Ptr ip0 `plusPtr` len
     in  ip1p
