{-# LANGUAGE NoImplicitPrelude #-}

-- | Primitive panics.
module GHC.Prim.Panic
   ( absentSumFieldError
   , panicError
   , absentError
   )
where

import GHC.Magic

default () -- Double and Integer aren't available yet
