{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O2 #-}

-- This gave a Lint crash

module T23209 where

import T23209_Aux

f a = let w = if a then Allocator (ArrayWriter s)
                   else Allocator (ArrayWriter e)
      in case combine w w of
