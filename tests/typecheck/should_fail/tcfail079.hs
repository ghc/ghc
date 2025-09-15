{-# LANGUAGE MagicHash #-}

module ShouldFail where

-- !!! unboxed field in newtype declaration

import GHC.Exts ( Int# )

newtype Unboxed = Unboxed Int#

f = [ Unboxed 1#, Unboxed 2# ] -- shouldn't be allowed!
