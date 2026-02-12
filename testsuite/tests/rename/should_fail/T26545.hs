{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StaticPointers     #-}

module Foo where

import GHC.Exts

f :: Int# -> Int#
f 0# = 0#
f n# = f (n# -# 1#)

h x = let v = f 3# in static (I# (v +# 1#))
