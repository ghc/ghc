{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts

{-# NOINLINE up #-}
up :: Int# -> Int#
up x = x +# 1#

{-# NOINLINE down #-}
down :: Int# -> Int#
down x = x -# 1#

{-# RULES "up/down" forall x. up (down x) = 42# #-}

main = do
    print (I# (up (down 1#)))
