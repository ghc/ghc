{-# LANGUAGE MagicHash #-}

-- !!! Test top-level unboxed types

module ShouldFail where

import GHC.Base
import GHC.Prim

x = 1#

y :: Int#
y = x +# 1#

main =  let
          z = x -# y
        in
        if isTrue# (z ># 3#) then putStrLn "Yes"
                             else putStrLn "No"

