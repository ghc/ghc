{-# LANGUAGE MagicHash #-}

-- Should be compiled with -O0

import Control.Concurrent
import GHC.Conc
import GHC.Prim
import GHC.Exts

main = do
    t <- forkIO (f 0 `seq` return ())
    threadDelay 10
    killThread t
    putStrLn "Done"

-- Non-allocating let-no-escape infinite loop in fail
{-# NOINLINE f #-}
f :: Int -> Bool
f i@(I# j) = let fail :: Int# -> Bool
                 fail i = fail (i +# 1#)
      in if (case i of
            0 -> True
            _ -> False) then fail j else False
