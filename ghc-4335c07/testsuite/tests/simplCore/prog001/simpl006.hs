-- !!! This one sent 4.06 into an infinite loop

-- But it worked ok if Simpl006Help.forever is 
-- defined in this module.  I have no idea why!

{-# OPTIONS -O #-}

module ShouldCompile where

import Control.Concurrent 
import Simpl006Help

after :: Int -> IO a -> IO a
after d c = c

every :: Int -> IO a -> IO ()
every d c = forever (after d c)


