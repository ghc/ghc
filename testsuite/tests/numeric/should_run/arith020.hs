{-# OPTIONS_GHC -O #-}

import Data.Word

-- Dividing a Word8 by 7 hits an edge case that was buggy.
-- Since the broken version made it into ghc twice now we have a test just for that.
prop :: Word8 -> IO ()
prop x = do
  print (x, x `quot` 7, x `rem` 7, x `quotRem` 7)
{-# NOINLINE prop #-}

main :: IO ()
main = prop 0
