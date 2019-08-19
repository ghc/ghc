{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Monad (unless)
import GHC.Exts
import GHC.Types

-- This test is nearly a copy of T11296. In T11296, it is
-- shrinkMutableByteArray# that is tested. Here, it is
-- shrinkSmallMutableArray# that is tested.

data SmallArray = SA (SmallMutableArray# RealWorld Integer)

main :: IO ()
main = do
    let element = 42 :: Integer
    ba# <- IO (\s0 -> case newSmallArray# 256# element s0 of
                        (# s1, ba# #) -> (# s1, SA ba# #))
    let go n = do
            shrink ba# n
            sz <- getSize ba#
            unless (sz == n) $ print (sz, n)
    mapM go [128, 64, 63, 32, 2, 1]
    return ()

shrink :: SmallArray -> Int -> IO ()
shrink (SA ba#) (I# n#) = IO (\s ->
    case shrinkSmallMutableArray# ba# n# s of
      s' -> (# s', () #))

getSize :: SmallArray -> IO Int
getSize (SA ba#) = IO (\s ->
    case getSizeofSmallMutableArray# ba# s of
      (# s', n# #) -> (# s', I# n# #))

