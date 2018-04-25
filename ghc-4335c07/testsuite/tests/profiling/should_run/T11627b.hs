{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}


-- A reduced test case for #11627


import GHC.Prim
import GHC.Types (Int(..),IO(..))
import System.Mem


main :: IO ()
main = do
    -- Allocate a large object (size >= 8/10 of one block = 8/10 * 4096 B)
    let nBytes = 123 * 4096
    b <- newBlob nBytes

    -- Shrink it by at least one word
    let delta = 100
    shrinkBlob b $ nBytes - delta

    -- Perform a heap census (assumes we are running with -i0, so a census is
    -- run after every GC)
    performGC

    -- Hold on to b so it is not GCed before the census
    shrinkBlob b $ nBytes - delta

------------------------------------------------------------------------------

data Blob = Blob# !(MutableByteArray# RealWorld)

newBlob :: Int -> IO Blob
newBlob (I# n#) =
    IO $ \s -> case newByteArray# n# s of
                   (# s', mba# #) -> (# s', Blob# mba# #)

shrinkBlob :: Blob -> Int -> IO ()
shrinkBlob (Blob# mba#) (I# n#) =
    IO $ \s -> case shrinkMutableByteArray# mba# n# s of
                   s' -> (# s', () #)
