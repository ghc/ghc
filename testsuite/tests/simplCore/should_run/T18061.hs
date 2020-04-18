module T18061 where

import Control.Concurrent
import Control.Monad
import Data.Word
import Foreign.Storable
import Foreign.ForeignPtr
import Numeric

main :: IO ()
main = do
  replicateM_ 49 $ threadDelay 1
  fptr <- mallocForeignPtrBytes 4
  withForeignPtr fptr $ \p ->
    forever $ do
      poke p (0xDEADBEEF :: Word32)
      threadDelay 10
      x <- peek p
      unless (x == 0xDEADBEEF) $ putStrLn (showHex x "")
