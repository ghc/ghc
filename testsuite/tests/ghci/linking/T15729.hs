module T15729 (main) where

import Foreign
import Foreign.C

foreign import ccall unsafe "readBss"
  readBss :: Int -> IO Int

main :: IO ()
main = do
  prefix <- mapM readBss [0 .. 10]
  print prefix
  samples <- mapM readBss [0, 19 .. bit 20 - 1]
  print $ foldr1 (.|.) samples
