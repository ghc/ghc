{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Main (main) where

import GHC.IO (IO(..))
import GHC.Exts
import Control.Monad

data MutableByteArray = MutableByteArray (MutableByteArray## RealWorld)

main :: IO ()
main = do
  MutableByteArray x <- allocateBytes
  -- Hard coding the expected closure type. Does anyone know
  -- a way to use hsc2hs to get this value. If not, then
  -- this test is in the wrong place since it doesn't
  -- actually using any of hsc2hs's features.
  let expected = 42
  let actual = W## (getClosureType## (unsafeCoerce## x))
  when (expected /= actual) $ fail
    ("Expected closure type " ++ show expected ++ " but got " ++ show actual)

allocateBytes :: IO MutableByteArray
allocateBytes = IO $ \s0 -> case newByteArray## 1## s0 of
  (## s1, arr ##) -> (## s1, MutableByteArray arr ##)

