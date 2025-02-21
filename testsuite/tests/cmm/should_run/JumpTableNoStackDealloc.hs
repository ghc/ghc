{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import Data.Foldable
import GHC.Exts
import GHC.Int
import System.IO

foreign import prim "foo" foo :: Int64# -> Int64#

main :: IO ()
main = for_ [0 .. 9] $ \(I64# x#) -> do
  let !res = I64# (foo x#)
  putStrLn $ "Result: " ++ show res
  hFlush stdout
