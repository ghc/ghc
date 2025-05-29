{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

import Numeric
import GHC.Prim
import GHC.Word
import GHC.IO
import GHC.Ptr
import Data.List
import qualified Data.ByteString as BS

foreign import prim "test" c_test :: Addr# -> State# RealWorld -> (# State# RealWorld, Word64# #)

main :: IO ()
main = do
    let bs = BS.pack $ take 100000 [ fromIntegral i | i <- [(1 :: Int) ..] ]
    n <- BS.useAsCString bs $ \(Ptr addr) -> IO $ \s ->
      case c_test addr s of (# s', n #) -> (# s', W64# n #)
    print $ showHex n ""
