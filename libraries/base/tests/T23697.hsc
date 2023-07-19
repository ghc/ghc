{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}

module Main (main) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import GHC.Ptr
import System.Posix.Internals
import Data.Bits

#include<fcntl.h>

main :: IO ()
main = do

  let 
    checkErr :: (Num a, Eq a) => a -> a
    checkErr = \case
        (-1) -> error "Returned (-1)"
        x    -> x

    fname = "foo"

  -- test: openat(AT_FDCWD...), write, close
  withCString fname $ \cstr -> do
    fd <- checkErr <$> openat (#const AT_FDCWD) cstr (o_WRONLY .|. o_CREAT) 0o666
    checkErr <$> write fd (Ptr "123456"##) 6
    checkErr <$> close fd

  putStrLn =<< readFile fname

foreign import ccall openat :: CInt -> CString -> CInt -> CUInt -> IO CInt
foreign import ccall write  :: CInt -> Ptr ()  -> CSize -> IO CInt
foreign import ccall close  :: CInt -> IO CInt
