{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Exception
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import GHC.IO.FD
import System.Exit

#ifdef _WIN32
#include <winsock.h>
#else
#include <sys/socket.h>
#endif

aF_INET :: CInt
aF_INET = #const AF_INET

sOCK_STREAM :: CInt
sOCK_STREAM = #const SOCK_STREAM

main :: IO ()
main = do
#ifdef _WIN32
  void $ initWinSock
#endif
  sock <- c_socket aF_INET sOCK_STREAM 0
  let fd = FD sock 1
  res <- try $ allocaBytes 1024 (\ptr -> readRawBufferPtr "T12010" fd ptr 0 1024)
  case res of
    Left (_ex :: IOException) -> exitSuccess
    Right res'                -> print res' >> exitFailure

foreign import stdcall unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt

#ifdef _WIN32
foreign import ccall unsafe "initWinSock" initWinSock :: IO Int
#endif
