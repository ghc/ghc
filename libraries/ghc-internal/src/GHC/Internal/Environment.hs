{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module GHC.Internal.Environment (getFullArgs) where

import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.C.String
import GHC.Internal.Foreign.C.String.Encoding as GHC
import GHC.Internal.Foreign.Marshal.Alloc
import GHC.Internal.Foreign.Marshal.Array
import GHC.Internal.Foreign.Storable
import GHC.Internal.Ptr
import GHC.Internal.Base
import GHC.Internal.Real ( fromIntegral )
import GHC.Internal.IO.Encoding

-- | Computation 'getFullArgs' is the "raw" version of
-- 'GHC.Internal.System.Environment.getArgs', similar to @argv@ in other languages. It
-- returns a list of the program's command line arguments, starting with the
-- program name, and including those normally eaten by the RTS (+RTS ... -RTS).
getFullArgs :: IO [String]
getFullArgs =
  alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        getFullProgArgv p_argc p_argv
        p    <- fromIntegral `liftM` peek p_argc
        argv <- peek p_argv
        enc <- argvEncoding
        peekArray p argv >>= mapM (GHC.peekCString enc)

foreign import ccall unsafe "getFullProgArgv"
    getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
