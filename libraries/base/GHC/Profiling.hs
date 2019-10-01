
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface, ExistentialQuantification #-}

module GHC.Profiling where

import Data.List
import Prelude (fromIntegral)
import Control.Monad ( forM )


import Control.Exception (evaluate)



import Foreign.C.String

import Foreign.C.Types

import Foreign.Marshal.Array

import Foreign.Ptr

import Foreign.StablePtr

import Foreign.Storable


import System.IO
import System.Mem

import Unsafe.Coerce

-- | @since 4.7.0.0

import GHC.Base

-- | Stop attributing ticks to cost centres. Allocations will still be
-- attributed.
--
-- @since 4.7.0.0
foreign import ccall stopProfTimer :: IO ()

-- | Start attributing ticks to cost centres. This is called by the RTS on
-- startup.
--
-- @since 4.7.0.0
foreign import ccall startProfTimer :: IO ()

#if defined(PROFILING)
foreign import ccall unsafe "setRootProfPtrs" c_setRootProfPtrs
    :: CInt -> Ptr (StablePtr a) -> Ptr CString -> IO ()

foreign import ccall "&g_rootProfileDebugLevel" g_rootProfileDebugLevel
    :: Ptr CInt

data Root = forall a. Root
    { rootDescr   :: String
    , rootClosure :: a
    }


setHeapRoots :: [Root] -> IO ()
setHeapRoots xs = do
    descs <- mapM (newCString . rootDescr) xs
    sps   <- forM xs $ \(Root _ a) ->
      newStablePtr =<< evaluate (unsafeCoerce a :: a)
    withArray descs $ \descs_arr ->
      withArray sps $ \sps_arr ->
        c_setRootProfPtrs (fromIntegral (length xs)) sps_arr descs_arr

#endif


