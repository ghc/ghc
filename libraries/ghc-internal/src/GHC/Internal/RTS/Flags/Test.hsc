{-# LANGUAGE NoImplicitPrelude #-}

-- | Module with fewer dependencies than GHC.Internal.RTS.Flags
-- that allows to quickly test if some flag is set.
module GHC.Internal.RTS.Flags.Test
  ( getUserEventTracingEnabled
  , getNumIoWorkerThreads
  )
where

import GHC.Internal.Ptr
import GHC.Internal.Foreign.Storable
import GHC.Internal.Data.Functor ((<$>))
import GHC.Internal.Types (Bool(..), Int, IO)
import GHC.Internal.Word (Word32)
import GHC.Internal.Real (fromIntegral)

#if defined(javascript_HOST_ARCH)
import GHC.Internal.Base (pure)
#else
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.Marshal.Utils
#endif
import GHC.Internal.Num( fromInteger )  -- For known-key names

#include "Rts.h"
#include "rts/Flags.h"

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr ()

-- | Specialized version of 'getTraceFlags' for just checking if user
-- event tracing is enabled.
getUserEventTracingEnabled :: IO Bool
getUserEventTracingEnabled = do
#if defined(javascript_HOST_ARCH)
  -- The JS backend does not currently have trace flags
  pure False
#else
  let ptr = (#ptr RTS_FLAGS, TraceFlags) rtsFlagsPtr
  toBool <$> (#{peek TRACE_FLAGS, user} ptr :: IO CBool)
#endif

-- | Specialized version of 'getMiscFlags' for just checking the number of IO worker threads
getNumIoWorkerThreads :: IO Int
getNumIoWorkerThreads = do
  let ptr = (#ptr RTS_FLAGS, MiscFlags) rtsFlagsPtr
  fromIntegral <$> (#{peek MISC_FLAGS, numIoWorkerThreads} ptr :: IO Word32)
