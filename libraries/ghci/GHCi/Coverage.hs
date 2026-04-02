{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module GHCi.Coverage (
  hpcAddModule,
  ) where

import Prelude -- See note [Why do we import Prelude here?]

import Control.Exception
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Word
import Foreign
import GHC.Foreign (CString)
import GHC.Utils.Encoding.UTF8 (utf8DecodeShortByteString)
import GHCi.ObjLink (lookupSymbol)

-- | Inform the run-time system that the given module name is instrumented via @hpc@
-- and to collect @.tix@ info.
--
-- Starts the `hpc` run-time if it hasn't already been started.
hpcAddModule ::
  ShortByteString ->
  -- ^ Name of the module to instrument
  Int ->
  -- ^ Number of hpc ticks in this module
  Int ->
  -- ^ 'HpcInfo's 'hpcInfoHash'
  ShortByteString ->
  -- ^ Name of the ticks array found in the c-stub.
  IO ()
hpcAddModule modlName ticks hash tickboxes = do
  SBS.useAsCString modlName $ \modlNameLiteral -> do
    -- we need to find the reference to the ticks array.
    lookupSymbol tickboxes >>= \ case
      Nothing -> do
        -- the symbol is not found, this is a bug!
        throwIO $ ErrorCall $ "hpcAddModule: failed to find symbol " <> utf8DecodeShortByteString tickboxes
      Just tickBoxRef -> do
        -- Calling 'hs_hpc_module' multiple times is safe, it will add the module only once.
        hpc_register_module modlNameLiteral (fromIntegral ticks) (fromIntegral hash) (castPtr tickBoxRef)
        -- calling 'hpc_startup' multiple times is safe, it will only be initialised once.
        hpc_startup

foreign import ccall unsafe "hs_hpc_module"
    hpc_register_module :: CString -> Word32 -> Word32 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "startupHpc"
    hpc_startup :: IO ()
