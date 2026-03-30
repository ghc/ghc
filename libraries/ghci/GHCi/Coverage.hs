{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module GHCi.Coverage ( hpcAddModule ) where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Word
import Foreign
import GHC.Fingerprint
import GHCi.RemoteTypes
import Data.ByteString
import GHC.Foreign (CString)
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BS8
import GHCi.ObjLink (lookupSymbol)
import Debug.Trace

-- | Used by GHCi to add an SPT entry for a set of interactive bindings.
hpcAddModule :: ByteString -> IO ()
hpcAddModule init_label = do
  lookupSymbol (BS8.unpack init_label) >>= \ case
    Nothing -> pure ()
    Just tickBoxRef -> do
      invokeFunc $ castPtrToFunPtr tickBoxRef
      hpc_startup

foreign import ccall "dynamic"
    invokeFunc :: FunPtr (IO ()) -> IO ()

-- foreign import ccall "hs_hpc_module"
--     hpc_register_module :: CString -> Word32 -> Word32 -> Ptr Word64 -> IO ()

foreign import ccall "startupHpc"
    hpc_startup :: IO ()
