{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "Rts.h"

module GHC.Internal.InfoProv.Types
    ( InfoProv(..)
    , ipLoc
    , ipeProv
    , InfoProvEnt
    , peekInfoProv
    , getIPE
    , StgInfoTable
    , lookupIPE
    ) where

import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Real (fromIntegral)
import GHC.Internal.Word (Word32)
import GHC.Internal.Show (Show)
import GHC.Internal.Ptr (Ptr(..), plusPtr)
import GHC.Internal.Foreign.C.String.Encoding (CString, peekCString)
import GHC.Internal.Foreign.C.Types (CBool(..))
import GHC.Internal.Foreign.Marshal.Alloc (allocaBytes)
import GHC.Internal.IO.Encoding (utf8)
import GHC.Internal.Foreign.Storable (peekByteOff)
import GHC.Internal.ClosureTypes
import GHC.Internal.Prim (whereFrom##)

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: ClosureType,
  ipTyDesc :: String,
  ipLabel :: String,
  -- | @since base-4.20.0.0
  ipUnitId :: String,
  ipMod :: String,
  ipSrcFile :: String,
  ipSrcSpan :: String
} deriving (Eq, Show)

ipLoc :: InfoProv -> String
ipLoc ipe = ipSrcFile ipe ++ ":" ++ ipSrcSpan ipe

data InfoProvEnt

data StgInfoTable

foreign import ccall "lookupIPE" c_lookupIPE :: Ptr StgInfoTable -> Ptr InfoProvEnt -> IO CBool

lookupIPE :: Ptr StgInfoTable -> IO (Maybe InfoProv)
lookupIPE itbl = allocaBytes (#size InfoProvEnt) $ \p -> do
  res <- c_lookupIPE itbl p
  case res of
    1 -> Just `fmap` peekInfoProv (ipeProv p)
    _ -> return Nothing

getIPE :: a -> r -> (Ptr InfoProvEnt -> IO r) -> IO r
getIPE obj fail k = allocaBytes (#size InfoProvEnt) $ \p -> IO $ \s ->
  case whereFrom## obj (unPtr p) s of
    (## s', 1## ##) -> unIO (k p) s'
    (## s', _   ##) -> (## s', fail ##)
  where
    unPtr (Ptr p) = p

ipeProv :: Ptr InfoProvEnt -> Ptr InfoProv
ipeProv p = (#ptr InfoProvEnt, prov) p

peekIpDesc :: Ptr InfoProv -> IO Word32
peekIpDesc p    =  (# peek InfoProv, closure_desc) p

peekIpName, peekIpLabel, peekIpUnitId, peekIpModule, peekIpSrcFile, peekIpSrcSpan, peekIpTyDesc :: Ptr InfoProv -> IO CString
peekIpName p    =  (# peek InfoProv, table_name) p
peekIpLabel p   =  (# peek InfoProv, label) p
peekIpUnitId p  =  (# peek InfoProv, unit_id) p
peekIpModule p  =  (# peek InfoProv, module) p
peekIpSrcFile p =  (# peek InfoProv, src_file) p
peekIpSrcSpan p =  (# peek InfoProv, src_span) p
peekIpTyDesc p  =  (# peek InfoProv, ty_desc) p

peekInfoProv :: Ptr InfoProv -> IO InfoProv
peekInfoProv infop = do
  name <- peekCString utf8 =<< peekIpName infop
  desc <- peekIpDesc infop
  tyDesc <- peekCString utf8 =<< peekIpTyDesc infop
  label <- peekCString utf8 =<< peekIpLabel infop
  unit_id <- peekCString utf8 =<< peekIpUnitId infop
  mod <- peekCString utf8 =<< peekIpModule infop
  file <- peekCString utf8 =<< peekIpSrcFile infop
  span <- peekCString utf8 =<< peekIpSrcSpan infop
  return InfoProv {
      ipName = name,
      -- The INVALID_OBJECT case should be impossible as we
      -- control the C code generating these values.
      ipDesc = toEnum $ fromIntegral desc,
      ipTyDesc = tyDesc,
      ipLabel = label,
      ipUnitId = unit_id,
      ipMod = mod,
      ipSrcFile = file,
      ipSrcSpan = span
    }

