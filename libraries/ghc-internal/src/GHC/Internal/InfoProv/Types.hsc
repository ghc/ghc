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
    ) where

import GHC.Internal.Base
import GHC.Internal.Data.Maybe
import GHC.Internal.Enum
import GHC.Internal.Show (Show)
import GHC.Internal.Ptr (Ptr(..), plusPtr)
import GHC.Internal.Foreign.C.String.Encoding (CString, peekCString)
import GHC.Internal.IO.Encoding (utf8)
import GHC.Internal.Foreign.Storable (peekByteOff)
import GHC.Internal.ClosureTypes
import GHC.Internal.Text.Read

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: ClosureType,
  ipTyDesc :: String,
  ipLabel :: String,
  ipMod :: String,
  ipSrcFile :: String,
  ipSrcSpan :: String
} deriving (Eq, Show)

ipLoc :: InfoProv -> String
ipLoc ipe = ipSrcFile ipe ++ ":" ++ ipSrcSpan ipe

data InfoProvEnt

getIPE :: a -> IO (Ptr InfoProvEnt)
getIPE obj = IO $ \s ->
   case whereFrom## obj s of
     (## s', addr ##) -> (## s', Ptr addr ##)

ipeProv :: Ptr InfoProvEnt -> Ptr InfoProv
ipeProv p = (#ptr InfoProvEnt, prov) p

peekIpName, peekIpDesc, peekIpLabel, peekIpModule, peekIpSrcFile, peekIpSrcSpan, peekIpTyDesc :: Ptr InfoProv -> IO CString
peekIpName p    =  (# peek InfoProv, table_name) p
peekIpDesc p    =  (# peek InfoProv, closure_desc) p
peekIpLabel p   =  (# peek InfoProv, label) p
peekIpModule p  =  (# peek InfoProv, module) p
peekIpSrcFile p =  (# peek InfoProv, src_file) p
peekIpSrcSpan p =  (# peek InfoProv, src_span) p
peekIpTyDesc p  =  (# peek InfoProv, ty_desc) p

peekInfoProv :: Ptr InfoProv -> IO InfoProv
peekInfoProv infop = do
  name <- peekCString utf8 =<< peekIpName infop
  desc <- peekCString utf8 =<< peekIpDesc infop
  tyDesc <- peekCString utf8 =<< peekIpTyDesc infop
  label <- peekCString utf8 =<< peekIpLabel infop
  mod <- peekCString utf8 =<< peekIpModule infop
  file <- peekCString utf8 =<< peekIpSrcFile infop
  span <- peekCString utf8 =<< peekIpSrcSpan infop
  return InfoProv {
      ipName = name,
      -- The INVALID_OBJECT case should be impossible as we
      -- control the C code generating these values.
      ipDesc = maybe INVALID_OBJECT toEnum . readMaybe @Int $ desc,
      ipTyDesc = tyDesc,
      ipLabel = label,
      ipMod = mod,
      ipSrcFile = file,
      ipSrcSpan = span
    }

