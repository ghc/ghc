{-# LANGUAGE CPP, DeriveDataTypeable #-}

module HsDoc
  ( HsDocString
  , LHsDocString
  , mkHsDocString
  , mkHsDocStringUtf8ByteString
  , unpackHDS
  , hsDocStringToByteString
  , ppr_mbDoc
  ) where

#include "HsVersions.h"

import GhcPrelude

import Encoding
import FastFunctions
import Outputable
import SrcLoc

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Data
import Foreign

-- | Haskell Documentation String
--
-- Internally this is a UTF8-Encoded 'ByteString'.
newtype HsDocString = HsDocString ByteString
  deriving (Eq, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr = text . unpackHDS

mkHsDocString :: String -> HsDocString
mkHsDocString s =
  inlinePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodeString ptr s
      pure (HsDocString (BS.fromForeignPtr buf 0 len))

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringUtf8ByteString :: ByteString -> HsDocString
mkHsDocStringUtf8ByteString = HsDocString

unpackHDS :: HsDocString -> String
unpackHDS = utf8DecodeByteString . hsDocStringToByteString

-- | Return the contents of a 'HsDocString' as a UTF8-encoded 'ByteString'.
hsDocStringToByteString :: HsDocString -> ByteString
hsDocStringToByteString (HsDocString bs) = bs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty
