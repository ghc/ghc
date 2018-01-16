{-# LANGUAGE CPP #-}

-- | @since 2.2.0
module Distribution.Utils.IOData
    ( -- * 'IOData' & 'IODataMode' type
      IOData(..)
    , IODataMode(..)
    , null
    , hGetContents
    , hPutContents
    ) where

import qualified Data.ByteString.Lazy as BS
import           Distribution.Compat.Prelude hiding (null)
import qualified Prelude
import qualified System.IO

-- | Represents either textual or binary data passed via I/O functions
-- which support binary/text mode
--
-- @since 2.2.0
data IOData = IODataText    String
              -- ^ How Text gets encoded is usually locale-dependent.
            | IODataBinary  BS.ByteString
              -- ^ Raw binary which gets read/written in binary mode.

-- | Test whether 'IOData' is empty
--
-- @since 2.2.0
null :: IOData -> Bool
null (IODataText s) = Prelude.null s
null (IODataBinary b) = BS.null b

instance NFData IOData where
    rnf (IODataText s) = rnf s
#if MIN_VERSION_bytestring(0,10,0)
    rnf (IODataBinary bs) = rnf bs
#else
    rnf (IODataBinary bs) = rnf (BS.length bs)
#endif

data IODataMode = IODataModeText | IODataModeBinary

-- | 'IOData' Wrapper for 'System.IO.hGetContents'
--
-- __Note__: This operation uses lazy I/O. Use 'NFData' to force all
-- data to be read and consequently the internal file handle to be
-- closed.
--
-- @since 2.2.0
hGetContents :: System.IO.Handle -> IODataMode -> Prelude.IO IOData
hGetContents h IODataModeText = do
    System.IO.hSetBinaryMode h False
    IODataText <$> System.IO.hGetContents h
hGetContents h IODataModeBinary = do
    System.IO.hSetBinaryMode h True
    IODataBinary <$> BS.hGetContents h

-- | 'IOData' Wrapper for 'System.IO.hPutStr' and 'System.IO.hClose'
--
-- This is the dual operation ot 'ioDataHGetContents',
-- and consequently the handle is closed with `hClose`.
--
-- @since 2.2.0
hPutContents :: System.IO.Handle -> IOData -> Prelude.IO ()
hPutContents h (IODataText c) = do
    System.IO.hSetBinaryMode h False
    System.IO.hPutStr h c
    System.IO.hClose h
hPutContents h (IODataBinary c) = do
    System.IO.hSetBinaryMode h True
    BS.hPutStr h c
    System.IO.hClose h
