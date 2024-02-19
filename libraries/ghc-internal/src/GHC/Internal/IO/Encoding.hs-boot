{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Encoding where

import GHC.Internal.IO (IO)
import GHC.Internal.IO.Encoding.Types

getLocaleEncoding, getFileSystemEncoding, getForeignEncoding :: IO TextEncoding

