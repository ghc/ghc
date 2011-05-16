{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.IO.Encoding where

import GHC.IO.Encoding.Types

localeEncoding, fileSystemEncoding, foreignEncoding :: TextEncoding
