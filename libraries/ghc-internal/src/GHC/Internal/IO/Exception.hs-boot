{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Exception where

import GHC.Internal.Base
import {-# SOURCE #-} GHC.Internal.Exception.Type

data IOException
instance Exception IOException

type IOError = IOException
userError :: String  -> IOError
unsupportedOperation :: IOError

