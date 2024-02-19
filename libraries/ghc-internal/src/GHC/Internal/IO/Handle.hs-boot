{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle where

import GHC.Internal.IO
import GHC.Internal.IO.Handle.Types

hFlush :: Handle -> IO ()

