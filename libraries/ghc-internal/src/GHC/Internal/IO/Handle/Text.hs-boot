{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle.Text ( hPutStrLn ) where

import GHC.Internal.Base (String, IO)
import {-# SOURCE #-} GHC.Internal.IO.Handle.Types (Handle)

hPutStrLn :: Handle -> String -> IO ()
