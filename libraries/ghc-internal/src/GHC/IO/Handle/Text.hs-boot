{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Handle.Text ( hPutStrLn ) where

import GHC.Base (String, IO)
import {-# SOURCE #-} GHC.IO.Handle.Types (Handle)

hPutStrLn :: Handle -> String -> IO ()
