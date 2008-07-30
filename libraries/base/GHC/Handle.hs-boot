{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Handle where

import GHC.IOBase

stdout :: Handle
stderr :: Handle
hFlush :: Handle -> IO ()
