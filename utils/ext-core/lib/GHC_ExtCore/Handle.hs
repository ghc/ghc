{-# OPTIONS -fglasgow-exts #-}

-- Replacement for GHC.Handle module

module GHC_ExtCore.Handle(Handle(..), stdin, stderr, stdout, hFlush) where

import GHC.Exts

newtype Handle = H Int

-- these shouldn't actually get used
stdout, stdin, stderr :: Handle
stdin  = H 0
stdout = H 1
stderr = H 2

-- ditto
hFlush :: Handle -> IO ()
hFlush _ = return ()
