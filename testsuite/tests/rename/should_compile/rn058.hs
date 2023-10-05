{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- #1386
-- We do not want a warning about unused imports

module Foo () where

import Control.Monad (liftM)

foo :: IO ()
foo = id `liftM` return ()

foreign export ccall "hs_foo" foo :: IO ()
