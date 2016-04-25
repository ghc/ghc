{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Test where

import qualified Control.Monad (ap)
-- Test that GHC warns about the following unused import:
import qualified Control.Monad.Reader

foo :: IO ()
foo = return id `Control.Monad.ap` return ()
