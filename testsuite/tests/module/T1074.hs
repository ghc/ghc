{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Test where

import qualified Control.Monad (ap)
import qualified Control.Monad.Reader

foo :: IO ()
foo = return id `Control.Monad.ap` return ()
