-- Should fail without -fglasgow-exts
-- Succeeds with -fglasgow-exts

module ShouldFail where

import Control.Monad.State

data S = S Int

newtype M a = M (StateT S IO a) deriving( Monad )




