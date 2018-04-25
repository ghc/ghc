module ShouldCompile where

import Prelude ()
import Control.Monad( Monad(return), unless )
  -- Should report Monad and return as unused imports
import GHC.Base
  -- But not their import from here

x = True
y x = unless
