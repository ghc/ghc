{-# OPTIONS_GHC -fwarn-implicit-prelude -fwarn-unused-imports #-}
module ShouldCompile where

import Prelude ()

-- !!! should produce no warnings
--     (the other use of importing nothing is
--     to nullify the implicit import of the Prelude)

