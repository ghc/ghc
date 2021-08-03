{-# LANGUAGE ScopedTypeVariables #-}
module GHCJS
    (
      module GHC
    , module Compiler.GhcjsProgram
    , module Compiler.Program
    , module Compiler.Settings
    ) where

import GHC
import Compiler.GhcjsProgram
import Compiler.Program
import Compiler.Settings
import Prelude
