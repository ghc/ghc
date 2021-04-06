{-
This module re-exports all the functions necessary to create, manipulate and pretty-print
GHC's diagnostic messages. It is meant as a one-stop-shop import for users.
-}

module GHC.Diagnostics (
  -- * Types, smart constructors and utility functions
    module Exports
  -- * Logging diagnostics
  , module Logging
  -- * Driver diagnostics
  , module Driver
  -- * TcRn diagnostics
  , module TcRn
  -- * Parser diagnostics
  , module Parser
  -- * hs-to-core diagnostics
  , module HsToCore
  ) where

import GHC.Types.Error as Exports
import GHC.Utils.Error as Exports
import GHC.Types.SourceError as Exports

import GHC.Utils.Logger as Logging

import GHC.Driver.Errors.Types as Driver
import GHC.Driver.Errors.Ppr as Driver ()

import GHC.Tc.Errors.Types as TcRn
import GHC.Tc.Errors.Ppr as TcRn ()

import GHC.Parser.Errors.Types as Parser
import GHC.Parser.Errors.Ppr as Parser

import GHC.HsToCore.Errors.Types as HsToCore
import GHC.HsToCore.Errors.Ppr as HsToCore ()
