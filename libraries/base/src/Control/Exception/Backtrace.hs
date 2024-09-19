-- |
-- Module      :  Control.Exception.Backtrace
-- Copyright   :  (c) The University of Glasgow 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Mechanisms for collecting diagnostic backtraces and their representation.
--

-- Note [Backtrace mechanisms]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- See module docstring above.


module Control.Exception.Backtrace
    ( -- * Backtrace mechanisms
      BacktraceMechanism(..)
    , getBacktraceMechanismState
    , setBacktraceMechanismState
      -- * Collecting backtraces
    , Backtraces
    , displayBacktraces
    , collectBacktraces
    ) where

import GHC.Internal.Exception.Backtrace
