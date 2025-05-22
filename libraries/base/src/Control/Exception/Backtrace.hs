-- |
-- Module      :  Control.Exception.Backtrace
-- Copyright   :  (c) The University of Glasgow 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides the 'Backtrace'\ s type, which provides a
-- common representation for backtrace information which can be, e.g., attached
-- to exceptions (via the 'Control.Exception.Context.ExceptionContext' facility).
-- These backtraces preserve useful context about the execution state of the program
-- using a variety of means; we call these means *backtrace mechanisms*.
--
-- We currently support four backtrace mechanisms:
--
--  - 'CostCentreBacktrace' captures the current cost-centre stack
--    using 'GHC.Stack.CCS.getCurrentCCS'.
--  - 'HasCallStackBacktrace' captures the 'HasCallStack' 'CallStack'.
--  - 'ExecutionBacktrace' captures the execution stack, unwound and resolved
--    to symbols via DWARF debug information.
--  - 'IPEBacktrace' captures the execution stack, resolved to names via info-table
--    provenance information.
--
-- Each of these are useful in different situations. While 'CostCentreBacktrace's are
-- readily mapped back to the source program, they require that the program be instrumented
-- with cost-centres, incurring runtime cost. Similarly, 'HasCallStackBacktrace's require that
-- the program be manually annotated with 'HasCallStack' constraints.
--
-- By contrast, 'IPEBacktrace's incur no runtime instrumentation but require that (at least
-- some subset of) the program be built with GHC\'s @-finfo-table-map@ flag. Moreover, because
-- info-table provenance information is derived after optimisation, it may be harder to relate
-- back to the structure of the source program.
--
-- 'ExecutionBacktrace's are similar to 'IPEBacktrace's but use DWARF stack unwinding
-- and symbol resolution; this allows for useful backtraces even in the presence
-- of foreign calls, both into and out of Haskell. However, for robust stack unwinding
-- the entirety of the program (and its dependencies, both Haskell and native) must
-- be compiled with debugging information (e.g. using GHC\'s @-g@ flag).


-- Note [Backtrace mechanisms]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- See module docstring above.


module Control.Exception.Backtrace
    ( -- * Backtrace mechanisms
      BacktraceMechanism(..)
    , getBacktraceMechanismState
    , setBacktraceMechanismState
      -- * Collecting backtraces
    , Backtraces(..)
    , displayBacktraces
    , collectBacktraces
    ) where

import GHC.Internal.Exception.Backtrace
