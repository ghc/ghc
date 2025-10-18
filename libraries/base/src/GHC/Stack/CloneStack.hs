-- |
-- This module exposes an interface for capturing the state of a thread's
-- execution stack for diagnostics purposes: 'cloneMyStack',
-- 'cloneThreadStack'.
--
-- Such a "cloned" stack can be decoded with 'decode' to a stack trace, given
-- that the @-finfo-table-map@ is enabled.
--
-- @since 4.17.0.0

module GHC.Stack.CloneStack (
  StackSnapshot(..),
  StackEntry(..),
  cloneMyStack,
  cloneThreadStack,
  decode
  ) where

import GHC.Internal.Stack.CloneStack
