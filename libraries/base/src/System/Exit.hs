{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  System.Exit
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exiting the program.
--

module System.Exit
    (ExitCode(ExitSuccess, ExitFailure),
     exitWith,
     exitFailure,
     exitSuccess,
     die
     ) where

import GHC.Internal.System.Exit