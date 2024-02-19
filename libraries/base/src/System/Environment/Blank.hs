{-# LANGUAGE Safe #-}

-- |
-- Module      :  System.Environment.Blank
-- Copyright   :  (c) Habib Alamin 2017
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A setEnv implementation that allows blank environment variables. Mimics
-- the `System.Posix.Env` module from the @unix@ package, but with support
-- for Windows too.
--
-- The matrix of platforms that:
--
--   * support @putenv("FOO")@ to unset environment variables,
--   * support @putenv("FOO=")@ to unset environment variables or set them
--     to blank values,
--   * support @unsetenv@ to unset environment variables,
--   * support @setenv@ to set environment variables,
--   * etc.
--
-- is very complicated. Some platforms don't support unsetting of environment
-- variables at all.
--

module System.Environment.Blank
    (
      module System.Environment,
      getEnv,
      getEnvDefault,
      setEnv,
      unsetEnv,
  ) where

import System.Environment
    (
      getArgs,
      getProgName,
      getExecutablePath,
      withArgs,
      withProgName,
      getEnvironment
    )

import GHC.Internal.System.Environment.Blank
