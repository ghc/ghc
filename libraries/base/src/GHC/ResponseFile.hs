{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.ResponseFile
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  portable
--
-- GCC style response files.
--
-- @since 4.12.0.0

module GHC.ResponseFile (
    getArgsWithResponseFiles,
    unescapeArgs,
    escapeArgs,
    expandResponse
  ) where

import GHC.Internal.ResponseFile
