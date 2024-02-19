{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.String
-- Copyright   :  (c) The University of Glasgow 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The @String@ type and associated operations.
--

module Data.String
    (String,
     IsString(..),
     -- *  Functions on strings
     lines,
     words,
     unlines,
     unwords
     ) where

import GHC.Internal.Data.String