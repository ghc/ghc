{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import GHC.Internal.Data.Functor.Const (Const (Const))
import GHC.Internal.Data.Functor.Identity (Identity (Identity))

-- | @since base-4.9.0.0
deriving instance IsString a => IsString (Const a (b :: k))

-- | @since base-4.9.0.0
deriving instance IsString a => IsString (Identity a)
