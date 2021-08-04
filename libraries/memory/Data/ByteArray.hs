-- |
-- Module      : Data.ByteArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple and efficient byte array types
--
-- This module should be imported qualified.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.ByteArray
    (
    -- * ByteArray Classes
      module Data.ByteArray.Types
    -- * ByteArray built-in types
    , module Data.ByteArray.Bytes
    , module Data.ByteArray.ScrubbedBytes
    , module Data.ByteArray.MemView
    , module Data.ByteArray.View
    -- * ByteArray methods
    , module Data.ByteArray.Methods
    ) where

import           Data.ByteArray.Types
import           Data.ByteArray.Methods
import           Data.ByteArray.ScrubbedBytes (ScrubbedBytes)
import           Data.ByteArray.Bytes         (Bytes)
import           Data.ByteArray.MemView       (MemView(..))
import           Data.ByteArray.View          (View, view, takeView, dropView)
