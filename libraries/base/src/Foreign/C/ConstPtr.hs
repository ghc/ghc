{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.C.ConstPtr
-- Copyright   :  (c) GHC Developers
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed @const@ pointers to foreign data. It is part
-- of the Foreign Function Interface (FFI).
--

module Foreign.C.ConstPtr
    (ConstPtr(..)
     ) where

import GHC.Internal.Foreign.C.ConstPtr