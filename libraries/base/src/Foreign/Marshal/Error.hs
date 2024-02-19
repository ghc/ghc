{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Marshal.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Routines for testing return values and raising a 'userError' exception
-- in case of values indicating an error state.
--

module Foreign.Marshal.Error
    (throwIf,
     throwIf_,
     throwIfNeg,
     throwIfNeg_,
     throwIfNull,
     void
     ) where

import GHC.Internal.Foreign.Marshal.Error