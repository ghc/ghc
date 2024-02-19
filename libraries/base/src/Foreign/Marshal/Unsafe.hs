-- |
--
-- Module      :  Foreign.Marshal.Unsafe
-- Copyright   :  (c) The FFI task force 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support. Unsafe API.
--

module Foreign.Marshal.Unsafe
    (-- *  Unsafe functions
     unsafeLocalState
     ) where

import GHC.Internal.Foreign.Marshal.Unsafe