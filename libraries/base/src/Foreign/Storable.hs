{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Storable
-- Copyright   :  (c) The FFI task force 2001
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The module "Foreign.Storable" provides most elementary support for
-- marshalling and is part of the language-independent portion of the
-- Foreign Function Interface (FFI), and will normally be imported via
-- the "Foreign" module.
--

module Foreign.Storable
    (Storable(sizeOf, alignment, peekElemOff, pokeElemOff, peekByteOff, pokeByteOff, peek, poke)
     ) where

import GHC.Internal.Foreign.Storable