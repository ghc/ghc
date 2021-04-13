{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Magic.Dict
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Defines the 'magicDict' function. For more information, see
-- @Note [magicDictId magic]@ in "GHC.Types.Id.Make" in GHC.
--
-- Use "GHC.Exts" from the @base@ package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Magic.Dict (magicDict) where

import GHC.Prim.Panic (panicError)

-- | 'magicDict' is a special-purpose placeholder value.
-- It is used internally by modules such as "GHC.TypeNats" to cast a typeclass
-- dictionary with a single method. It is eliminated by a rule during compilation.
-- For the details, see @Note [magicDictId magic]@ in "GHC.Types.Id.Make" in GHC.
magicDict :: a
{-# NOINLINE magicDict #-}
magicDict = panicError "Non-rewritten magicDict"#
