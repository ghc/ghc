{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
--

-- N.B. This is a legacy module which we would at some point like to
-- deprecate and drop from `base`. In short, everything found here is
-- better imported from elsewhere. Until we have done so we prefer to
-- keep the export list as specific as possible (e.g. avoiding module
-- exports) to avoid changes in `ghc-internal` inadvertently
-- compromising the stability of this interface.

module GHC.Base
    ( module GHC.Types
    , module GHC.Prim
    , module GHC.Prim.Ext
    , module GHC.Prim.PtrEq
    , module GHC.Internal.Err
    , module GHC.Internal.Maybe

      -- * Equality and ordering
    , IP(..)
    , Eq(..)
    , Ord(..)
      -- ** Monomorphic equality operators
    , eqInt, neInt
    , eqWord, neWord
    , eqChar, neChar
    , eqFloat, eqDouble
    , gtInt, geInt, leInt, ltInt, compareInt, compareInt#
    , gtWord, geWord, leWord, ltWord, compareWord, compareWord#

      -- * C Strings
    , unpackCString#, unpackAppendCString#, unpackFoldrCString#
    , cstringLength#
    , unpackCStringUtf8#, unpackAppendCStringUtf8#, unpackFoldrCStringUtf8#
    , unpackNBytes#

      -- * Magic combinators
    , inline, noinline, lazy, oneShot, runRW#, DataToTag(..)
    , WithDict(withDict)

      -- * Functions over 'Bool'
    , (&&), (||), not

      -- Void
    , Void
    , absurd
    , vacuous

      -- * Semigroup/Monoid
    , Semigroup(..)
    , Monoid(..)

      -- * Functors
    , Functor(..)
    , Applicative(..)
    , (<**>)
    , liftA
    , liftA3
    , join
    , Monad(..)
    , (=<<)
    , when
    , sequence
    , mapM
    , liftM
    , liftM2
    , liftM3
    , liftM4
    , liftM5
    , ap
    , Alternative(..)
    , MonadPlus(..)

      -- Lists
    , NonEmpty(..)
    , foldr
    , build
    , augment
    , map
    , mapFB
    , (++)
    , String
    , unsafeChr
    , ord
    , eqString
    , minInt, maxInt

      -- * Miscellanea
    , otherwise
    , id
    , assert
    , breakpoint
    , breakpointCond
    , Opaque(..)
    , const
    , (.)
    , flip
    , ($)
    , ($!)
    , until
    , asTypeOf

      -- * IO
    , returnIO
    , bindIO
    , thenIO
    , failIO
    , unIO

      -- * Low-level integer utilities
    , getTag
    , quotInt
    , remInt
    , divInt
    , modInt
    , quotRemInt
    , divModInt
    , shift_mask
    , shiftL#
    , shiftRL#
    , iShiftL#
    , iShiftRA#
    , iShiftRL#
    , divInt#, divInt8#, divInt16#, divInt32#
    , modInt#, modInt8#, modInt16#, modInt32#
    , divModInt#, divModInt8#, divModInt16#, divModInt32#
    ) where

import GHC.Internal.Base
import GHC.Types
import GHC.Prim hiding (dataToTagLarge#, dataToTagSmall#)
import GHC.Prim.Ext
import GHC.Prim.PtrEq
import GHC.Internal.Err
import GHC.Internal.Maybe
