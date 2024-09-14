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
    , inline, noinline, lazy, oneShot, runRW#, seq#, DataToTag(..)
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
import GHC.Prim hiding (dataToTagLarge#, dataToTagSmall#, whereFrom#)
   -- Hide dataToTagLarge# because it is expected to break for
   -- GHC-internal reasons in the near future, and shouldn't
   -- be exposed from base (not even GHC.Exts)
   -- whereFrom# is similarly internal.

import GHC.Prim.Ext
import GHC.Prim.PtrEq
import GHC.Internal.Err
import GHC.Internal.IO.Magic (seq#)
import GHC.Internal.Maybe
import GHC.Types hiding (
  Unit#,
  Solo#,
  Tuple0#,
  Tuple1#,
  Tuple2#,
  Tuple3#,
  Tuple4#,
  Tuple5#,
  Tuple6#,
  Tuple7#,
  Tuple8#,
  Tuple9#,
  Tuple10#,
  Tuple11#,
  Tuple12#,
  Tuple13#,
  Tuple14#,
  Tuple15#,
  Tuple16#,
  Tuple17#,
  Tuple18#,
  Tuple19#,
  Tuple20#,
  Tuple21#,
  Tuple22#,
  Tuple23#,
  Tuple24#,
  Tuple25#,
  Tuple26#,
  Tuple27#,
  Tuple28#,
  Tuple29#,
  Tuple30#,
  Tuple31#,
  Tuple32#,
  Tuple33#,
  Tuple34#,
  Tuple35#,
  Tuple36#,
  Tuple37#,
  Tuple38#,
  Tuple39#,
  Tuple40#,
  Tuple41#,
  Tuple42#,
  Tuple43#,
  Tuple44#,
  Tuple45#,
  Tuple46#,
  Tuple47#,
  Tuple48#,
  Tuple49#,
  Tuple50#,
  Tuple51#,
  Tuple52#,
  Tuple53#,
  Tuple54#,
  Tuple55#,
  Tuple56#,
  Tuple57#,
  Tuple58#,
  Tuple59#,
  Tuple60#,
  Tuple61#,
  Tuple62#,
  Tuple63#,
  Tuple64#,
  Sum2#,
  Sum3#,
  Sum4#,
  Sum5#,
  Sum6#,
  Sum7#,
  Sum8#,
  Sum9#,
  Sum10#,
  Sum11#,
  Sum12#,
  Sum13#,
  Sum14#,
  Sum15#,
  Sum16#,
  Sum17#,
  Sum18#,
  Sum19#,
  Sum20#,
  Sum21#,
  Sum22#,
  Sum23#,
  Sum24#,
  Sum25#,
  Sum26#,
  Sum27#,
  Sum28#,
  Sum29#,
  Sum30#,
  Sum31#,
  Sum32#,
  Sum33#,
  Sum34#,
  Sum35#,
  Sum36#,
  Sum37#,
  Sum38#,
  Sum39#,
  Sum40#,
  Sum41#,
  Sum42#,
  Sum43#,
  Sum44#,
  Sum45#,
  Sum46#,
  Sum47#,
  Sum48#,
  Sum49#,
  Sum50#,
  Sum51#,
  Sum52#,
  Sum53#,
  Sum54#,
  Sum55#,
  Sum56#,
  Sum57#,
  Sum58#,
  Sum59#,
  Sum60#,
  Sum61#,
  Sum62#,
  Sum63#,
  )
