{-# LANGUAGE MagicHash #-}

-- |
--
-- Module      :  GHC.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC Extensions: this is the Approved Way to get at GHC-specific extensions.
--
-- Note: no other @base@ module should import this module.

module GHC.Exts
    (-- **  Pointer types
     Ptr(..),
     FunPtr(..),
     -- **  Other primitive types
     module GHC.Types,
     -- **  Legacy interface for arrays of arrays
     module GHC.Internal.ArrayArray,
     -- *  Primitive operations
     {-# DEPRECATED ["The BCO, mkApUpd0#, and newBCO# re-exports from GHC.Exts have been deprecated and will be removed in 9.14", "These symbols should be imported from ghc-internal instead if needed."] #-}
     Prim.BCO,
     {-# DEPRECATED ["The BCO, mkApUpd0#, and newBCO# re-exports from GHC.Exts have been deprecated and will be removed in 9.14", "These symbols should be imported from ghc-internal instead if needed."] #-}
     Prim.mkApUpd0#,
     {-# DEPRECATED ["The BCO, mkApUpd0#, and newBCO# re-exports from GHC.Exts have been deprecated and will be removed in 9.14", "These symbols should be imported from ghc-internal instead if needed."] #-}
     Prim.newBCO#,
     module GHC.Prim,
     module GHC.Prim.Ext,
     -- **  Running 'RealWorld' state thread
     runRW#,
     -- **  Bit shift operations
     shiftL#,
     shiftRL#,
     iShiftL#,
     iShiftRA#,
     iShiftRL#,
     -- **  Pointer comparison operations
     reallyUnsafePtrEquality,
     unsafePtrEquality#,
     eqStableName#,
     sameArray#,
     sameMutableArray#,
     sameSmallArray#,
     sameSmallMutableArray#,
     sameByteArray#,
     sameMutableByteArray#,
     sameMVar#,
     sameMutVar#,
     sameTVar#,
     sameIOPort#,
     samePromptTag#,
     -- **  Compat wrapper
     atomicModifyMutVar#,
     -- **  Resize functions
     -- |  Resizing arrays of boxed elements is currently handled in
     -- library space (rather than being a primop) since there is not
     -- an efficient way to grow arrays. However, resize operations
     -- may become primops in a future release of GHC.
     resizeSmallMutableArray#,
     -- **  Fusion
     build,
     augment,
     -- *  Overloaded lists
     IsList(..),
     -- *  Transform comprehensions
     Down(..),
     groupWith,
     sortWith,
     the,
     -- *  Strings
     -- **  Overloaded string literals
     IsString(..),
     -- **  CString
     unpackCString#,
     unpackAppendCString#,
     unpackFoldrCString#,
     unpackCStringUtf8#,
     unpackNBytes#,
     cstringLength#,
     -- *  Debugging
     -- **  Breakpoints
     breakpoint,
     breakpointCond,
     -- **  Event logging
     traceEvent,
     -- **  The call stack
     currentCallStack,
     -- *  Ids with special behaviour
     inline,
     noinline,
     lazy,
     oneShot,
     considerAccessible,
     seq#,
     -- *  SpecConstr annotations
     SpecConstrAnnotation(..),
     SPEC(..),
     -- *  Coercions
     -- **  Safe coercions
     -- |  These are available from the /Trustworthy/ module "Data.Coerce" as well.
     --
     -- @since 4.7.0.0
     coerce,
     -- **  Very unsafe coercion
     unsafeCoerce#,
     -- **  Casting class dictionaries with single methods
     WithDict(..),
     -- *  Converting ADTs to constructor tags
     DataToTag(..),
     -- *  The maximum tuple size
     maxTupleSize
     ) where

import GHC.Internal.Exts
import GHC.Internal.ArrayArray
import GHC.Prim hiding
  ( coerce
  -- Hide dataToTag# ops because they are expected to break for
  -- GHC-internal reasons in the near future, and shouldn't
  -- be exposed from base (not even GHC.Exts)
  , dataToTagSmall#, dataToTagLarge#
  -- whereFrom# is similarly internal.
  , whereFrom#
  , isByteArrayWeaklyPinned#, isMutableByteArrayWeaklyPinned#

  -- deprecated
  , BCO, mkApUpd0#, newBCO#

  -- Don't re-export vector FMA instructions
  , fmaddFloatX4#
  , fmsubFloatX4#
  , fnmaddFloatX4#
  , fnmsubFloatX4#
  , fmaddFloatX8#
  , fmsubFloatX8#
  , fnmaddFloatX8#
  , fnmsubFloatX8#
  , fmaddFloatX16#
  , fmsubFloatX16#
  , fnmaddFloatX16#
  , fnmsubFloatX16#
  , fmaddDoubleX2#
  , fmsubDoubleX2#
  , fnmaddDoubleX2#
  , fnmsubDoubleX2#
  , fmaddDoubleX4#
  , fmsubDoubleX4#
  , fnmaddDoubleX4#
  , fnmsubDoubleX4#
  , fmaddDoubleX8#
  , fmsubDoubleX8#
  , fnmaddDoubleX8#
  , fnmsubDoubleX8#
  )
import qualified GHC.Prim as Prim
  ( BCO, mkApUpd0#, newBCO# )

import GHC.Prim.Ext

import GHC.Types hiding (
  IO,   -- Exported from "GHC.IO"
  Type, -- Exported from "Data.Kind"
  -- GHC's internal representation of 'TyCon's, for 'Typeable'
  Module, TrName, TyCon, TypeLitSort, KindRep, KindBndr,
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
