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
import GHC.Types
  hiding ( IO   -- Exported from "GHC.IO"
         , Type -- Exported from "Data.Kind"

           -- GHC's internal representation of 'TyCon's, for 'Typeable'
         , Module, TrName, TyCon, TypeLitSort, KindRep, KindBndr )
import GHC.Prim hiding ( coerce, dataToTagSmall#, dataToTagLarge# )
  -- Hide dataToTag# ops because they are expected to break for
  -- GHC-internal reasons in the near future, and shouldn't
  -- be exposed from base (not even GHC.Exts)

import GHC.Prim.Ext
