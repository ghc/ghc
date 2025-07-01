{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnliftedNewtypes           #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler types
module GHC.ByteCode.Types
  ( CompiledByteCode(..), seqCompiledByteCode
  , BCOByteArray(..), mkBCOByteArray
  , FFIInfo(..)
  , RegBitmap(..)
  , NativeCallType(..), NativeCallInfo(..), voidTupleReturnInfo, voidPrimCallInfo
  , ByteOff(..), WordOff(..), HalfWord(..)
  , UnlinkedBCO(..), BCOPtr(..), BCONPtr(..)
  , ItblEnv, ItblPtr(..)
  , AddrEnv, AddrPtr(..)
  , FlatBag, sizeFlatBag, fromSmallArray, elemsFlatBag

  -- * Mod Breaks
  , ModBreaks (..), BreakpointId(..), BreakTickIndex

  -- * Internal Mod Breaks
  , InternalModBreaks(..), CgBreakInfo(..), seqInternalModBreaks
  -- ** Internal breakpoint identifier
  , InternalBreakpointId(..), BreakInfoIndex
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Data.FlatBag
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Utils.Outputable
import GHC.Builtin.PrimOps
import GHC.Types.SptEntry
import GHC.HsToCore.Breakpoints
import GHC.ByteCode.Breakpoints
import GHCi.Message
import GHCi.RemoteTypes
import GHCi.FFI
import Control.DeepSeq
import GHCi.ResolvedBCO ( BCOByteArray(..), mkBCOByteArray )

import Foreign
import Data.ByteString (ByteString)
import qualified GHC.Exts.Heap as Heap
import GHC.Cmm.Expr ( GlobalRegSet, emptyRegSet, regSetToList )
import GHC.Unit.Module

-- -----------------------------------------------------------------------------
-- Compiled Byte Code

data CompiledByteCode = CompiledByteCode
  { bc_bcos   :: FlatBag UnlinkedBCO
    -- ^ Bunch of interpretable bindings

  , bc_itbls  :: [(Name, ConInfoTable)]
    -- ^ Mapping from DataCons to their info tables

  , bc_strs   :: [(Name, ByteString)]
    -- ^ top-level strings (heap allocated)

  , bc_breaks :: Maybe InternalModBreaks
    -- ^ All breakpoint information (no information if breakpoints are disabled).
    --
    -- This information is used when loading a bytecode object: we will
    -- construct the arrays to be used at runtime to trigger breakpoints at load time
    -- from it (in 'allocateBreakArrays' and 'allocateCCS' in 'GHC.ByteCode.Loader').

  , bc_spt_entries :: ![SptEntry]
    -- ^ Static pointer table entries which should be loaded along with the
    -- BCOs. See Note [Grand plan for static forms] in
    -- "GHC.Iface.Tidy.StaticPtrTable".
  }

-- | A libffi ffi_cif function prototype.
data FFIInfo = FFIInfo { ffiInfoArgs :: ![FFIType], ffiInfoRet :: !FFIType }
  deriving (Show)

instance Outputable CompiledByteCode where
  ppr CompiledByteCode{..} = ppr $ elemsFlatBag bc_bcos

-- Not a real NFData instance, because ModBreaks contains some things
-- we can't rnf
seqCompiledByteCode :: CompiledByteCode -> ()
seqCompiledByteCode CompiledByteCode{..} =
  rnf bc_bcos `seq`
  rnf bc_itbls `seq`
  rnf bc_strs `seq`
  case bc_breaks of
    Nothing -> ()
    Just ibks -> seqInternalModBreaks ibks

newtype ByteOff = ByteOff Int
    deriving (Enum, Eq, Show, Integral, Num, Ord, Real, Outputable)

newtype WordOff = WordOff Int
    deriving (Enum, Eq, Show, Integral, Num, Ord, Real, Outputable)

-- A type for values that are half the size of a word on the target
-- platform where the interpreter runs (which may be a different
-- wordsize than the compiler).
newtype HalfWord = HalfWord Word
    deriving (Enum, Eq, Show, Integral, Num, Ord, Real, Outputable)

newtype RegBitmap = RegBitmap { unRegBitmap :: Word32 }
    deriving (Enum, Eq, Show, Integral, Num, Ord, Real, Bits, FiniteBits, Outputable)

{- Note [GHCi TupleInfo]
~~~~~~~~~~~~~~~~~~~~~~~~
   This contains the data we need for passing unboxed tuples between
   bytecode and native code

   In general we closely follow the native calling convention that
   GHC uses for unboxed tuples, but we don't use any registers in
   bytecode. All tuple elements are expanded to use a full register
   or a full word on the stack.

   The position of tuple elements that are returned on the stack in
   the native calling convention is unchanged when returning the same
   tuple in bytecode.

   The order of the remaining elements is determined by the register in
   which they would have been returned, rather than by their position in
   the tuple in the Haskell source code. This makes jumping between bytecode
   and native code easier: A map of live registers is enough to convert the
   tuple.

   See GHC.StgToByteCode.layoutTuple for more details.
-}

data NativeCallType = NativePrimCall
                    | NativeTupleReturn
  deriving (Eq)

data NativeCallInfo = NativeCallInfo
  { nativeCallType           :: !NativeCallType
  , nativeCallSize           :: !WordOff   -- total size of arguments in words
  , nativeCallRegs           :: !GlobalRegSet
  , nativeCallStackSpillSize :: !WordOff {- words spilled on the stack by
                                            GHCs native calling convention -}
  }

instance Outputable NativeCallInfo where
  ppr NativeCallInfo{..} = text "<arg_size" <+> ppr nativeCallSize <+>
                           text "stack" <+> ppr nativeCallStackSpillSize <+>
                           text "regs"  <+>
                           ppr (map (text @SDoc . show) $ regSetToList nativeCallRegs) <>
                           char '>'


voidTupleReturnInfo :: NativeCallInfo
voidTupleReturnInfo = NativeCallInfo NativeTupleReturn 0 emptyRegSet 0

voidPrimCallInfo :: NativeCallInfo
voidPrimCallInfo = NativeCallInfo NativePrimCall 0 emptyRegSet 0

type ItblEnv = NameEnv (Name, ItblPtr)
type AddrEnv = NameEnv (Name, AddrPtr)
        -- We need the Name in the range so we know which
        -- elements to filter out when unloading a module

newtype ItblPtr = ItblPtr (RemotePtr Heap.StgInfoTable)
  deriving (Show, NFData)
newtype AddrPtr = AddrPtr (RemotePtr ())
  deriving (NFData)

{-
--------------------------------------------------------------------------------
-- * Byte Code Objects (BCOs)
--------------------------------------------------------------------------------

Note [Case continuation BCOs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A stack with a BCO stack frame at the top looks like:

                                      (an StgBCO)
         |       ...        |      +---> +---------[1]--+
         +------------------+      |     | info_tbl_ptr | ------+
         |    OTHER FRAME   |      |     +--------------+       |
         +------------------+      |     | StgArrBytes* | <--- the byte code
         |       ...        |      |     +--------------+       |
         +------------------+      |     |     ...      |       |
         |       fvs1       |      |                            |
         +------------------+      |                            |
         |       ...        |      |        (StgInfoTable)      |
         +------------------+      |           +----------+ <---+
         |      args1       |      |           |    ...   |
         +------------------+      |           +----------+
         |   some StgBCO*   | -----+           | type=BCO |
         +------------------+                  +----------+
      Sp | stg_apply_interp | -----+           |   ...    |
         +------------------+      |
                                   |
                                   |   (StgInfoTable)
                                   +----> +--------------+
                                          |     ...      |
                                          +--------------+
                                          | type=RET_BCO |
                                          +--------------+
                                          |     ...      |


In the case of bytecode objects found on the heap (e.g. thunks and functions),
the bytecode may refer to free variables recorded in the BCO closure itself.
By contrast, in /case continuation/ BCOs the code may additionally refer to free
variables in their stack frame. These are references by way of statically known
stack offsets (tracked using `BCEnv` in `StgToByteCode`).

For instance, consider the function:

    f x y = case y of ... -> g x

Here the RHS of the alternative refers to `x`, which will be recorded in the
continuation stack frame of the `case`.

Even less obvious is that case continuation BCOs may also refer to free
variables in *parent* stack frames. For instance,

    f x y = case y of
      ... -> case g x of
        ... -> x

Here, the RHS of the first alternative still refers to the `x` in the stack
frame of the `case`. Additionally, the RHS of the second alternative also
refers to `x` but it must traverse to its case's *parent* stack frame to find `x`.

However, in /case continuation/ BCOs, the code may additionally refer to free
variables that are outside of that BCO's stack frame -- some free variables of a
case continuation BCO may only be found in the stack frame of a parent BCO.

Yet, references to these out-of-frame variables are also done in terms of stack
offsets. Thus, they rely on the position of /another frame/ to be fixed. (See
Note [PUSH_L underflow] for more information about references to previous
frames and nested BCOs)

This makes case continuation BCOs special: unlike normal BCOs, case cont BCO
frames cannot be moved on the stack independently from their parent BCOs.
-}

data UnlinkedBCO
   = UnlinkedBCO {
        unlinkedBCOName   :: !Name,
        unlinkedBCOArity  :: {-# UNPACK #-} !Int,
        unlinkedBCOInstrs :: !(BCOByteArray Word16),      -- insns
        unlinkedBCOBitmap :: !(BCOByteArray Word),      -- bitmap
        unlinkedBCOLits   :: !(FlatBag BCONPtr),       -- non-ptrs
        unlinkedBCOPtrs   :: !(FlatBag BCOPtr)         -- ptrs
   }

instance NFData UnlinkedBCO where
  rnf UnlinkedBCO{..} =
    rnf unlinkedBCOLits `seq`
    rnf unlinkedBCOPtrs

data BCOPtr
  = BCOPtrName   !Name
  | BCOPtrPrimOp !PrimOp
  | BCOPtrBCO    !UnlinkedBCO
  | BCOPtrBreakArray !Module
    -- ^ Converted to the actual 'BreakArray' remote pointer at link-time

instance NFData BCOPtr where
  rnf (BCOPtrBCO bco) = rnf bco
  rnf x = x `seq` ()

data BCONPtr
  = BCONPtrWord  {-# UNPACK #-} !Word
  | BCONPtrLbl   !FastString
  | BCONPtrItbl  !Name
  -- | A reference to a top-level string literal; see
  -- Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode.
  | BCONPtrAddr  !Name
  -- | A top-level string literal.
  -- Also see Note [Allocating string literals] in GHC.ByteCode.Asm.
  | BCONPtrStr   !ByteString
  -- | Same as 'BCONPtrStr' but with benefits of 'FastString' interning logic.
  | BCONPtrFS    !FastString
  -- | A libffi ffi_cif function prototype.
  | BCONPtrFFIInfo !FFIInfo
  -- | A 'CostCentre' remote pointer array's respective 'Module' and index
  | BCONPtrCostCentre !Module !BreakTickIndex

instance NFData BCONPtr where
  rnf x = x `seq` ()

instance Outputable UnlinkedBCO where
   ppr (UnlinkedBCO nm _arity _insns _bitmap lits ptrs)
      = sep [text "BCO", ppr nm, text "with",
             ppr (sizeFlatBag lits), text "lits",
             ppr (sizeFlatBag ptrs), text "ptrs" ]

