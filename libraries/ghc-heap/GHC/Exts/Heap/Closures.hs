{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
-- Late cost centres introduce a thunk in the asBox function, which leads to
-- an additional wrapper being added to any value placed inside a box.
-- This can be removed once our boot compiler is no longer affected by #25212
{-# OPTIONS_GHC -fno-prof-late  #-}
{-# LANGUAGE NamedFieldPuns #-}

module GHC.Exts.Heap.Closures (
    -- * Closures
      Closure
    , GenClosure(..)
    , getClosureInfoTbl
    , getClosureInfoTbl_maybe
    , getClosurePtrArgs
    , getClosurePtrArgs_maybe
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , allClosures
    , closureSize

    -- * Stack
    , StgStackClosure
    , GenStgStackClosure(..)
    , StackFrame
    , GenStackFrame(..)
    , StackField
    , GenStackField(..)

    -- * Boxes
    , Box(..)
    , areBoxesEqual
    , asBox
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.Constants
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTable () -- see Note [No way-dependent imports]
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
import GHC.Exts.Heap.InfoTableProf () -- see Note [No way-dependent imports]

{-
Note [No way-dependent imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`ghc -M` currently assumes that the imports for a module are the same
in every way.  This is arguably a bug, but breaking this assumption by
importing different things in different ways can cause trouble.  For
example, this module in the profiling way imports and uses
GHC.Exts.Heap.InfoTableProf.  When it was not also imported in the
vanilla way, there were intermittent build failures due to this module
being compiled in the profiling way before GHC.Exts.Heap.InfoTableProf
in the profiling way. (#15197)
-}
#endif

import GHC.Exts.Heap.ProfInfo.Types

import Data.Bits
import Data.Foldable (toList)
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import Numeric
import GHC.Stack (HasCallStack)

------------------------------------------------------------------------
-- Boxes

foreign import prim "aToWordzh" aToWord# :: Any -> Word#

foreign import prim "reallyUnsafePtrEqualityUpToTag"
    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#

-- | An arbitrary Haskell value in a safe Box. The point is that even
-- unevaluated thunks can safely be moved around inside the Box, and when
-- required, e.g. in 'getBoxedClosureData', the function knows how far it has
-- to evaluate the argument.
data Box = Box Any

instance Show Box where
-- From libraries/base/GHC/Ptr.lhs
   showsPrec _ (Box a) rs =
    -- unsafePerformIO (print "↓" >> pClosure a) `seq`
    pad_out (showHex addr "") ++ (if tag>0 then "/" ++ show tag else "") ++ rs
     where
       ptr  = W# (aToWord# a)
       tag  = ptr .&. fromIntegral tAG_MASK -- ((1 `shiftL` TAG_BITS) -1)
       addr = ptr - tag
       pad_out ls = '0':'x':ls

-- |This takes an arbitrary value and puts it into a box.
-- Note that calls like
--
-- > asBox (head list)
--
-- will put the thunk \"head list\" into the box, /not/ the element at the head
-- of the list. For that, use careful case expressions:
--
-- > case list of x:_ -> asBox x
asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

-- | Boxes can be compared, but this is not pure, as different heap objects can,
-- after garbage collection, become the same object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = case reallyUnsafePtrEqualityUpToTag# a b of
    0# -> pure False
    _  -> pure True


------------------------------------------------------------------------
-- Closures
type Closure = GenClosure Box

-- | This is the representation of a Haskell value on the heap. It reflects
-- <https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/Closures.h>
--
-- The data type is parametrized by `b`: the type to store references in.
-- Usually this is a 'Box' with the type synonym 'Closure'.
--
-- All Heap objects have the same basic layout. A header containing a pointer to
-- the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects>
-- for more information.
data GenClosure b
  = -- | A data constructor
    ConstrClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        , pkg        :: !String         -- ^ Package name
        , modl       :: !String         -- ^ Module name
        , name       :: !String         -- ^ Constructor name
        }

    -- | A function
  | FunClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk, an expression not obviously in head normal form
  | ThunkClosure
        { info       :: !StgInfoTable
        , ptrArgs    :: ![b]            -- ^ Pointer arguments
        , dataArgs   :: ![Word]         -- ^ Non-pointer arguments
        }

    -- | A thunk which performs a simple selection operation
  | SelectorClosure
        { info       :: !StgInfoTable
        , selectee   :: !b              -- ^ Pointer to the object being
                                        --   selected from
        }

    -- | An unsaturated function application
  | PAPClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Arity of the partial application
        , n_args     :: !HalfWord       -- ^ Size of the payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
    -- | A function application
  | APClosure
        { info       :: !StgInfoTable
        , arity      :: !HalfWord       -- ^ Always 0
        , n_args     :: !HalfWord       -- ^ Size of payload in words
        , fun        :: !b              -- ^ Pointer to a 'FunClosure'
        , payload    :: ![b]            -- ^ Sequence of already applied
                                        --   arguments
        }

    -- | A suspended thunk evaluation
  | APStackClosure
        { info       :: !StgInfoTable
        , fun        :: !b              -- ^ Function closure
        , payload    :: ![b]            -- ^ Stack right before suspension
        }

    -- | A pointer to another closure, introduced when a thunk is updated
    -- to point at its value
  | IndClosure
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ Target closure
        }

   -- | A byte-code object (BCO) which can be interpreted by GHC's byte-code
   -- interpreter (e.g. as used by GHCi)
  | BCOClosure
        { info       :: !StgInfoTable
        , instrs     :: !b              -- ^ A pointer to an ArrWords
                                        --   of instructions
        , literals   :: !b              -- ^ A pointer to an ArrWords
                                        --   of literals
        , bcoptrs    :: !b              -- ^ A pointer to an ArrWords
                                        --   of byte code objects
        , arity      :: !HalfWord       -- ^ The arity of this BCO
        , size       :: !HalfWord       -- ^ The size of this BCO in words
        , bitmap     :: ![Word]         -- ^ An StgLargeBitmap describing the
                                        --   pointerhood of its args/free vars
        }

    -- | A thunk under evaluation by another thread
  | BlackholeClosure
        { info       :: !StgInfoTable
        , indirectee :: !b              -- ^ The target closure
        }

    -- | A @ByteArray#@
  | ArrWordsClosure
        { info       :: !StgInfoTable
        , bytes      :: !Word           -- ^ Size of array in bytes
        , arrWords   :: ![Word]         -- ^ Array payload
        }

    -- | A @MutableByteArray#@
  | MutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccSize    :: !Word           -- ^ ?? Closures.h vs ClosureMacros.h
        , mccPayload :: ![b]            -- ^ Array payload
        -- Card table ignored
        }

    -- | A @SmallMutableArray#@
    --
    -- @since 8.10.1
  | SmallMutArrClosure
        { info       :: !StgInfoTable
        , mccPtrs    :: !Word           -- ^ Number of pointers
        , mccPayload :: ![b]            -- ^ Array payload
        }

  -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
    { info       :: !StgInfoTable
    , queueHead  :: !b              -- ^ Pointer to head of queue
    , queueTail  :: !b              -- ^ Pointer to tail of queue
    , value      :: !b              -- ^ Pointer to closure
    }

    -- | An @IOPort#@, with a queue of thread state objects blocking on them
  | IOPortClosure
        { info       :: !StgInfoTable
        , queueHead  :: !b              -- ^ Pointer to head of queue
        , queueTail  :: !b              -- ^ Pointer to tail of queue
        , value      :: !b              -- ^ Pointer to closure
        }

    -- | A @MutVar#@
  | MutVarClosure
        { info       :: !StgInfoTable
        , var        :: !b              -- ^ Pointer to contents
        }

    -- | An STM blocking queue.
  | BlockingQueueClosure
        { info       :: !StgInfoTable
        , link       :: !b              -- ^ ?? Here so it looks like an IND
        , blackHole  :: !b              -- ^ The blackhole closure
        , owner      :: !b              -- ^ The owning thread state object
        , queue      :: !b              -- ^ ??
        }

  | WeakClosure
        { info        :: !StgInfoTable
        , cfinalizers :: !b
        , key         :: !b
        , value       :: !b
        , finalizer   :: !b
        , weakLink    :: !(Maybe b) -- ^ next weak pointer for the capability
        }

  -- | Representation of StgTSO: A Thread State Object. The values for
  -- 'what_next', 'why_blocked' and 'flags' are defined in @Constants.h@.
  | TSOClosure
      { info                :: !StgInfoTable
      -- pointers
      , link                :: !b
      , global_link         :: !b
      , tsoStack            :: !b -- ^ stackobj from StgTSO
      , trec                :: !b
      , blocked_exceptions  :: !b
      , bq                  :: !b
      , thread_label        :: !(Maybe b)
      -- values
      , what_next           :: !WhatNext
      , why_blocked         :: !WhyBlocked
      , flags               :: ![TsoFlags]
      , threadId            :: !Word64
      , saved_errno         :: !Word32
      , tso_dirty           :: !Word32 -- ^ non-zero => dirty
      , alloc_limit         :: !Int64
      , tot_stack_size      :: !Word32
      , prof                :: !(Maybe StgTSOProfInfo)
      }

  -- | Representation of StgStack: The 'tsoStack ' of a 'TSOClosure'.
  | StackClosure
      { info            :: !StgInfoTable
      , stack_size      :: !Word32 -- ^ stack size in *words*
      , stack_dirty     :: !Word8 -- ^ non-zero => dirty
      , stack_marking   :: !Word8
      }

    ------------------------------------------------------------
    -- Unboxed unlifted closures

    -- | Primitive Int
  | IntClosure
        { ptipe      :: PrimType
        , intVal     :: !Int }

    -- | Primitive Word
  | WordClosure
        { ptipe      :: PrimType
        , wordVal    :: !Word }

    -- | Primitive Int64
  | Int64Closure
        { ptipe      :: PrimType
        , int64Val   :: !Int64 }

    -- | Primitive Word64
  | Word64Closure
        { ptipe      :: PrimType
        , word64Val  :: !Word64 }

    -- | Primitive Addr
  | AddrClosure
        { ptipe      :: PrimType
        , addrVal    :: !(Ptr ()) }

    -- | Primitive Float
  | FloatClosure
        { ptipe      :: PrimType
        , floatVal   :: !Float }

    -- | Primitive Double
  | DoubleClosure
        { ptipe      :: PrimType
        , doubleVal  :: !Double }

    -----------------------------------------------------------
    -- Anything else

    -- | Another kind of closure
  | OtherClosure
        { info       :: !StgInfoTable
        , hvalues    :: ![b]
        , rawWords   :: ![Word]
        }

  | UnsupportedClosure
        { info       :: !StgInfoTable
        }

    -- | A primitive word from a bitmap encoded stack frame payload
    --
    -- The type itself cannot be restored (i.e. it might represent a Word8#
    -- or an Int#).
  |  UnknownTypeWordSizedPrimitive
        { wordVal :: !Word }
  deriving (Show, Generic, Functor, Foldable, Traversable)

-- | Get the info table for a heap closure, or Nothing for a prim value
--
-- @since 9.14.1
getClosureInfoTbl_maybe :: GenClosure b -> Maybe StgInfoTable
{-# INLINE getClosureInfoTbl_maybe #-} -- Ensure we can get rid of the just box
getClosureInfoTbl_maybe closure = case closure of
  ConstrClosure{info} ->Just info
  FunClosure{info} ->Just info
  ThunkClosure{info} ->Just info
  SelectorClosure{info} ->Just info
  PAPClosure{info} ->Just info
  APClosure{info} ->Just info
  APStackClosure{info} ->Just info
  IndClosure{info} ->Just info
  BCOClosure{info} ->Just info
  BlackholeClosure{info} ->Just info
  ArrWordsClosure{info} ->Just info
  MutArrClosure{info} ->Just info
  SmallMutArrClosure{info} ->Just info
  MVarClosure{info} ->Just info
  IOPortClosure{info} ->Just info
  MutVarClosure{info} ->Just info
  BlockingQueueClosure{info} ->Just info
  WeakClosure{info} ->Just info
  TSOClosure{info} ->Just info
  StackClosure{info} ->Just info

  IntClosure{} -> Nothing
  WordClosure{} -> Nothing
  Int64Closure{} -> Nothing
  Word64Closure{} -> Nothing
  AddrClosure{} -> Nothing
  FloatClosure{} -> Nothing
  DoubleClosure{} -> Nothing

  OtherClosure{info} -> Just info
  UnsupportedClosure {info} -> Just info

  UnknownTypeWordSizedPrimitive{} -> Nothing

-- | Partial version of getClosureInfoTbl_maybe for when we know we deal with a
-- heap closure.
--
-- @since 9.14.1
getClosureInfoTbl :: HasCallStack => GenClosure b -> StgInfoTable
getClosureInfoTbl closure = case getClosureInfoTbl_maybe closure of
  Just info -> info
  Nothing -> error "getClosureInfoTbl - Closure without info table"

-- | Get the info table for a heap closure, or Nothing for a prim value
--
-- @since 9.14.1
getClosurePtrArgs_maybe :: GenClosure b -> Maybe [b]
{-# INLINE getClosurePtrArgs_maybe #-} -- Ensure we can get rid of the just box
getClosurePtrArgs_maybe closure = case closure of
  ConstrClosure{ptrArgs} -> Just ptrArgs
  FunClosure{ptrArgs} -> Just ptrArgs
  ThunkClosure{ptrArgs} -> Just ptrArgs
  SelectorClosure{} -> Nothing
  PAPClosure{} -> Nothing
  APClosure{} -> Nothing
  APStackClosure{} -> Nothing
  IndClosure{} -> Nothing
  BCOClosure{} -> Nothing
  BlackholeClosure{} -> Nothing
  ArrWordsClosure{} -> Nothing
  MutArrClosure{} -> Nothing
  SmallMutArrClosure{} -> Nothing
  MVarClosure{} -> Nothing
  IOPortClosure{} -> Nothing
  MutVarClosure{} -> Nothing
  BlockingQueueClosure{} -> Nothing
  WeakClosure{} -> Nothing
  TSOClosure{} -> Nothing
  StackClosure{} -> Nothing

  IntClosure{} -> Nothing
  WordClosure{} -> Nothing
  Int64Closure{} -> Nothing
  Word64Closure{} -> Nothing
  AddrClosure{} -> Nothing
  FloatClosure{} -> Nothing
  DoubleClosure{} -> Nothing

  OtherClosure{} -> Nothing
  UnsupportedClosure{} -> Nothing

  UnknownTypeWordSizedPrimitive{} -> Nothing

-- | Partial version of getClosureInfoTbl_maybe for when we know we deal with a
-- heap closure.
--
-- @since 9.14.1
getClosurePtrArgs :: HasCallStack => GenClosure b -> [b]
getClosurePtrArgs closure = case getClosurePtrArgs_maybe closure of
  Just ptrs -> ptrs
  Nothing -> error "getClosurePtrArgs - Closure without ptrArgs field"

type StgStackClosure = GenStgStackClosure Box

-- | A decoded @StgStack@ with `StackFrame`s
--
-- Stack related data structures (`GenStgStackClosure`, `GenStackField`,
-- `GenStackFrame`) are defined separately from `GenClosure` as their related
-- functions are very different. Though, both are closures in the sense of RTS
-- structures, their decoding logic differs: While it's safe to keep a reference
-- to a heap closure, the garbage collector does not update references to stack
-- located closures.
--
-- Additionally, stack frames don't appear outside of the stack. Thus, keeping
-- `GenStackFrame` and `GenClosure` separated, makes these types more precise
-- (in the sense what values to expect.)
data GenStgStackClosure b = GenStgStackClosure
      { ssc_info            :: !StgInfoTable
      , ssc_stack_size      :: !Word32 -- ^ stack size in *words*
      , ssc_stack           :: ![GenStackFrame b]
      }
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackField = GenStackField Box

-- | Bitmap-encoded payload on the stack
data GenStackField b
    -- | A non-pointer field
    = StackWord !Word
    -- | A pointer field
    | StackBox  !b
  deriving (Foldable, Functor, Generic, Show, Traversable)

type StackFrame = GenStackFrame Box

-- | A single stack frame
data GenStackFrame b =
   UpdateFrame
      { info_tbl           :: !StgInfoTable
      , updatee            :: !b
      }

  | CatchFrame
      { info_tbl            :: !StgInfoTable
      , handler             :: !b
      }

  | CatchStmFrame
      { info_tbl            :: !StgInfoTable
      , catchFrameCode      :: !b
      , handler             :: !b
      }

  | CatchRetryFrame
      { info_tbl            :: !StgInfoTable
      , running_alt_code    :: !Word
      , first_code          :: !b
      , alt_code            :: !b
      }

  | AtomicallyFrame
      { info_tbl            :: !StgInfoTable
      , atomicallyFrameCode :: !b
      , result              :: !b
      }

  | UnderflowFrame
      { info_tbl            :: !StgInfoTable
      , nextChunk           :: !(GenStgStackClosure b)
      }

  | StopFrame
      { info_tbl            :: !StgInfoTable }

  | RetSmall
      { info_tbl            :: !StgInfoTable
      , stack_payload       :: ![GenStackField b]
      }

  | RetBig
      { info_tbl            :: !StgInfoTable
      , stack_payload       :: ![GenStackField b]
      }

  | RetFun
      { info_tbl            :: !StgInfoTable
      , retFunSize          :: !Word
      , retFunFun           :: !b
      , retFunPayload       :: ![GenStackField b]
      }

  |  RetBCO
      { info_tbl            :: !StgInfoTable
      , bco                 :: !b -- ^ always a BCOClosure
      , bcoArgs             :: ![GenStackField b]
      }
  deriving (Foldable, Functor, Generic, Show, Traversable)

data PrimType
  = PInt
  | PWord
  | PInt64
  | PWord64
  | PAddr
  | PFloat
  | PDouble
  deriving (Eq, Show, Generic, Ord)

data WhatNext
  = ThreadRunGHC
  | ThreadInterpret
  | ThreadKilled
  | ThreadComplete
  | WhatNextUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data WhyBlocked
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  | BlockedOnDoProc
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible
  | BlockedOnMsgThrowTo
  | ThreadMigrating
  | WhyBlockedUnknownValue Word16 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

data TsoFlags
  = TsoLocked
  | TsoBlockx
  | TsoInterruptible
  | TsoStoppedOnBreakpoint
  | TsoMarked
  | TsoSqueezed
  | TsoAllocLimit
  | TsoFlagsUnknownValue Word32 -- ^ Please report this as a bug
  deriving (Eq, Show, Generic, Ord)

-- | For generic code, this function returns all referenced closures.
allClosures :: GenClosure b -> [b]
allClosures (ConstrClosure {..}) = ptrArgs
allClosures (ThunkClosure {..}) = ptrArgs
allClosures (SelectorClosure {..}) = [selectee]
allClosures (IndClosure {..}) = [indirectee]
allClosures (BlackholeClosure {..}) = [indirectee]
allClosures (APClosure {..}) = fun:payload
allClosures (PAPClosure {..}) = fun:payload
allClosures (APStackClosure {..}) = fun:payload
allClosures (BCOClosure {..}) = [instrs,literals,bcoptrs]
allClosures (ArrWordsClosure {}) = []
allClosures (MutArrClosure {..}) = mccPayload
allClosures (SmallMutArrClosure {..}) = mccPayload
allClosures (MutVarClosure {..}) = [var]
allClosures (MVarClosure {..}) = [queueHead,queueTail,value]
allClosures (IOPortClosure {..}) = [queueHead,queueTail,value]
allClosures (FunClosure {..}) = ptrArgs
allClosures (BlockingQueueClosure {..}) = [link, blackHole, owner, queue]
allClosures (WeakClosure {..}) = [cfinalizers, key, value, finalizer] ++ Data.Foldable.toList weakLink
allClosures (OtherClosure {..}) = hvalues
allClosures _ = []

-- | Get the size of the top-level closure in words.
-- Includes header and payload. Does not follow pointers.
--
-- @since 8.10.1
closureSize :: Box -> Int
closureSize (Box x) = I# (closureSize# x)
