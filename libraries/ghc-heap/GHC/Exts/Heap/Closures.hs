{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module GHC.Exts.Heap.Closures (
    -- * Closures
      Closure
    , GenClosure(..)
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , RetFunType(..)
    , allClosures

    -- * Boxes
    , Box(..)
    , areBoxesEqual
    , asBox
#if MIN_TOOL_VERSION_ghc(9,7,0)
    , StackFrameIter(..)
#endif
    ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.Constants
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable

-- `ghc -M` currently doesn't properly account for ways when generating
-- dependencies (#15197). This import ensures correct build-ordering between
-- this module and GHC.Exts.Heap.InfoTableProf. It should be removed when #15197
-- is fixed.
import GHC.Exts.Heap.InfoTableProf ()
#endif

import GHC.Exts.Heap.ProfInfo.Types

import Data.Bits
import Data.Foldable (toList)
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import Numeric

#if MIN_TOOL_VERSION_ghc(9,7,0)
import GHC.Stack.CloneStack (StackSnapshot(..), stackSnapshotToString)
import GHC.Exts.Stack.Constants
#endif

------------------------------------------------------------------------
-- Boxes

foreign import prim "aToWordzh" aToWord# :: Any -> Word#

foreign import prim "reallyUnsafePtrEqualityUpToTag"
    reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#

#if MIN_TOOL_VERSION_ghc(9,7,0)
-- | Iterator state for stack decoding
data StackFrameIter =
  -- | Represents a `StackClosure` / @StgStack@
  SfiStackClosure
    { stackSnapshot# :: !StackSnapshot# }
  -- | Represents a closure on the stack
  | SfiClosure
    { stackSnapshot# :: !StackSnapshot#,
      index :: !WordOffset
    }
  -- | Represents a primitive word on the stack
  | SfiPrimitive
    { stackSnapshot# :: !StackSnapshot#,
      index :: !WordOffset
    }

instance Eq StackFrameIter where
  (SfiStackClosure s1#) == (SfiStackClosure s2#) =
    (StackSnapshot s1#) == (StackSnapshot s2#)
  (SfiClosure s1# i1) == (SfiClosure s2# i2) =
    (StackSnapshot s1#) == (StackSnapshot s2#)
    && i1 == i2
  (SfiPrimitive s1# i1) == (SfiPrimitive s2# i2) =
    (StackSnapshot s1#) == (StackSnapshot s2#)
    && i1 == i2
  _ == _ = False

instance Show StackFrameIter where
   showsPrec _ (SfiStackClosure s#) rs =
    "SfiStackClosure { stackSnapshot# = " ++ stackSnapshotToString (StackSnapshot s#) ++ "}" ++ rs
   showsPrec _ (SfiClosure s# i ) rs =
    "SfiClosure { stackSnapshot# = " ++ stackSnapshotToString (StackSnapshot s#) ++ show i ++ ", " ++ "}" ++ rs
   showsPrec _ (SfiPrimitive s# i ) rs =
    "SfiPrimitive { stackSnapshot# = " ++ stackSnapshotToString (StackSnapshot s#) ++ show i ++ ", " ++ "}" ++ rs

-- | An arbitrary Haskell value in a safe Box.
--
-- The point is that even unevaluated thunks can safely be moved around inside
-- the Box, and when required, e.g. in 'getBoxedClosureData', the function knows
-- how far it has to evaluate the argument.
--
-- `Box`es can be used to increase (and enforce) laziness: In a graph of
-- closures they can act as a barrier of evaluation. `Closure` is an example for
-- this.
data Box =
  -- | A heap located closure.
  Box Any
  -- | A value or reference to a value on the stack.
  | StackFrameBox StackFrameIter
#else
-- | An arbitrary Haskell value in a safe Box. The point is that even
-- unevaluated thunks can safely be moved around inside the Box, and when
-- required, e.g. in 'getBoxedClosureData', the function knows how far it has
-- to evaluate the argument.
data Box = Box Any
#endif

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
#if MIN_TOOL_VERSION_ghc(9,7,0)
   showsPrec _ (StackFrameBox sfi) rs =
    "(StackFrameBox " ++ show sfi ++ ")" ++ rs
#endif

-- | Boxes can be compared, but this is not pure, as different heap objects can,
-- after garbage collection, become the same object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = case reallyUnsafePtrEqualityUpToTag# a b of
    0# -> pure False
    _  -> pure True
#if MIN_TOOL_VERSION_ghc(9,7,0)
areBoxesEqual (StackFrameBox sfi1) (StackFrameBox sfi2) =
  pure $ sfi1 == sfi2
areBoxesEqual _ _ = pure False
#endif

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
#if __GLASGOW_HASKELL__ >= 811
      , stack_marking   :: !Word8
#endif
      -- | The frames of the stack. Only available if a cloned stack was
      -- decoded, otherwise empty.
      , stack           :: ![b]
      }

#if MIN_TOOL_VERSION_ghc(9,7,0)
  | UpdateFrame
      { info            :: !StgInfoTable
      , updatee :: !b
      }

  | CatchFrame
      { info            :: !StgInfoTable
      , exceptions_blocked :: Word
      , handler :: !b
      }

  | CatchStmFrame
      { info            :: !StgInfoTable
      , catchFrameCode :: !b
      , handler :: !b
      }

  | CatchRetryFrame
      { info            :: !StgInfoTable
      , running_alt_code :: !Word
      , first_code :: !b
      , alt_code :: !b
      }

  | AtomicallyFrame
      { info            :: !StgInfoTable
      , atomicallyFrameCode :: !b
      , result :: !b
      }

  | UnderflowFrame
      { info            :: !StgInfoTable
      , nextChunk       :: !b
      }

  | StopFrame
      { info            :: !StgInfoTable }

  | RetSmall
      { info            :: !StgInfoTable
      , payload :: ![b]
      }

  | RetBig
      { info            :: !StgInfoTable
      , payload :: ![b]
      }

  | RetFun
      { info            :: !StgInfoTable
      , retFunType :: RetFunType
      , retFunSize :: Word
      , retFunFun :: !b
      , retFunPayload :: ![b]
      }

  |  RetBCO
      { info            :: !StgInfoTable
      , bco :: !b -- must be a BCOClosure
      , bcoArgs :: ![b]
      }
#endif
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

  |  UnknownTypeWordSizedPrimitive
        { wordVal :: !Word }
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data RetFunType =
      ARG_GEN     |
      ARG_GEN_BIG |
      ARG_BCO     |
      ARG_NONE    |
      ARG_N       |
      ARG_P       |
      ARG_F       |
      ARG_D       |
      ARG_L       |
      ARG_V16     |
      ARG_V32     |
      ARG_V64     |
      ARG_NN      |
      ARG_NP      |
      ARG_PN      |
      ARG_PP      |
      ARG_NNN     |
      ARG_NNP     |
      ARG_NPN     |
      ARG_NPP     |
      ARG_PNN     |
      ARG_PNP     |
      ARG_PPN     |
      ARG_PPP     |
      ARG_PPPP    |
      ARG_PPPPP   |
      ARG_PPPPPP  |
      ARG_PPPPPPP |
      ARG_PPPPPPPP
      deriving (Show, Eq, Enum, Generic)

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
#if MIN_TOOL_VERSION_ghc(9,7,0)
allClosures (StackClosure {..}) = stack
allClosures (UpdateFrame {..}) = [updatee]
allClosures (CatchFrame {..}) = [handler]
allClosures (CatchStmFrame {..}) = [catchFrameCode, handler]
allClosures (CatchRetryFrame {..}) = [first_code, alt_code]
allClosures (AtomicallyFrame {..}) = [atomicallyFrameCode, result]
allClosures (RetSmall {..}) = payload
allClosures (RetBig {..}) = payload
allClosures (RetFun {..}) = retFunFun : retFunPayload
allClosures (RetBCO {..}) = bco : bcoArgs
#endif
allClosures _ = []
