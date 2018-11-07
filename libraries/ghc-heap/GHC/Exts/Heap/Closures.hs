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
    , allClosures

    -- * Boxes
    , Box(..)
    , areBoxesEqual
    , asBox
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

import Data.Bits
import Data.Int
import Data.Word
import GHC.Exts
import GHC.Generics
import Numeric

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
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls =
          '0':'x':(replicate (2*wORD_SIZE - length ls) '0') ++ ls

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
-- <http://ghc.haskell.org/trac/ghc/browser/includes/rts/storage/Closures.h>
--
-- The data type is parametrized by the type to store references in. Usually
-- this is a 'Box' with the type synonym 'Closure'.
--
-- All Heap objects have the same basic layout. A header containing a pointer
-- to the info table and a payload with various fields. The @info@ field below
-- always refers to the info table pointed to by the header. The remaining
-- fields are the payload.
--
-- See
-- <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects>
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

    -- | An @MVar#@, with a queue of thread state objects blocking on them
  | MVarClosure
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
        , addrVal    :: !Int }

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
  deriving (Show, Generic, Functor, Foldable, Traversable)


data PrimType
  = PInt
  | PWord
  | PInt64
  | PWord64
  | PAddr
  | PFloat
  | PDouble
  deriving (Eq, Show, Generic)

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
allClosures (ArrWordsClosure {..}) = []
allClosures (MutArrClosure {..}) = mccPayload
allClosures (MutVarClosure {..}) = [var]
allClosures (MVarClosure {..}) = [queueHead,queueTail,value]
allClosures (FunClosure {..}) = ptrArgs
allClosures (BlockingQueueClosure {..}) = [link, blackHole, owner, queue]
allClosures (OtherClosure {..}) = hvalues
allClosures _ = []
