{-# LANGUAGE MagicHash, UnboxedTuples, CPP, ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes, BangPatterns, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternGuards #-}
{-|
Module      :  GHC.HeapView
Copyright   :  (c) 2012 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>

With this module, you can investigate the heap representation of Haskell
values, i.e. to investigate sharing and lazy evaluation.
-}


module GHC.HeapView (
    -- * Heap data types
    GenClosure(..),
    Closure,
    allPtrs,
    ClosureType(..),
    StgInfoTable(..),
    HalfWord,
    -- * Reading from the heap
    getClosureData,
    getBoxedClosureData,
    getClosureRaw,
    -- * Pretty printing
    ppClosure,

    -- * Boxes
    Box(..),
    asBox,
    areBoxesEqual,
    )
    where

import GHC.Exts         ( Any,
                          Ptr(..), Addr#, Int(..), Word(..), Word#, Int#,
                          ByteArray#, Array#, sizeofByteArray#, sizeofArray#, indexArray#, indexWordArray#,
                          unsafeCoerce# )

import GHC.Arr          (Array(..))


import Foreign          hiding ( void )
import Numeric          ( showHex )
import Data.Char
import Data.List
import Data.Functor
import Data.Foldable    ( Foldable )
import Data.Traversable ( Traversable )
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Exception.Base (evaluate)

#include "ghcautoconf.h"

-- | An arbitrarily Haskell value in a safe Box. The point is that even
-- unevaluated thunks can safely be moved around inside the Box, and when
-- required, e.g. in 'getBoxedClosureData', the function knows how far it has
-- to evalue the argument.
data Box = Box Any

#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#else
type HalfWord = Word16
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
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls =
          '0':'x':(replicate (2*wORD_SIZE - length ls) '0') ++ ls

-- | Boxes can be compared, but this is not pure, as different heap objects can,
-- after garbage collection, become the same object.
areBoxesEqual :: Box -> Box -> IO Bool
areBoxesEqual (Box a) (Box b) = case reallyUnsafePtrEqualityUpToTag# a b of
    0# -> return False
    _  -> return True


{-|
  This takes an arbitrary value and puts it into a box. Note that calls like

  > asBox (head list)

  will put the thunk \"head list\" into the box, /not/ the element at the head
  of the list. For that, use careful case expressions:

  > case list of x:_ -> asBox x
-}
asBox :: a -> Box
asBox x = Box (unsafeCoerce# x)

{-
   StgInfoTable parsing derived from ByteCodeItbls.lhs
   Removed the code parameter for now
   Replaced Type by an enumeration
   Remove stuff dependent on GHCI_TABLES_NEXT_TO_CODE
 -}

{-| This is a somewhat faithful representation of an info table. See
   <http://hackage.haskell.org/trac/ghc/browser/includes/rts/storage/InfoTables.h>
   for more details on this data structure. Note that the 'Storable' instance
   provided here does _not_ support writing.
 -}
data StgInfoTable = StgInfoTable {
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: ClosureType,
   srtlen :: HalfWord
  }
  deriving (Show)

instance Storable StgInfoTable where

   sizeOf itbl
      = sum
        [
         fieldSz ptrs itbl,
         fieldSz nptrs itbl,
         sizeOf (undefined :: HalfWord),
         fieldSz srtlen itbl
        ]

   alignment _
      = wORD_SIZE

   poke _a0 _itbl
      = error "Storable StgInfoTable is read-only"

   peek a0
      = flip (evalStateT) (castPtr a0)
      $ do
           ptrs'   <- load
           nptrs'  <- load
           tipe'   <- load
           srtlen' <- load
           return
              StgInfoTable {
                 ptrs   = ptrs',
                 nptrs  = nptrs',
                 tipe   = toEnum (fromIntegral (tipe'::HalfWord)),
                 srtlen = srtlen'
              }

fieldSz :: Storable b => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

load :: Storable a => PtrIO a
load = do addr <- advance
          lift (peek addr)

type PtrIO = StateT (Ptr Word8) IO

advance :: Storable a => PtrIO (Ptr a)
advance = StateT adv where
    adv addr = case castPtr addr of { addrCast -> return
        (addrCast, addr `plusPtr` sizeOfPointee addrCast) }

sizeOfPointee :: (Storable a) => Ptr a -> Int
sizeOfPointee addr = sizeOf (typeHack addr)
    where typeHack = undefined :: Ptr a -> a

{-
   Data Type representing Closures
 -}


{-| A closure type enumeration, in order matching the actual value on the heap.
   Needs to be synchronized with
   <http://hackage.haskell.org/trac/ghc/browser/includes/rts/storage/ClosureTypes.h>
 -}
data ClosureType
    = INVALID_OBJECT
    | CONSTR
    | CONSTR_1_0
    | CONSTR_0_1
    | CONSTR_2_0
    | CONSTR_1_1
    | CONSTR_0_2
    | CONSTR_NOCAF
    | FUN
    | FUN_1_0
    | FUN_0_1
    | FUN_2_0
    | FUN_1_1
    | FUN_0_2
    | FUN_STATIC
    | THUNK
    | THUNK_1_0
    | THUNK_0_1
    | THUNK_2_0
    | THUNK_1_1
    | THUNK_0_2
    | THUNK_STATIC
    | THUNK_SELECTOR
    | BCO
    | AP
    | PAP
    | AP_STACK
    | IND
    | IND_STATIC
    | RET_BCO
    | RET_SMALL
    | RET_BIG
    | RET_FUN
    | UPDATE_FRAME
    | CATCH_FRAME
    | UNDERFLOW_FRAME
    | STOP_FRAME
    | BLOCKING_QUEUE
    | BLACKHOLE
    | MVAR_CLEAN
    | MVAR_DIRTY
    | TVAR
    | ARR_WORDS
    | MUT_ARR_PTRS_CLEAN
    | MUT_ARR_PTRS_DIRTY
    | MUT_ARR_PTRS_FROZEN0
    | MUT_ARR_PTRS_FROZEN
    | MUT_VAR_CLEAN
    | MUT_VAR_DIRTY
    | WEAK
    | PRIM
    | MUT_PRIM
    | TSO
    | STACK
    | TREC_CHUNK
    | ATOMICALLY_FRAME
    | CATCH_RETRY_FRAME
    | CATCH_STM_FRAME
    | WHITEHOLE
    | SMALL_MUT_ARR_PTRS_CLEAN
    | SMALL_MUT_ARR_PTRS_DIRTY
    | SMALL_MUT_ARR_PTRS_FROZEN0
    | SMALL_MUT_ARR_PTRS_FROZEN
    | COMPACT_NFDATA
    | N_CLOSURE_TYPES
 deriving (Show, Eq, Enum, Ord)

{-| This is the main data type of this module, representing a Haskell value on
  the heap. This reflects
  <http://hackage.haskell.org/trac/ghc/browser/includes/rts/storage/Closures.h>

  The data type is parametrized by the type to store references in, which
  is usually a 'Box' with appropriate type synonym 'Closure'.
 -}
data GenClosure b
  = ConsClosure
        { info       :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
        , pkg        :: String
        , modl       :: String
        , name       :: String
        }
  | ThunkClosure
        { info       :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
        }
  | SelectorClosure
        { info       :: StgInfoTable
        , selectee   :: b
        }
  | IndClosure
        { info       :: StgInfoTable
        , indirectee   :: b
        }
  | BlackholeClosure
        { info       :: StgInfoTable
        , indirectee   :: b
        }
    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
  | APClosure
        { info       :: StgInfoTable
        , arity      :: HalfWord
        , n_args     :: HalfWord
        , fun        :: b
        , payload    :: [b]
        }
  | PAPClosure
        { info       :: StgInfoTable
        , arity      :: HalfWord
        , n_args     :: HalfWord
        , fun        :: b
        , payload    :: [b]
        }
  | APStackClosure
        { info       :: StgInfoTable
        , fun        :: b
        , payload    :: [b]
        }
  | BCOClosure
        { info       :: StgInfoTable
        , instrs     :: b
        , literals   :: b
        , bcoptrs    :: b
        , arity      :: HalfWord
        , size       :: HalfWord
        , bitmap     :: Word
        }
  | ArrWordsClosure
        { info       :: StgInfoTable
        , bytes      :: Word
        , arrWords   :: [Word]
        }
  | MutArrClosure
        { info       :: StgInfoTable
        , mccPtrs    :: Word
        , mccSize    :: Word
        , mccPayload :: [b]
        -- Card table ignored
        }
  | MutVarClosure
        { info       :: StgInfoTable
        , var        :: b
        }
  | MVarClosure
        { info       :: StgInfoTable
        , queueHead  :: b
        , queueTail  :: b
        , value      :: b
        }
  | FunClosure
        { info       :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
        }
  | BlockingQueueClosure
        { info       :: StgInfoTable
        , link       :: b
        , blackHole  :: b
        , owner      :: b
        , queue      :: b
        }
  | OtherClosure
        { info       :: StgInfoTable
        , hvalues    :: [b]
        , rawWords   :: [Word]
        }
  | UnsupportedClosure
        { info       :: StgInfoTable
        }
 deriving (Show, Functor, Foldable, Traversable)


type Closure = GenClosure Box

-- | For generic code, this function returns all referenced closures.
allPtrs :: GenClosure b -> [b]
allPtrs (ConsClosure {..}) = ptrArgs
allPtrs (ThunkClosure {..}) = ptrArgs
allPtrs (SelectorClosure {..}) = [selectee]
allPtrs (IndClosure {..}) = [indirectee]
allPtrs (BlackholeClosure {..}) = [indirectee]
allPtrs (APClosure {..}) = fun:payload
allPtrs (PAPClosure {..}) = fun:payload
allPtrs (APStackClosure {..}) = fun:payload
allPtrs (BCOClosure {..}) = [instrs,literals,bcoptrs]
allPtrs (ArrWordsClosure {..}) = []
allPtrs (MutArrClosure {..}) = mccPayload
allPtrs (MutVarClosure {..}) = [var]
allPtrs (MVarClosure {..}) = [queueHead,queueTail,value]
allPtrs (FunClosure {..}) = ptrArgs
allPtrs (BlockingQueueClosure {..}) = [link, blackHole, owner, queue]
allPtrs (OtherClosure {..}) = hvalues
allPtrs (UnsupportedClosure {..}) = []


foreign import prim "aToWordzh" aToWord# :: Any -> Word#
foreign import prim "slurpClosurezh" slurpClosure# :: Any -> (# Addr#, ByteArray#, Array# b #)
foreign import prim "reallyUnsafePtrEqualityUpToTag" reallyUnsafePtrEqualityUpToTag# :: Any -> Any -> Int#

--pClosure x = do
--    getClosure x >>= print

-- | This returns the raw representation of the given argument. The second
-- component of the triple are the words on the heap, and the third component
-- are those words that are actually pointers. Once back in Haskell word, the
-- 'Word'  may be outdated after a garbage collector run, but the corresponding
-- 'Box' will still point to the correct value.
getClosureRaw :: a -> IO (Ptr StgInfoTable, [Word], [Box])
getClosureRaw x =
    case slurpClosure# (unsafeCoerce# x) of
        (# iptr, dat, ptrs #) -> do
            let nelems = (I# (sizeofByteArray# dat)) `div` wORD_SIZE
                rawWords = [W# (indexWordArray# dat i) | I# i <- [0.. fromIntegral nelems -1] ]
                pelems = I# (sizeofArray# ptrs)
                ptrList = amap' Box $ Array 0 (pelems - 1) pelems ptrs
            -- This is just for good measure, and seems to be not important.
            mapM_ evaluate ptrList
            -- This seems to be required to avoid crashes as well
            void $ evaluate nelems
            -- The following deep evaluation is crucial to avoid crashes (but why)?
            mapM_ evaluate rawWords
            return (Ptr iptr, rawWords, ptrList)

-- From compiler/ghci/RtClosureInspect.hs
amap' :: (t -> b) -> Array Int t -> [b]
amap' f (Array i0 i _ arr#) = map g [0 .. i - i0]
    where g (I# i#) = case indexArray# arr# i# of
                          (# e #) -> f e

-- derived from vacuum-1.0.0.2/src/GHC/Vacuum/Internal.hs, which got it from
-- compiler/ghci/DebuggerUtils.hs
dataConInfoPtrToNames :: Ptr StgInfoTable -> IO (String, String, String)
dataConInfoPtrToNames ptr = do
    conDescAddress <- getConDescAddress ptr
    wl <- peekArray0 0 conDescAddress
    let (pkg, modl, name) = parse wl
    return (b2s pkg, b2s modl, b2s name)
  where
    b2s :: [Word8] -> String
    b2s = fmap (chr . fromIntegral)

    getConDescAddress :: Ptr StgInfoTable -> IO (Ptr Word8)
    getConDescAddress ptr'
      | True = do
          offsetToString <- peek (ptr' `plusPtr` (negate wORD_SIZE))
          return $ (ptr' `plusPtr` stdInfoTableSizeB)
                    `plusPtr` (fromIntegral (offsetToString :: Word))
    -- This is code for !ghciTablesNextToCode:
    {-
      | otherwise = peek . intPtrToPtr
                      . (+ fromIntegral
                            stdInfoTableSizeB)
                        . ptrToIntPtr $ ptr
    -}

    -- hmmmmmm. Is there any way to tell this?
    opt_SccProfilingOn = False

    stdInfoTableSizeW :: Int
    -- The size of a standard info table varies with profiling/ticky etc,
    -- so we can't get it from Constants
    -- It must vary in sync with mkStdInfoTable
    stdInfoTableSizeW
      = size_fixed + size_prof
      where
        size_fixed = 2  -- layout, type
        size_prof | opt_SccProfilingOn = 2
                  | otherwise    = 0

    stdInfoTableSizeB :: Int
    stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE

-- From vacuum-1.0.0.2/src/GHC/Vacuum/Internal.hs
parse :: [Word8] -> ([Word8], [Word8], [Word8])
parse input = if not . all (>0) . fmap length $ [pkg,modl,occ]
                --then (error . concat)
                --        ["getConDescAddress:parse:"
                --        ,"(not . all (>0) . fmap le"
                --        ,"ngth $ [pkg,modl,occ]"]
                then ([], [], input) -- Not in the pkg.modl.occ format, for example END_TSO_QUEUE
                else (pkg, modl, occ)
--   = ASSERT (all (>0) (map length [pkg, modl, occ])) (pkg, modl, occ)   -- XXXXXXXXXXXXXXXX
  where
        (pkg, rest1) = break (== fromIntegral (ord ':')) input
        (modl, occ)
            = (concat $ intersperse [dot] $ reverse modWords, occWord)
            where
            (modWords, occWord) = if (length rest1 < 1) --  XXXXXXXXx YUKX
                                    --then error "getConDescAddress:parse:length rest1 < 1"
                                    then parseModOcc [] []
                                    else parseModOcc [] (tail rest1)
        -- ASSERT (length rest1 > 0) (parseModOcc [] (tail rest1))
        dot = fromIntegral (ord '.')
        parseModOcc :: [[Word8]] -> [Word8] -> ([[Word8]], [Word8])
        parseModOcc acc str
            = case break (== dot) str of
                (top, []) -> (acc, top)
                (top, _:bot) -> parseModOcc (top : acc) bot


-- | This function returns parsed heap representation of the argument _at this
-- moment_, even if it is unevaluated or an indirection or other exotic stuff.
-- Beware when passing something to this function, the same caveats as for
-- 'asBox' apply.
getClosureData :: a -> IO Closure
getClosureData x = do
    (iptr, wds, ptrs) <- getClosureRaw x
    itbl <- peek iptr
    case tipe itbl of
        t | t >= CONSTR && t <= CONSTR_NOCAF -> do
            (pkg, modl, name) <- dataConInfoPtrToNames iptr
            if modl == "ByteCodeInstr" && name == "BreakInfo"
              then return $ UnsupportedClosure itbl
              else return $ ConsClosure itbl ptrs (drop (length ptrs + 1) wds) pkg modl name

        t | t >= THUNK && t <= THUNK_STATIC -> do
            return $ ThunkClosure itbl ptrs (drop (length ptrs + 2) wds)

        t | t >= FUN && t <= FUN_STATIC -> do
            return $ FunClosure itbl ptrs (drop (length ptrs + 1) wds)

        AP -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to AP"
            unless (length wds >= 3) $
                fail "Expected at least 3 raw words to AP"
            return $ APClosure itbl
                (fromIntegral $ wds !! 2)
                (fromIntegral $ shiftR (wds !! 2) (wORD_SIZE_IN_BITS `div` 2))
                (head ptrs) (tail ptrs)

        PAP -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to PAP"
            unless (length wds >= 3) $
                fail "Expected at least 3 raw words to AP"
            return $ PAPClosure itbl
                (fromIntegral $ wds !! 2)
                (fromIntegral $ shiftR (wds !! 2) (wORD_SIZE_IN_BITS `div` 2))
                (head ptrs) (tail ptrs)

        AP_STACK -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to AP_STACK"
            return $ APStackClosure itbl (head ptrs) (tail ptrs)

        THUNK_SELECTOR -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to THUNK_SELECTOR"
            return $ SelectorClosure itbl (head ptrs)

        IND -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to IND"
            return $ IndClosure itbl (head ptrs)
        IND_STATIC -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to IND_STATIC"
            return $ IndClosure itbl (head ptrs)
        BLACKHOLE -> do
            unless (length ptrs >= 1) $
                fail "Expected at least 1 ptr argument to BLACKHOLE"
            return $ BlackholeClosure itbl (head ptrs)

        BCO -> do
            unless (length ptrs >= 3) $
                fail $ "Expected at least 3 ptr argument to BCO, found " ++ show (length ptrs)
            unless (length wds >= 6) $
                fail $ "Expected at least 6 words to BCO, found " ++ show (length wds)
            return $ BCOClosure itbl (ptrs !! 0) (ptrs !! 1) (ptrs !! 2)
                (fromIntegral $ wds !! 4)
                (fromIntegral $ shiftR (wds !! 4) (wORD_SIZE_IN_BITS `div` 2))
                (wds !! 5)

        ARR_WORDS -> do
            unless (length wds >= 2) $
                fail $ "Expected at least 2 words to ARR_WORDS, found " ++ show (length wds)
            return $ ArrWordsClosure itbl (wds !! 1) (drop 2 wds)

        t | t == MUT_ARR_PTRS_FROZEN || t == MUT_ARR_PTRS_FROZEN0 -> do
            unless (length wds >= 3) $
                fail $ "Expected at least 3 words to MUT_ARR_PTRS_FROZEN0 found " ++ show (length wds)
            return $ MutArrClosure itbl (wds !! 1) (wds !! 2) ptrs

        t | t == MUT_VAR_CLEAN || t == MUT_VAR_DIRTY ->
            return $ MutVarClosure itbl (head ptrs)

        t | t == MVAR_CLEAN || t == MVAR_DIRTY -> do
            unless (length ptrs >= 3) $
                fail $ "Expected at least 3 ptrs to MVAR, found " ++ show (length ptrs)
            return $ MVarClosure itbl (ptrs !! 0) (ptrs !! 1) (ptrs !! 2)

        BLOCKING_QUEUE ->
            return $ OtherClosure itbl ptrs wds
        --    return $ BlockingQueueClosure itbl
        --        (ptrs !! 0) (ptrs !! 1) (ptrs !! 2) (ptrs !! 3)

        --  return $ OtherClosure itbl ptrs wds
        --
        _ ->
            return $ UnsupportedClosure itbl

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a


isChar :: GenClosure b -> Maybe Char
isChar (ConsClosure { name = "C#", dataArgs = [ch], ptrArgs = []}) = Just (chr (fromIntegral ch))
isChar _ = Nothing

isCons :: GenClosure b -> Maybe (b, b)
isCons (ConsClosure { name = ":", dataArgs = [], ptrArgs = [h,t]}) = Just (h,t)
isCons _ = Nothing

isTup :: GenClosure b -> Maybe [b]
isTup (ConsClosure { dataArgs = [], ..}) =
    if length name >= 3 &&
       head name == '(' && last name == ')' &&
       all (==',') (tail (init name))
    then Just ptrArgs else Nothing
isTup _ = Nothing


-- | A pretty-printer that tries to generate valid Haskell for evalutated data.
-- It assumes that for the included boxes, you already replaced them by Strings
-- using 'Data.Foldable.map' or, if you need to do IO, 'Data.Foldable.mapM'.
--
-- The parameter gives the precedendence, to avoid avoidable parenthesises.
ppClosure :: (Int -> b -> String) -> Int -> GenClosure b -> String
ppClosure showBox prec c = case c of
    _ | Just ch <- isChar c -> app $
        ["C#", show ch]
    _ | Just (h,t) <- isCons c -> addBraces (5 <= prec) $
        showBox 5 h ++ " : " ++ showBox 4 t
    _ | Just vs <- isTup c ->
        "(" ++ intercalate "," (map (showBox 0) vs) ++ ")"
    ConsClosure {..} -> app $
        name : map (showBox 10) ptrArgs ++ map show dataArgs
    ThunkClosure {..} -> app $
        "_thunk" : map (showBox 10) ptrArgs ++ map show dataArgs
    SelectorClosure {..} -> app
        ["_sel", showBox 10 selectee]
    IndClosure {..} -> app
        ["_ind", showBox 10 indirectee]
    BlackholeClosure {..} -> app
        ["_bh",  showBox 10 indirectee]
    APClosure {..} -> app $ map (showBox 10) $
        fun : payload
    PAPClosure {..} -> app $ map (showBox 10) $
        fun : payload
    APStackClosure {..} -> app $ map (showBox 10) $
        fun : payload
    BCOClosure {..} -> app
        ["_bco"]
    ArrWordsClosure {..} -> app
        ["toArray", "("++show (length arrWords) ++ " words)", intercalate "," (shorten (map show arrWords)) ]
    MutArrClosure {..} -> app
        ["toMutArray", "("++show (length mccPayload) ++ " ptrs)",  intercalate "," (shorten (map (showBox 10) mccPayload))]
    MutVarClosure {..} -> app $
        ["_mutVar", (showBox 10) var]
    MVarClosure {..} -> app $
        ["MVar", (showBox 10) value]
    FunClosure {..} ->
        "_fun" ++ braceize (map (showBox 0) ptrArgs ++ map show dataArgs)
    BlockingQueueClosure {..} ->
        "_blockingQueue"
    OtherClosure {..} ->
        "_other"
    UnsupportedClosure {..} ->
        "_unsupported"
  where
    app [a] = a  ++ "()"
    app xs = addBraces (10 <= prec) (intercalate " " xs)

    shorten xs = if length xs > 20 then take 20 xs ++ ["(and more)"] else xs


-- Utilities

addBraces :: Bool -> String -> String
addBraces True t = "(" ++ t ++ ")"
addBraces False t = t

braceize :: [String] -> String
braceize [] = ""
braceize xs = "{" ++ intercalate "," xs ++ "}"

-- This used to be available via GHC.Constants
#include "MachDeps.h"
wORD_SIZE, tAG_MASK, wORD_SIZE_IN_BITS :: Int
wORD_SIZE = SIZEOF_HSWORD
tAG_MASK = (1 `shift` TAG_BITS) - 1
wORD_SIZE_IN_BITS = WORD_SIZE_IN_BITS

