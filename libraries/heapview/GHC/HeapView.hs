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
    -- * Heap maps
    -- $heapmap
    HeapTree(..),
    buildHeapTree,
    ppHeapTree,
    HeapGraphEntry(..),
    HeapGraphIndex,
    HeapGraph(..),
    lookupHeapGraph,
    heapGraphRoot,
    buildHeapGraph,
    multiBuildHeapGraph,
    addHeapGraph,
    annotateHeapGraph,
    updateHeapGraph,
    ppHeapGraph,
    -- * Boxes
    Box(..),
    asBox,
    areBoxesEqual,
    -- * Disassembler
    disassembleBCO,
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
import Data.Maybe       ( catMaybes )
import Data.Monoid      ( Monoid, (<>), mempty )
import Data.Functor
import Data.Function
import Data.Foldable    ( Foldable )
import qualified Data.Foldable as F
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import Control.Monad
import Control.Exception.Base (evaluate)

import GHC.Disassembler

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
data ClosureType =
          INVALID_OBJECT
        | CONSTR
        | CONSTR_1_0
        | CONSTR_0_1
        | CONSTR_2_0
        | CONSTR_1_1
        | CONSTR_0_2
        | CONSTR_STATIC
        | CONSTR_NOCAF_STATIC
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
        | IND_PERM
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
 deriving (Show, Eq, Enum, Ord)

{-| This is the main data type of this module, representing a Haskell value on
  the heap. This reflects
  <http://hackage.haskell.org/trac/ghc/browser/includes/rts/storage/Closures.h>

  The data type is parametrized by the type to store references in, which
  is usually a 'Box' with appropriate type synonym 'Closure'.
 -}
data GenClosure b =
    ConsClosure {
        info         :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
        , pkg        :: String
        , modl       :: String
        , name       :: String
    } |
    ThunkClosure {
        info         :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
    } |
    SelectorClosure {
        info         :: StgInfoTable
        , selectee   :: b
    } |
    IndClosure {
        info         :: StgInfoTable
        , indirectee   :: b
    } |
    BlackholeClosure {
        info         :: StgInfoTable
        , indirectee   :: b
    } |
    -- In GHCi, if Linker.h would allow a reverse lookup, we could for exported
    -- functions fun actually find the name here.
    -- At least the other direction works via "lookupSymbol
    -- base_GHCziBase_zpzp_closure" and yields the same address (up to tags)
    APClosure {
        info         :: StgInfoTable
        , arity      :: HalfWord
        , n_args     :: HalfWord
        , fun        :: b
        , payload    :: [b]
    } |
    PAPClosure {
        info         :: StgInfoTable
        , arity      :: HalfWord
        , n_args     :: HalfWord
        , fun        :: b
        , payload    :: [b]
    } |
    APStackClosure {
        info         :: StgInfoTable
        , fun        :: b
        , payload    :: [b]
    } |
    BCOClosure {
        info         :: StgInfoTable
        , instrs     :: b
        , literals   :: b
        , bcoptrs    :: b
        , arity      :: HalfWord
        , size       :: HalfWord
        , bitmap     :: Word
    } |
    ArrWordsClosure {
        info         :: StgInfoTable
        , bytes      :: Word
        , arrWords   :: [Word]
    } |
    MutArrClosure {
        info         :: StgInfoTable
        , mccPtrs    :: Word
        , mccSize    :: Word
        , mccPayload :: [b]
        -- Card table ignored
    } |
    MutVarClosure {
        info         :: StgInfoTable
        , var        :: b
    } |
    MVarClosure {
        info         :: StgInfoTable
        , queueHead  :: b
        , queueTail  :: b
        , value      :: b
    } |
    FunClosure {
        info         :: StgInfoTable
        , ptrArgs    :: [b]
        , dataArgs   :: [Word]
    } |
    BlockingQueueClosure {
        info         :: StgInfoTable
        , link       :: b
        , blackHole  :: b
        , owner      :: b
        , queue      :: b
    } |
    OtherClosure {
        info         :: StgInfoTable
        , hvalues    :: [b]
        , rawWords   :: [Word]
    } |
    UnsupportedClosure {
        info         :: StgInfoTable
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
        t | t >= CONSTR && t <= CONSTR_NOCAF_STATIC -> do
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


isNil :: GenClosure b -> Bool
isNil (ConsClosure { name = "[]", dataArgs = [], ptrArgs = []}) = True
isNil _ = False

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

{- $heapmap

   For more global views of the heap, you can use heap maps. These come in
   variations, either a trees or as graphs, depending on
   whether you want to detect cycles and sharing or not.

   The entries of a 'HeapGraph' can be annotated with arbitrary values. Most
   operations expect this to be in the 'Monoid' class: They use 'mempty' to
   annotate closures added because the passed values reference them, and they
   use 'mappend' to combine the annotations when two values conincide, e.g.
   during 'updateHeapGraph'.
-}

-- | Heap maps as tree, i.e. no sharing, no cycles.
data HeapTree = HeapTree Box (GenClosure HeapTree) | EndOfHeapTree

heapTreeClosure :: HeapTree -> Maybe (GenClosure HeapTree)
heapTreeClosure (HeapTree _ c) = Just c
heapTreeClosure EndOfHeapTree = Nothing

-- | Constructing an 'HeapTree' from a boxed value. It takes a depth parameter
-- that prevents it from running ad infinitum for cyclic or infinite
-- structures.
buildHeapTree :: Int -> Box -> IO HeapTree
buildHeapTree 0 _ = do
    return $ EndOfHeapTree
buildHeapTree n b = do
    c <- getBoxedClosureData b
    c' <- T.mapM (buildHeapTree (n-1)) c
    return $ HeapTree b c'

-- | Pretty-Printing a heap Tree
--
-- Example output for @[Just 4, Nothing, *something*]@, where *something* is an
-- unevaluated expression depending on the command line argument.
--
-- >[Just (I# 4),Nothing,Just (_thunk ["arg1","arg2"])]
ppHeapTree :: HeapTree -> String
ppHeapTree = go 0
  where
    go _ EndOfHeapTree = "..."
    go prec t@(HeapTree _ c')
        | Just s <- isHeapTreeString t = show s
        | Just l <- isHeapTreeList t   = "[" ++ intercalate "," (map ppHeapTree l) ++ "]"
        | Just bc <- disassembleBCO heapTreeClosure c'
                                       = app ("_bco" : map (go 10) (concatMap F.toList bc))
        | otherwise                    = ppClosure go prec c'
      where
        app [a] = a ++ "()"
        app xs = addBraces (10 <= prec) (intercalate " " xs)

isHeapTreeList :: HeapTree -> Maybe ([HeapTree])
isHeapTreeList tree = do
    c <- heapTreeClosure tree
    if isNil c
      then return []
      else do
        (h,t) <- isCons c
        t' <- isHeapTreeList t
        return $ (:) h t'

isHeapTreeString :: HeapTree -> Maybe String
isHeapTreeString t = do
    list <- isHeapTreeList t
    -- We do not want to print empty lists as "" as we do not know that they
    -- are really strings.
    if (null list)
        then Nothing
        else mapM (isChar <=< heapTreeClosure) list

-- | For heap graphs, i.e. data structures that also represent sharing and
-- cyclic structures, these are the entries. If the referenced value is
-- @Nothing@, then we do not have that value in the map, most likely due to
-- exceeding the recursion bound passed to 'buildHeapGraph'.
--
-- Besides a pointer to the stored value and the closure representation we
-- also keep track of whether the value was still alive at the last update of the
-- heap graph. In addition we have a slot for arbitrary data, for the user's convenience.
data HeapGraphEntry a = HeapGraphEntry {
        hgeBox :: Box,
        hgeClosure :: GenClosure (Maybe HeapGraphIndex),
        hgeLive :: Bool,
        hgeData :: a}
    deriving (Show, Functor)
type HeapGraphIndex = Int

-- | The whole graph. The suggested interface is to only use 'lookupHeapGraph',
-- as the internal representation may change. Nevertheless, we export it here:
-- Sometimes the user knows better what he needs than we do.
newtype HeapGraph a = HeapGraph (M.IntMap (HeapGraphEntry a))
    deriving (Show)

lookupHeapGraph :: HeapGraphIndex -> (HeapGraph a) -> Maybe (HeapGraphEntry a)
lookupHeapGraph i (HeapGraph m) = M.lookup i m

heapGraphRoot :: HeapGraphIndex
heapGraphRoot = 0

-- | Creates a 'HeapGraph' for the value in the box, but not recursing further
-- than the given limit. The initial value has index 'heapGraphRoot'.
buildHeapGraph
   :: Monoid a
   => Int -- ^ Search limit
   -> a -- ^ Data value for the root
   -> Box -- ^ The value to start with
   -> IO (HeapGraph a)
buildHeapGraph limit rootD initialBox =
    fst <$> multiBuildHeapGraph limit [(rootD, initialBox)]

-- | Creates a 'HeapGraph' for the values in multiple boxes, but not recursing
--   further than the given limit.
--
--   Returns the 'HeapGraph' and the indices of initial values. The arbitrary
--   type @a@ can be used to make the connection between the input and the
--   resulting list of indices, and to store additional data.
multiBuildHeapGraph
    :: Monoid a
    => Int -- ^ Search limit
    -> [(a, Box)] -- ^ Starting values with associated data entry
    -> IO (HeapGraph a, [(a, HeapGraphIndex)])
multiBuildHeapGraph limit = generalBuildHeapGraph limit (HeapGraph M.empty)

-- | Adds an entry to an existing 'HeapGraph'.
--
--   Returns the updated 'HeapGraph' and the index of the added value.
addHeapGraph
    :: Monoid a
    => Int -- ^ Search limit
    -> a -- ^ Data to be stored with the added value
    -> Box -- ^ Value to add to the graph
    -> HeapGraph a -- ^ Graph to extend
    -> IO (HeapGraphIndex, HeapGraph a)
addHeapGraph limit d box hg = do
    (hg', [(_,i)]) <- generalBuildHeapGraph limit hg [(d,box)]
    return (i, hg')

-- | Adds the given annotation to the entry at the given index, using the
-- 'mappend' operation of its 'Monoid' instance.
annotateHeapGraph :: Monoid a => a -> HeapGraphIndex -> HeapGraph a -> HeapGraph a
annotateHeapGraph d i (HeapGraph hg) = HeapGraph $ M.update go i hg
  where
    go hge = Just $ hge { hgeData = hgeData hge <> d }

generalBuildHeapGraph
    :: Monoid a
    => Int
    -> HeapGraph a
    -> [(a,Box)]
    -> IO (HeapGraph a, [(a, HeapGraphIndex)])
generalBuildHeapGraph limit _ _ | limit <= 0 = error "buildHeapGraph: limit has to be positive"
generalBuildHeapGraph limit (HeapGraph hg) addBoxes = do
    -- First collect all boxes from the existing heap graph
    let boxList = [ (hgeBox hge, i) | (i, hge) <- M.toList hg ]
        indices | M.null hg = [0..]
                | otherwise = [1 + fst (M.findMax hg)..]

        initialState = (boxList, indices, [])
    -- It is ok to use the Monoid (IntMap a) instance here, because
    -- we will, besides the first time, use 'tell' only to add singletons not
    -- already there
    (is, hg') <- runWriterT (evalStateT run initialState)
    -- Now add the annotations of the root values
    let hg'' = foldl' (flip (uncurry annotateHeapGraph)) (HeapGraph hg') is
    return (hg'', is)
  where
    run = do
        lift $ tell hg -- Start with the initial map
        forM addBoxes $ \(d, b) -> do
            -- Cannot fail, as limit is not zero here
            Just i <- add limit b
            return (d, i)

    add 0  _ = return Nothing
    add n b = do
        -- If the box is in the map, return the index
        (existing,_,_) <- get
        mbI <- liftIO $ findM (areBoxesEqual b . fst) existing
        case mbI of
            Just (_,i) -> return $ Just i
            Nothing -> do
                -- Otherwise, allocate a new index
                i <- nextI
                -- And register it
                modify (\(x,y,z) -> ((b,i):x, y, z))
                -- Look up the closure
                c <- liftIO $ getBoxedClosureData b
                -- Find indicies for all boxes contained in the map
                c' <- T.mapM (add (n-1)) c
                -- Add add the resulting closure to the map
                lift $ tell (M.singleton i (HeapGraphEntry b c' True mempty))
                return $ Just i
    nextI = do
        i <- gets (head . (\(_,b,_) -> b))
        modify (\(a,b,c) -> (a, tail b, c))
        return i

-- | This function updates a heap graph to reflect the current state of
-- closures on the heap, conforming to the following specification.
--
--  * Every entry whose value has been garbage collected by now is marked as
--    dead by setting 'hgeLive' to @False@
--  * Every entry whose value is still live gets the 'hgeClosure' field updated
--    and newly referenced closures are, up to the given depth, added to the graph.
--  * A map mapping previous indicies to the corresponding new indicies is returned as well.
--  * The closure at 'heapGraphRoot' stays at 'heapGraphRoot'
updateHeapGraph :: Monoid a => Int -> HeapGraph a -> IO (HeapGraph a, HeapGraphIndex -> HeapGraphIndex)
updateHeapGraph limit (HeapGraph startHG) = do
    (hg', indexMap) <- runWriterT $ foldM go (HeapGraph M.empty) (M.toList startHG)
    return (hg', (M.!) indexMap)
  where
    go hg (i, hge) = do
        (j, hg') <- liftIO $ addHeapGraph limit (hgeData hge) (hgeBox hge) hg
        tell (M.singleton i j)
        return hg'

-- | Pretty-prints a HeapGraph. The resulting string contains newlines. Example
-- for @let s = \"Ki\" in (s, s, cycle \"Ho\")@:
--
-- >let x1 = "Ki"
-- >    x6 = C# 'H' : C# 'o' : x6
-- >in (x1,x1,x6)
ppHeapGraph :: HeapGraph a -> String
ppHeapGraph (HeapGraph m) = letWrapper ++ ppRef 0 (Just heapGraphRoot)
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph m) [heapGraphRoot]

    letWrapper =
        if null bindings
        then ""
        else "let " ++ intercalate "\n    " (map ppBinding bindings) ++ "\nin "

    bindingLetter i = case hgeClosure (iToE i) of
        ThunkClosure {..} -> 't'
        SelectorClosure {..} -> 't'
        APClosure {..} -> 't'
        PAPClosure {..} -> 'f'
        BCOClosure {..} -> 't'
        FunClosure {..} -> 'f'
        _ -> 'x'

    ppBindingMap = M.fromList $
        concat $
        map (zipWith (\j (i,c) -> (i, [c] ++ show j)) [(1::Int)..]) $
        groupBy ((==) `on` snd) $
        sortBy (compare `on` snd)
        [ (i, bindingLetter i) | i <- bindings ]

    ppVar i = ppBindingMap M.! i
    ppBinding i = ppVar i ++ " = " ++ ppEntry 0 (iToE i)

    ppEntry prec hge
        | Just s <- isString hge = show s
        | Just l <- isList hge   = "[" ++ intercalate "," (map (ppRef 0) l) ++ "]"
        | Just bc <- disassembleBCO (fmap (hgeClosure . iToE)) (hgeClosure hge)
                                       = app ("_bco" : map (ppRef 10) (concatMap F.toList bc))
        | otherwise = ppClosure ppRef prec (hgeClosure hge)
      where
        app [a] = a  ++ "()"
        app xs = addBraces (10 <= prec) (intercalate " " xs)

    ppRef _ Nothing = "..."
    ppRef prec (Just i) | i `elem` bindings = ppVar i
                        | otherwise = ppEntry prec (iToE i)
    iToE i = m M.! i

    iToUnboundE i = if i `elem` bindings then Nothing else M.lookup i m

    isList :: HeapGraphEntry a -> Maybe ([Maybe HeapGraphIndex])
    isList hge =
        if isNil (hgeClosure hge)
          then return []
          else do
            (h,t) <- isCons (hgeClosure hge)
            ti <- t
            e <- iToUnboundE ti
            t' <- isList e
            return $ (:) h t'

    isString :: HeapGraphEntry a -> Maybe String
    isString e = do
        list <- isList e
        -- We do not want to print empty lists as "" as we do not know that they
        -- are really strings.
        if (null list)
            then Nothing
            else mapM (isChar . hgeClosure <=< iToUnboundE <=< id) list


-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allPtrs . hgeClosure) (M.elems m)

-- | This function integrates the disassembler in "GHC.Disassembler". The first
-- argument should a function that dereferences the pointer in the closure to a
-- closure.
--
-- If any of these return 'Nothing', then 'disassembleBCO' returns Nothing
disassembleBCO :: (a -> Maybe (GenClosure b)) -> GenClosure a -> Maybe [BCI b]
disassembleBCO deref (BCOClosure {..}) = do
    opsC <- deref instrs
    litsC <- deref literals
    ptrsC  <- deref bcoptrs
    return $ disassemble (mccPayload ptrsC) (arrWords litsC) (toBytes (bytes opsC) (arrWords opsC))
disassembleBCO _ _ = Nothing

-- Utilities

findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _p [] = return Nothing
findM p (x:xs) = do
    b <- p x
    if b then return (Just x) else findM p xs

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

