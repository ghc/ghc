{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- | Create real byte-code objects from 'ResolvedBCO's.
module GHCi.CreateBCO (createBCOs) where

import Prelude -- See note [Why do we import Prelude here?]
import Data.List (sortBy)
import Data.Ord (comparing)
import GHCi.ResolvedBCO
import GHCi.RemoteTypes
import GHCi.BreakArray
import GHC.Data.SizedSeq
import Data.List (partition)
import Data.Graph

import System.IO (fixIO)
import Control.Monad
import Data.Array.Base
import Foreign hiding (newArray)
import Unsafe.Coerce (unsafeCoerce, unsafeCoerceUnlifted)
import GHC.Arr          ( Array(..) )
import GHC.Exts   hiding ( BCO, mkApUpd0#, newBCO# )
import GHC.Internal.Base ( BCO, mkApUpd0#, newBCO# )
import GHC.IO
import Control.Exception ( ErrorCall(..) )

{-
Note [Tying the knot in createBCOs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two passes for creating the BCOs:

1. Allocate unlifted static cons, which are never mutual recursive, but may refer
   to the array constructed in the second pass.

2. Allocate BCOs and lifted static cons, which may have circular references
   amongst themselves, and also refer to the unlifted cons allocated in the
   first pass.

Notably, it is crucial that all unlifted static cons are eagerly allocated,
returning an evaluated and properly tagged unlifted value, and that all
references to an unlifted static constructor use that unlifted value directly,
rather than a thunk, to preserve the unliftedness invariants. Not doing so
resulted in #25636, which was fixed by the commit introducing this Note.

The unlifted static cons must be allocated in topological order, to ensure a
reference from one to another has already been allocated and can be promptly used.

The BCOs and lifted cons are allocated in 'fixIO', where references to other
BCOs and static cons in the same group are resolved by writing into the
'PtrsArr' a thunk that indexes the recursively constructed array of BCOs.
References to unlifted cons are looked up in the array from the first pass and
must definitely not be thunks.

References from unlifted cons to BCOs are resolved similarly by constructing a
thunk into the second pass array, hence why allocating the unlifted cons must
be inside of the 'fixIO'.
-}

createBCOs :: [ResolvedBCO] -> IO [HValueRef]
createBCOs objs = do

  let (unl_objs, bcos) = partition isUnliftedObj objs

  let n_bcos = length bcos
  -- See Note [Tying the knot in createBCOs]
  (unl_hvals, hvals) <- fixIO $ \ ~(_, hvs) -> do

    let arr = listArray (0, n_bcos-1) hvs

    -- First, construct the array of unlifted static cons.
    --
    -- Top-level unlifted constructors are never mutual recursive, so we can do
    -- this by filling the array in topological order.
    --
    -- Lifted fields of unlifted data will store
    -- thunks indexing the `arr` constructed by fixIO.
    (unl_cons, unl_hvals) <- createUnliftedStaticCons unl_objs arr

    -- Second, construct the lifted BCOs and static cons which may have
    -- (circular) references to one another in this group. References from this
    -- group to the unlifted static cons will be resolved by looking them up in
    -- the array constructed in the first pass.
    hvals <- mapM (createBCO arr unl_cons) bcos
    return (unl_hvals, hvals)

  mapM mkRemoteRef (unl_hvals ++ hvals)

  where
    isUnliftedObj :: ResolvedBCO -> Bool
    isUnliftedObj = \case
      ResolvedStaticCon{..} -> resolvedStaticConIsUnlifted
      _                     -> False

createBCO :: Array Int HValue -> UnlConsArr -> ResolvedBCO -> IO HValue
createBCO _ _ obj | resolvedBCOIsLE obj /= isLittleEndian
  = throwIO (ErrorCall $
        unlines [ "The endianness of the ResolvedBCO does not match"
                , "the systems endianness. Using ghc and iserv in a"
                , "mixed endianness setup is not supported!"
                ])
createBCO arr unl_arr bco
   = do linked_thing <- linkBCO' arr unl_arr bco
        case linked_thing of
          LinkedBCO bco_arity linked_bco -> do
            -- Note [Updatable CAF BCOs]
            -- ~~~~~~~~~~~~~~~~~~~~~~~~~
            -- Why do we need mkApUpd0 here?  Otherwise top-level
            -- interpreted CAFs don't get updated after evaluation.  A
            -- top-level BCO will evaluate itself and return its value
            -- when entered, but it won't update itself.  Wrapping the BCO
            -- in an AP_UPD thunk will take care of the update for us.
            --
            -- Furthermore:
            --   (a) An AP thunk *must* point directly to a BCO
            --   (b) A zero-arity BCO *must* be wrapped in an AP thunk
            --   (c) An AP is always fully saturated, so we *can't* wrap
            --       non-zero arity BCOs in an AP thunk.
            --
            -- See #17424.
            if (bco_arity > 0)
               then return (HValue (unsafeCoerce linked_bco))
               else case mkApUpd0# linked_bco of { (# final_bco #) ->
                      return (HValue final_bco) }
          LinkedStaticCon linked_static_con -> do
            return linked_static_con
          LinkedUnliftedStaticCon linked_static_con -> do
            return $! forgetUnliftedHValue linked_static_con

-- | The resulting of linking a BCO or static constructor
data LinkedBCO
  = LinkedBCO !Int{-BCO arity-} BCO
  | LinkedStaticCon HValue
  | LinkedUnliftedStaticCon UnliftedHValue

-- | Construct an array of unlifted constructor closures given a list of 'UnliftedStaticCons'.
--
-- INVARIANT: Top-level unlifted constructors are never mutual recursive, so we
-- can do this by filling the array in topological order.
--
-- Lifted fields of unlifted data will be filled by looking them up in the
-- given array of lifted resolved objs.
createUnliftedStaticCons
  :: [ResolvedBCO]    -- ^ 'UnliftedStaticCon's ONLY.
  -> Array Int HValue -- ^ Lifted resolved objects
  -> IO (UnlConsArr, [HValue])
  -- ^ Return both the array to look up the unlifted static constrs by 'BCOIx',
  -- and a list with the same unlifted objects, albeit the unliftedness is
  -- forgotten using 'forgetUnliftedHValue' (allowing them to be put into a
  -- list and later combined with the heap values of lifted objects).
createUnliftedStaticCons objs lif_arr = do

  -- Get topologically sorted objs with their original indices
  let topoSortedObjs = topSortObjs objs
  unl_arr <- newUnlConsArr (length topoSortedObjs)

  -- Process objs in topological order, but write them at their original indexes
  indexed_vs <- forM topoSortedObjs $ \(origIdx, obj) -> case obj of
    ResolvedStaticCon{..}
      | resolvedStaticConIsUnlifted
      -> do
        -- Because we topologically sort the objs, all unlifted references we
        -- care about when linking this BCO will already be filled in.
        -- The lifted ones are resolved by knot tying (see the fixIO above).
        lbc <- linkBCO' lif_arr unl_arr obj
        case lbc of
          LinkedUnliftedStaticCon linked_static_con -> do
            writeUnlConsArr origIdx linked_static_con unl_arr -- Write it to its original index position
            return (origIdx, forgetUnliftedHValue linked_static_con)
          _ -> error "createUnliftedStaticCons: unexpected lifted ResolvedBCO"
    _ ->
      error "createUnliftedStaticCons: unexpected lifted ResolvedBCO"

  -- Return them in the original order
  let vs = map snd $ sortBy (comparing fst) indexed_vs
  return (unl_arr, vs)
  where
    -- Return the topologically sorted objects with their original index.
    topSortObjs :: [ResolvedBCO] -> [(Int, ResolvedBCO)]
    topSortObjs objs =
      let
        edges = [ ((origIdx, obj), origIdx, getUnlDeps obj)
                | (origIdx, obj) <- zip [0..] objs ]

        getUnlDeps :: ResolvedBCO -> [Int]
        getUnlDeps (ResolvedStaticCon{..}) =
          [ k | ptr <- ssElts resolvedStaticConPtrs
              , ResolvedUnliftedStaticConRef k <- [ptr] ]
        getUnlDeps _ = []

        (graph, vertexToNode, _keyToVertex) = graphFromEdges edges
        sortedVertices = topSort graph
      in
        [ ix_obj | v <- sortedVertices
                 , let (ix_obj, _, _) = vertexToNode v ]

linkBCO' :: Array Int HValue -> UnlConsArr -> ResolvedBCO -> IO LinkedBCO
linkBCO' arr unl_arr resolved_obj =
  case resolved_obj of
    ResolvedBCO{..} -> do
      let
          ptrs   = ssElts resolvedBCOPtrs
          n_ptrs = sizeSS resolvedBCOPtrs

          !(I# arity#)  = resolvedBCOArity

          insns_barr = barr (getBCOByteArray resolvedBCOInstrs)
          bitmap_barr = barr (getBCOByteArray resolvedBCOBitmap)
          literals_barr = barr (getBCOByteArray resolvedBCOLits)

      PtrsArr marr <- mkPtrsArray arr unl_arr n_ptrs ptrs
      IO $ \s ->
        case unsafeFreezeArray# marr s of { (# s, arr #) ->
        case newBCO# insns_barr literals_barr arr arity# bitmap_barr s of
          (# s, hval #) -> (# s, LinkedBCO resolvedBCOArity hval #)
        }
    ResolvedStaticCon{..} -> do

      let
        ptrs             = ssElts resolvedStaticConPtrs
        n_ptrs           = sizeSS resolvedStaticConPtrs
        !(W# data_size#) = resolvedStaticConArity

        literals_barr = barr (getBCOByteArray resolvedStaticConLits)

        !(W# itbl_ptr_w#) = resolvedStaticConInfoPtr
        !(Ptr itbl_ptr#)  = Ptr (int2Addr# (word2Int# itbl_ptr_w#))

      PtrsArr marr <- mkPtrsArray arr unl_arr n_ptrs ptrs

      IO $ \s ->
        case unsafeFreezeArray# marr s of { (# s, arr #) ->
        case newConAppObj# itbl_ptr# literals_barr arr data_size# s of
          (# s, hval #) ->
            if resolvedStaticConIsUnlifted then
              (# s, LinkedUnliftedStaticCon (UnliftedHValue (unsafeCoerce# hval)) #)
            else
              (# s, LinkedStaticCon (HValue hval) #)
        }
  where
    !(EmptyArr empty#) = emptyArr -- See Note [BCO empty array]
    barr arr# = if I# (sizeofByteArray# arr#) == 0 then empty# else arr#

-- we recursively link any sub-BCOs while making the ptrs array
mkPtrsArray :: Array Int HValue -> UnlConsArr -> Word -> [ResolvedBCOPtr] -> IO PtrsArr
mkPtrsArray arr unl_arr n_ptrs ptrs = do
  marr <- newPtrsArray (fromIntegral n_ptrs)
  let
    fill (ResolvedBCORef n) i =
      writePtrsArrayHValue i (arr ! n) marr  -- must be lazy!
    fill (ResolvedStaticConRef n) i = do
      writePtrsArrayHValue i (arr ! n) marr  -- must be lazy!
    fill (ResolvedUnliftedStaticConRef n) i = do
      -- must be strict! we want to store the unlifted con,
      -- not the arr indexing thunk.
      !unl_val <- readUnlConsArr n unl_arr
      writePtrsArrayHValue i unl_val marr
    fill (ResolvedBCOPtr r) i = do
      hv <- localRef r
      writePtrsArrayHValue i hv marr
    fill (ResolvedBCOStaticPtr r) i = do
      writePtrsArrayPtr i (fromRemotePtr r)  marr
    fill (ResolvedBCOPtrBCO bco) i = do
      obj <- linkBCO' arr unl_arr bco
      case obj of
        LinkedBCO _ bco ->
          writePtrsArrayBCO i bco marr
        LinkedStaticCon linked_static_con ->
          writePtrsArrayHValue i linked_static_con marr
        LinkedUnliftedStaticCon linked_static_con -> do
          let !unl_val = forgetUnliftedHValue linked_static_con
          writePtrsArrayHValue i unl_val marr
    fill (ResolvedBCOPtrBreakArray r) i = do
      BA mba <- localRef r
      writePtrsArrayMBA i mba marr
  zipWithM_ fill ptrs [0..]
  return marr

--------------------------------------------------------------------------------
-- * Unlifted static constructors
--------------------------------------------------------------------------------

-- | A heap closure of unlifted type
type UnliftedHValue :: UnliftedType
newtype UnliftedHValue = UnliftedHValue (Any @UnliftedType)

-- | Forget that a heap closure is unlifted, and return it as a lifted heap closure.
-- Note: Going the other way around for an arbitrary heap closure is totally unsafe!
forgetUnliftedHValue :: UnliftedHValue -> HValue
forgetUnliftedHValue (UnliftedHValue a) = HValue (unsafeCoerce# a)

-- | A lifted array with unlifted static constructor 'UnliftedHValue's
data UnlConsArr = UnlConsArr (MutableArray# RealWorld UnliftedHValue)

-- | Create a 'UnlConsArr' of the given size with all elements initialized to
-- an empty ByteArray#
newUnlConsArr :: Int -> IO UnlConsArr
newUnlConsArr (I# arr_size#) = IO $ \s ->
  -- Zero value to initialize the array.
  -- Would be better to use undefined but can't for unlifted values.
  let !(EmptyArr emp_arr#) = emptyArr
  in case newArray# arr_size# (UnliftedHValue (unsafeCoerceUnlifted emp_arr#)) s of
      (# s, arr #) -> (# s, UnlConsArr arr #)

-- | Write an unlifted contructor closure into a 'UnlConsArr'
writeUnlConsArr :: Int -> UnliftedHValue -> UnlConsArr -> IO ()
writeUnlConsArr (I# i#) unl_hval (UnlConsArr unl_arr#) = IO $ \s ->
  case writeArray# unl_arr# i# unl_hval s of
    s -> (# s, () #)

-- | Read an unlifted constructor closure from an 'UnlConsArr',
-- but forget that the heap closure is unlifted using 'forgetUnliftedHValue'.
-- This allows us to return it in @IO@ and return it in the final resolved objs list.
readUnlConsArr :: Int -> UnlConsArr -> IO HValue
readUnlConsArr (I# n#) (UnlConsArr unl_arr#) = IO $ \s ->
  case readArray# unl_arr# n# s of
    (# s, val #) -> (# s, forgetUnliftedHValue val #)

--------------------------------------------------------------------------------
-- * PtrsArr
--------------------------------------------------------------------------------

data PtrsArr = PtrsArr (MutableArray# RealWorld HValue)

newPtrsArray :: Int -> IO PtrsArr
newPtrsArray (I# i) = IO $ \s ->
  case newArray# i undefined s of (# s', arr #) -> (# s', PtrsArr arr #)

writePtrsArrayHValue :: Int -> HValue -> PtrsArr -> IO ()
writePtrsArrayHValue (I# i) hv (PtrsArr arr) = IO $ \s ->
  case writeArray# arr i hv s of s' -> (# s', () #)

writePtrsArrayPtr :: Int -> Ptr a -> PtrsArr -> IO ()
writePtrsArrayPtr (I# i) (Ptr a#) (PtrsArr arr) = IO $ \s ->
  case writeArrayAddr# arr i a# s of s' -> (# s', () #)

-- This is rather delicate: convincing GHC to pass an Addr# as an Any but
-- without making a thunk turns out to be surprisingly tricky.
{-# NOINLINE writeArrayAddr# #-}
writeArrayAddr# :: MutableArray# s a -> Int# -> Addr# -> State# s -> State# s
#if defined(javascript_HOST_ARCH)
-- Addr# isn't coercible with Any with the JS backend.
writeArrayAddr# = error "writeArrayAddr#: currently unsupported with the JS backend"
#else
writeArrayAddr# marr i addr s = unsafeCoerce# writeArray# marr i addr s
#endif

writePtrsArrayBCO :: Int -> BCO -> PtrsArr -> IO ()
writePtrsArrayBCO (I# i) bco (PtrsArr arr) = IO $ \s ->
  case (unsafeCoerce# writeArray#) arr i bco s of s' -> (# s', () #)

writePtrsArrayMBA :: Int -> MutableByteArray# s -> PtrsArr -> IO ()
writePtrsArrayMBA (I# i) mba (PtrsArr arr) = IO $ \s ->
  case (unsafeCoerce# writeArray#) arr i mba s of s' -> (# s', () #)

--------------------------------------------------------------------------------
-- * Empty array
--------------------------------------------------------------------------------
{- Note [BCO empty array]
   ~~~~~~~~~~~~~~~~~~~~~~
Lots of BCOs have empty ptrs or nptrs, but empty arrays are not free:
they are 2-word heap objects.  So let's make a single empty array and
share it between all BCOs.
-}

data EmptyArr = EmptyArr ByteArray#

{-# NOINLINE emptyArr #-}
emptyArr :: EmptyArr
emptyArr = unsafeDupablePerformIO $ IO $ \s ->
  case newByteArray# 0# s of { (# s, arr #) ->
  case unsafeFreezeByteArray# arr s of { (# s, farr #) ->
  (# s, EmptyArr farr #)
  }}


