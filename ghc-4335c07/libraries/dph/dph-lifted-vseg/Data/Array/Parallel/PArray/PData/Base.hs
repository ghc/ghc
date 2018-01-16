{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, UndecidableInstances, ParallelListComp #-}
-- Undeciable instances only need for derived Show instance
#include "fusion-phases.h"

-- | Parallel array types and the PR class that works on the generic
--   representation of array data.
module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , length, takeData

        , PR (..)
        , PData(..), PDatas(..)
        , bpermutePR)
where
import Data.Array.Parallel.Pretty
import GHC.Exts
import SpecConstr                               ()
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T
import Prelude hiding (length)


-- PArray ---------------------------------------------------------------------
-- | A parallel array consisting of a length field and some array data.

--   IMPORTANT: 
--   The vectoriser requires the PArray data constructor to have this specific
--   form, because it builds them explicitly. Specifically, the array length
--   must be unboxed.
--
--   TODO: Why do we need the NoSpecConstr annotation?
-- 
{-# ANN type PArray NoSpecConstr #-}
data PArray a
        = PArray Int# (PData  a)

deriving instance T.Typeable PArray


-- | Take the length field of a `PArray`.
{-# INLINE_PA length #-}
length :: PArray a -> Int
length (PArray n# _)   = (I# n#)


-- | Take the `PData` of a `PArray`.
{-# INLINE_PA takeData #-}
takeData :: PArray a -> PData a
takeData (PArray _ d)   = d


-- Parallel array data --------------------------------------------------------
-- | A chunk of parallel array data with a linear index space.
-- 
--   In contrast to a `PArray`, a `PData` may not have a fixed length, and its
--   elements may have been converted to a generic representation. Whereas a
--   `PArray` is the \"user view\" of an array, a `PData` is a type only
--   used internally to the library.

--   An example of an array with no length is PData Void. We can index this
--   at an arbitrary position, and always get a 'void' element back.
--
{-# ANN type PData NoSpecConstr #-}
data family PData a

-- | Several chunks of parallel array data.
--
--   Although a `PArray` of atomic type such as `Int` only contains a
--   single `PData` chunk, nested arrays may contain several, which we 
--   wrap up into a `PDatas`.
{-# ANN type PDatas NoSpecConstr #-}
data family PDatas a


-- Put these here to break an import loop.
data instance PData Int
        = PInt  (U.Array  Int)

data instance PDatas Int
        = PInts (U.Arrays Int)


-- PR -------------------------------------------------------------------------
-- | The PR (Parallel Representation) class holds primitive array operators that
--   work on our generic representation of data.
--
--   There are instances for all atomic types such  as `Int` and `Double`, tuples,
--   nested arrays `PData (PArray a)` and for the  generic types we used to represent
--   user level algebraic data, `Sum2` and `Wrap` and `Void`. All array data 
--   is converted to this fixed set of types.
--
--   TODO: refactor to change PData Int to U.Array Int, 
--         there's not need to wrap an extra PData constructor around these arrays,
--         and the type of bpermute is different than the others.
class PR a where

  -- House Keeping ------------------------------
  --  These methods are helpful for debugging, but we don't want their
  --  associated type classes as superclasses of PR.

  -- | (debugging) Check that an array has a well formed representation.
  --   This should only return `False` where there is a bug in the library.
  validPR       :: PData a -> Bool

  -- | (debugging) Ensure an array is fully evaluted.
  nfPR          :: PData a -> ()

  -- | (debugging) Weak equality of contained elements.
  --
  --   Returns `True` for functions of the same type. In the case of nested arrays,
  --   returns `True` if the array defines the same set of elements, but does not
  --   care about the exact form of the segement descriptors.
  similarPR     :: a -> a -> Bool

  -- | (debugging) Check that an index is within an array.
  -- 
  --   Arrays containing `Void` elements don't have a fixed length, and return 
  --   `Void` for all indices. If the array does have a fixed length, and the 
  --   flag is true, then we allow the index to be equal to this length, as
  --   well as less than it.
  coversPR      :: Bool -> PData a -> Int   -> Bool

  -- | (debugging) Pretty print the physical representation of an element.
  pprpPR        :: a       -> Doc

  -- | (debugging) Pretty print the physical representation of some array data.
  pprpDataPR    :: PData a -> Doc

  -- | (debugging) Get the representation of this type.
  --   We don't use the Typeable class for this because the vectoriser
  --   won't handle the Typeable superclass on PR.
  typeRepPR      :: a        -> T.TypeRep

  -- | (debugging) Given a 'PData a' get the representation of the 'a'
  typeRepDataPR  :: PData  a -> T.TypeRep

  -- | (debugging) Given a 'PDatas a' get the representation of the 'a'
  typeRepDatasPR :: PDatas a -> T.TypeRep


  -- Constructors -------------------------------
  -- | Produce an empty array with size zero.
  emptyPR       :: PData a

  -- | O(n). Define an array of the given size, that maps all elements to the
  --  same value.
  -- 
  --   We require the replication count to be > 0 so that it's easier to
  --   maintain the `validPR` invariants for nested arrays.
  replicatePR   :: Int -> a -> PData a

  -- | O(sum lengths). Segmented replicate.
  --   
  --   Given a Segment Descriptor (Segd), replicate each each element in the
  --   array according to the length of the corrsponding segment.
  --   The array data must define at least as many elements as there are segments
  --   in the descriptor.

  --   TODO: This takes a whole Segd instead of just the lengths. If the Segd knew
  --         that there were no zero length segments then we could implement this
  --         more efficiently in the nested case case. If there are no zeros, then
  --         all psegs in the result are reachable from the vsegs, and we wouldn't
  --         need to pack them after the replicate.
  --         
  replicatesPR  :: U.Segd -> PData a -> PData a

  -- | Append two arrays.
  appendPR      :: PData a -> PData a -> PData a

  -- | Segmented append.
  --
  --   The first descriptor defines the segmentation of the result, 
  --   and the others define the segmentation of each source array.
  appendvsPR    :: U.Segd
                -> U.VSegd -> PDatas a
                -> U.VSegd -> PDatas a
                -> PData a


  -- Projections --------------------------------
  -- | O(1). Get the length of an array, if it has one.
  --  
  --   Applying this function to an array of `Void` will yield `error`, as
  --   these arrays have no fixed length. To check array bounds, use the
  --   `coversPR` method instead, as that is a total function.
  lengthPR      :: PData a -> Int
  
  -- | O(1). Retrieve a single element from a single array.
  indexPR       :: PData a -> Int -> a

  -- | O(1). Shared indexing.
  --   Retrieve several elements from several chunks of array data, 
  --   given the chunkid and index in that chunk for each element.
  indexsPR      :: PDatas a -> U.Array (Int, Int) -> PData a

  -- | O(1). Shared indexing
  indexvsPR     :: PDatas a -> U.VSegd -> U.Array (Int, Int) -> PData a

  -- | O(slice len). Extract a slice of elements from an array,
  --  given the starting index and length of the slice.
  extractPR     :: PData a -> Int -> Int -> PData a

  -- | O(sum seglens). Shared extract.
  --  Extract several slices from several source arrays.
  --  
  --  The Scattered Segment Descriptor (`SSegd`) describes where to get each 
  --  slice, and all slices are concatenated together into the result.
  extractssPR    :: PDatas a -> U.SSegd -> PData a

  -- | O(sum seglens). Shared extract.
  --  Extract several slices from several source arrays.
  --  TODO: we're refactoring the library so functions use the VSeg form directly,
  --        instead of going via a SSegd.
  extractvsPR    :: PDatas a -> U.VSegd -> PData a
  extractvsPR pdatas vsegd
        = extractssPR pdatas (U.unsafeDemoteToSSegdOfVSegd vsegd)
  
  -- Pack and Combine ---------------------------
  -- | Select elements of an array that have their corresponding tag set to
  --   the given value. 
  --
  --   The data array must define at least as many elements as the length
  --   of the tags array. 
  packByTagPR   :: PData a -> U.Array Tag -> Tag -> PData a

  -- | Combine two arrays based on a selector.
  -- 
  --   See the documentation for selectors in the dph-prim-seq library
  --   for how this works.
  combine2PR    :: U.Sel2 -> PData a -> PData a -> PData a


  -- Conversions --------------------------------
  -- | Convert a boxed vector to an array.
  fromVectorPR  :: Vector a -> PData a

  -- | Convert an array to a boxed vector.
  toVectorPR    :: PData a -> Vector a


  -- PDatas -------------------------------------
  -- | O(1). Yield an empty collection of `PData`.
  emptydPR      :: PDatas a

  -- | O(1). Yield a singleton collection of `PData`.
  singletondPR  :: PData a  -> PDatas a

  -- | O(1). Yield how many `PData` are in the collection.
  lengthdPR     :: PDatas a -> Int

  -- | O(1). Lookup a `PData` from a collection.
  indexdPR      :: PDatas a -> Int -> PData a

  -- | O(n). Append two collections of `PData`.
  appenddPR     :: PDatas a -> PDatas a -> PDatas a

  -- | O(n). Convert a vector of `PData` to a `PDatas`.
  fromVectordPR :: V.Vector (PData a) -> PDatas a

  -- | O(n). Convert a `PDatas` to a vector of `PData`.
  toVectordPR   :: PDatas a           -> V.Vector (PData a)



-- | O(len indices) Backwards permutation.
--   Retrieve several elements from a single array.
bpermutePR :: PR a => PData a -> U.Array Int -> PData a
bpermutePR pdata ixs
 = indexsPR     (singletondPR pdata) 
                (U.zip  (U.replicate (U.length ixs) 0)
                        ixs)


-- Pretty ---------------------------------------------------------------------
instance PR a  => PprPhysical (PData a) where
 pprp = pprpDataPR

instance PR a  => PprPhysical (PDatas a) where
 pprp pdatas
  = vcat
  $ [ int n <> colon <> text " " <> pprpDataPR pd
        | n  <- [0..]
        | pd <- V.toList $ toVectordPR pdatas]

