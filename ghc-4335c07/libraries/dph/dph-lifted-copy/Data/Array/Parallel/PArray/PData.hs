-- | Defines the family of types that store parallel array data, 
--   and the operators we can apply to it.
--
module Data.Array.Parallel.PArray.PData (
  PData, PDatas,
  PR(..),

  -- These types have corresponding members in the PR class.
  T_emptyPR,
  T_replicatePR,
  T_replicatelPR,
  T_repeatPR,
  T_indexPR,
  T_extractPR,
  T_bpermutePR,
  T_appPR,
  T_applPR,
  T_packByTagPR,
  T_combine2PR,
  T_updatePR,
  T_fromListPR,
  T_nfPR
)
where 
import qualified Data.Array.Parallel.Unlifted   as U
import Data.Array.Parallel.Base                 (Tag)
import GHC.Exts                                 (Int#)
import SpecConstr


-- | Parallel Data.
--   This is the family of types that store parallel array data.
--
--   PData takes the type of an element and produces the type we use to store
--   an array of those elements. The instances for PData use an efficient
--   representation that depends on the type of elements being stored.
--   For example, an array of pairs is stored as two separate arrays, one for
--   each element type. This lets us avoid storing the intermediate Pair/Tuple
--   constructors and the pointers to the elements.
-- 
--   Most of the instances are defined in "Data.Array.Parallel.PArray.Instances",
--   though the instances for function closures are defined in their own module, 
--   "Data.Array.Parallel.Lifted.Closure".
--
--   Note that PData is just a flat chunk of memory containing elements, and doesn't
--   include a field giving the length of the array. We use PArray when we want to
--   pass around the array data along with its length.
--
{-# ANN type PData NoSpecConstr #-}
data family PData a

-- | Fake type family to satisfy the vectoriser interface.
--   We don't define any PR functions for this family, but we need it to satisfy
--   the vectoriser API.
data family PDatas a

-- | A PR dictionary contains the primitive functions that operate directly
--   on parallel array data.
-- 
--   It's called PR because the functions work on our internal, efficient
--   Representation of the user-level array.
--
class PR a where
  emptyPR      :: T_emptyPR a
  replicatePR  :: T_replicatePR a
  replicatelPR :: T_replicatelPR a
  repeatPR     :: T_repeatPR a
  indexPR      :: T_indexPR a
  extractPR    :: T_extractPR a
  bpermutePR   :: T_bpermutePR a
  appPR        :: T_appPR a
  applPR       :: T_applPR a
  packByTagPR  :: T_packByTagPR a
  combine2PR   :: T_combine2PR a
  updatePR     :: T_updatePR a
  fromListPR   :: T_fromListPR a
  nfPR         :: T_nfPR a


-- Operator Types -------------------------------------------------------------
-- | An empty array.
type T_emptyPR      a 
        =  PData a


-- | Produce an array containing copies of a given element.
type T_replicatePR  a
        =  Int#                 -- number of copies \/ elements in resulting array
        -> a                    -- element to replicate
        -> PData a


-- | Segmented replicate.
type T_replicatelPR a
        =  U.Segd               -- segment descriptor of result array
        -> PData a 
        -> PData a


-- | Produce an array containing copies of some other array.
type T_repeatPR a
        =  Int#                 -- number of times to repeat
        -> Int#                 -- length of source array
        -> PData a              -- source array
        -> PData a


-- | Retrieve a numbered element from an array.
type T_indexPR a
        =  PData a              -- source array
        -> Int#                 -- index of desired element
        -> a


-- | Extract a subrange of elements from an array.
--
--   extract [:23, 42, 93, 50, 27:] 1 3  = [:42, 93, 50:]
-- 
type T_extractPR a
        =  PData a              -- source array
        -> Int#                 -- starting index
        -> Int#                 -- length of result array
        -> PData a


-- | Construct a new array by selecting elements from a source array.
--
--   bpermute [:50, 60, 20, 30:] 3 [:0, 3, 2:]  = [:50, 30, 20:]
--
type T_bpermutePR a
        =  PData a              -- source array
        -> Int#                 -- length of resulting array
        -> U.Array Int          -- indices of elements in source array
        -> PData a          


-- | Append two arrays.
type T_appPR a
        = PData a -> PData a -> PData a


-- | Segmented append.
type T_applPR a
        =  U.Segd               -- result segd
        -> U.Segd -> PData a    -- src segd/data 1
        -> U.Segd -> PData a    -- src segd/data 2
        -> PData a


-- | Select some elements from an array that correspond to a particular tag value
--	and pack them into a new array.
--
--   packByTag [:23, 42, 95, 50, 27, 49:] 3 [:1, 2, 1, 2, 3, 2:] 2 = [:42, 50, 49:]
--
type T_packByTagPR  a
        = PData a               -- source array
        -> Int#                 -- length of resulting array
        -> U.Array Tag          -- tag values of elements in source array
        -> Int#                 -- tag value of the elements to select
        -> PData a


-- | Combine two arrays based on a selector
--   The selector says which source array to choose for each element of the
--   resulting array.
type T_combine2PR a
        =  Int#                 -- length of resulting array
        -> U.Sel2               -- selector
        -> PData a              -- first source array
        -> PData a              -- second source array
        -> PData a


-- | Copy an array, but update the values of some of the elements in the result.
type T_updatePR a
        =  PData a              -- source array
        -> U.Array Int          -- indices of elements to update
        -> PData a              -- new element values. This array should have the
                                --   same length as the array of indices
        -> PData a


-- | Convert a list to an array.
type T_fromListPR a
        =  Int#                 -- length of resulting array
        -> [a]                  -- source list
        -> PData a


-- | Force an array to normal form.
type T_nfPR a
        = PData a -> ()

