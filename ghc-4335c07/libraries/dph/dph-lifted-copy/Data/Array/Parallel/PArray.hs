
-- | Parallel Arrays.
---
--   Parallel arrays use a fixed generic representation. All data stored in
--   them is converted to the generic representation, and we have a small
--   number of operators that work on arrays of these generic types.
--
--   Representation types include Ints, Floats, Tuples and Sums, so arrays of
--   these types can be stored directly. However, user defined algebraic data
--   needs to be converted as we don't have operators that work directly on
--   arrays of these types.
--
--   The top-level PArray type is built up from several type families and
--   clases:
--
--     PArray        - This is the top level type. It holds an array length, 
--                     and array data in the generic representation (PData).
--
--     PRepr         - Family of types that can be converted to the generic
--                     representation. We supply instances for basic types
--                     like Ints Floats etc, but the vectoriser needs to make
--                     the instances for user-defined data types itself.
--      PA class     - Contains methods to convert to and from the generic
--                     representation (PData).
-- 
--     PData         - Family of types that can be stored directly in parallel
--                     arrays. We supply all the PData instances we need here
--                     in the library.
--      PR class     - Contains methods that work directly on parallel arrays.
--                     Most of these are just wrappers for the corresponding
--                     U.Array operators.
--
--     Scalar class  - Contains methods to convert between the generic 
--                     representation (PData) and plain U.Arrays.
--
--  Note that the PRepr family and PA class are related.
--     so are the PData family and PR class.
--
--  For motivational material see:
--    "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--  For discussion of how the mapping to generic types works see:
--    "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
module Data.Array.Parallel.PArray (
  PArray, PA, Random(..),

  -- * Evaluation
  nf,

  -- * Constructors
  empty,
  singleton,
  replicate,
  (+:+),
  concat,
  nestUSegd,
  
  -- * Projections
  length,
  (!:),
  slice,

  -- * Update
  update,
  
  -- * Pack and Combine
  pack,
  bpermute,
  
  -- * Enumerations
  enumFromTo,
  indexed,
  
  -- * Tuples
  zip,
  unzip,
  
  -- * Conversions
  fromList,     toList,
  fromUArray,   toUArray,
  fromUArray2,
  fromUArray3
) 
where
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.PArray.PReprInstances        ()
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Scalar
import Data.Array.Parallel.Base.Text
import qualified Data.Array.Parallel.Unlifted           as U
import qualified System.Random as R
import Prelude          hiding ( length, replicate, zip, unzip, enumFromTo, concat )


-- NOTE: 
-- Most of these functions just export the corresponding "vectorised" 
-- function from "Data.Array.Parallel.Lifted.Closure". We don't export
-- higher-order functions like map and filter because the versions 
-- from there want closures as their function parameters.
--
-- Instead, we should probably move the first-order "vectorised" 
-- functions from D.A.P.L.Closure into this module, and just define
-- the higher-order and lifted ones there.
--
-- We still want to export these plain PArray functions to make it easier
-- to convert between vectorised and unvectorised code in benchmarks.
--

-- | O(1). An empty array, with no elements.
empty :: PA a => PArray a
{-# INLINE empty #-}
empty = emptyPA


-- | O(1). Retrieve a numbered element from an array.
(!:) :: PA a => PArray a -> Int -> a
{-# INLINE (!:) #-}
(!:) = indexPA_v


-- | O(1). Yield the length of an array.
length :: PA a => PArray a -> Int
{-# INLINE length #-}
length = lengthPA_v


-- | O(n). Produce an array containing copies of a given element.
replicate :: PA a => Int -> a -> PArray a
{-# INLINE replicate #-}
replicate = replicatePA_v


-- | O(1). Produce an array containing a single element.
singleton :: PA a => a -> PArray a
{-# INLINE singleton #-}
singleton = singletonPA_v


-- | O(1). Takes two arrays and returns an array of corresponding pairs.
--         If one array is short, excess elements of the longer array are
--         discarded.
zip :: (PA a, PA b) => PArray a -> PArray b -> PArray (a,b)
{-# INLINE zip #-}
zip = zipPA_v


-- | O(1). Transform an array into an array of the first components,
--         and an array of the second components.
unzip :: (PA a, PA b) => PArray (a,b) -> (PArray a, PArray b)
{-# INLINE unzip #-}
unzip = unzipPA_v


-- | Select the elements of an array that have their tag set as True.
--   
-- @
-- packPA [12, 24, 42, 93] [True, False, False, True]
--  = [24, 42]
-- @
pack :: PA a => PArray a -> PArray Bool -> PArray a
{-# INLINE pack #-}
pack = packPA_v



-- | Concatenate an array of arrays into a single array.
concat :: PA a => PArray (PArray a) -> PArray a
{-# INLINE concat #-}
concat = concatPA_v


-- | Append two arrays
(+:+) :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE (+:+) #-}
(+:+) = appPA_v


-- | O(n). Tag each element of an array with its index.
--
--   @indexed [42, 93, 13] = [(0, 42), (1, 93), (2, 13)]@ 
--
indexed :: PA a => PArray a -> PArray (Int, a)
{-# INLINE indexed #-}
indexed = indexedPA_v


-- | Extract a subrange of elements from an array.
--   The first argument is the starting index, while the second is the 
--   length of the slice.
--  
slice :: PA a => Int -> Int -> PArray a -> PArray a
{-# INLINE slice #-}
slice = slicePA_v


-- | Copy the source array in the destination, using new values for the given indices.
update :: PA a => PArray a -> PArray (Int,a) -> PArray a
{-# INLINE update #-}
update = updatePA_v


-- | O(n). Backwards permutation of array elements.
--
--   @bpermute [50, 60, 20, 30] [0, 3, 2]  = [50, 30, 20]@
--
bpermute :: PA a => PArray a -> PArray Int -> PArray a
{-# INLINE bpermute #-}
bpermute  = bpermutePA_v


-- | O(n). Generate a range of @Int@s.
enumFromTo :: Int -> Int -> PArray Int
{-# INLINE enumFromTo #-}
enumFromTo = enumFromToPA_v


-- Conversion -----------------------------------------------------------------
-- | Create a `PArray` from a list.
fromList :: PA a => [a] -> PArray a
{-# INLINE fromList #-}
fromList = fromListPA

-- | Create a list from a `PArray`.
toList :: PA a => PArray a -> [a]
toList xs = [indexPA_v xs i | i <- [0 .. length xs - 1]]


instance (PA a, Show a) => Show (PArray a) where
  showsPrec n xs = showsApp n "fromList<PArray>" (toList xs)


-- Evaluation -----------------------------------------------------------------
-- | Ensure an array is fully evaluated.
nf :: PA a => PArray a -> ()
nf = nfPA


-- Randoms --------------------------------------------------------------------
class Random a where
  randoms  :: R.RandomGen g => Int -> g -> PArray a
  randomRs :: R.RandomGen g => Int -> (a, a) -> g -> PArray a

prim_randoms :: (Scalar a, R.Random a, R.RandomGen g) => Int -> g -> PArray a
prim_randoms n = fromUArray . U.randoms n

prim_randomRs :: (Scalar a, R.Random a, R.RandomGen g) => Int -> (a, a) -> g -> PArray a
prim_randomRs n r = fromUArray . U.randomRs n r

instance Random Int where
  randoms = prim_randoms
  randomRs = prim_randomRs

instance Random Double where
  randoms = prim_randoms
  randomRs = prim_randomRs

