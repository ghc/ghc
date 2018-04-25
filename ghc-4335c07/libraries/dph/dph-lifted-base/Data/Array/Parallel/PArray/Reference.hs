
-- | Reference implementation of operators on unvectorised parallel arrays.
--
--   * In this module we just used boxed vectors as the array representation. 
--     This won't be fast, but it means we can write the operators without
--     needing type class dictionaries such as PA. This makes them
--     much easier to use as reference code.
--
--   * The operators should also all do bounds checks, sanity checks, and 
--     give nice error messages if something is wrong. The ideas is that
--     this code can be run side-by-side production code during debugging.
---
--   TODO: check lengths properly in functions like zip, extracts
--
module Data.Array.Parallel.PArray.Reference
        ( PArray
        , valid
        , nf
        
        -- * Constructors
        , empty
        , singleton,    singletonl
        , replicate,    replicatel,     replicates,     replicates'
        , append,       appendl
        , concat,       concatl
        , unconcat
        , nestUSegd
        
        -- * Projections
        , length,       lengthl
        , index,        indexl
        , extract,      extracts,       extracts'
        , slice,        slicel
        , takeUSegd
        
        -- * Pack and Combine
        , pack,         packl
        , packByTag
        , combine2
        
        -- * Enumerations
        , enumFromTo,   enumFromTol
        
        -- * Tuples
        , zip,          zipl
        , unzip,        unzipl
        
        -- * Conversions
        , fromVector,   toVector
        , fromList,     toList
        , fromUArray,   toUArray
        , fromUArray2)
where
import Data.Array.Parallel.Base                 (Tag)
import Data.Vector                              (Vector)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Control.Monad
import Prelude hiding
        ( replicate, length, concat
        , enumFromTo
        , zip, unzip)

-------------------------------------------------------------------------------
here :: String -> String
here str   = "Data.Array.Parallel.PArray." ++ str

die :: String -> String -> a
die fn str = error (here $ fn ++ " " ++ str)

-- Array Type -----------------------------------------------------------------
type PArray a
        = Vector a
        
-- | Lift a unary array operator.
lift1   :: (a -> b) -> PArray a -> PArray b
lift1 f vec
        = V.map f vec


-- | Lift a binary array operator.
lift2   :: (a -> b -> c) -> PArray a -> PArray b -> PArray c
lift2 f vec1 vec2
        | V.length vec1 /= V.length vec2
        = die "lift2" "length mismatch"
 
        | otherwise
        = V.zipWith f vec1 vec2


-- | Lift a trinary array operator
lift3   :: (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d
lift3 f vec1 vec2 vec3
        |   V.length vec1 /= V.length vec2
         || V.length vec1 /= V.length vec3
        = die "lift3" "length mismatch"
 
        | otherwise
        = V.zipWith3 f vec1 vec2 vec3


-- Basics ---------------------------------------------------------------------
-- | Check that an array has a valid internal representation.
valid :: PArray a -> Bool
valid _ = True

-- | Force an array to normal form.
nf :: PArray a -> ()
nf _    = ()


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
empty :: PArray a
empty           = V.empty


-- | O(1). Produce an array containing a single element.
singleton :: a -> PArray a
singleton       = V.singleton


-- | O(n). Produce an array of singleton arrays.
singletonl :: PArray a -> PArray (PArray a)
singletonl      = lift1 singleton


-- | O(n). Define an array of the given size, that maps all elements to the same value.
replicate :: Int -> a -> PArray a
replicate       = V.replicate


-- | O(sum lengths). Lifted replicate.
replicatel :: PArray Int -> PArray a -> PArray (PArray a)
replicatel      = lift2 replicate


-- | O(sum lengths). Segmented replicate.
replicates :: U.Segd -> PArray a -> PArray a
replicates segd vec
        | V.length vec /= U.lengthSegd segd
        = die "replicates" $ unlines
                [ "segd length mismatch"
                , "  segd length  = " ++ (show $ U.lengthSegd segd)
                , "  array length = " ++ (show $ V.length vec)]

        | otherwise
        = join 
        $ V.zipWith V.replicate
                (V.convert $ U.lengthsSegd segd)
                vec


-- | O(sum lengths). Wrapper for segmented replicate that takes replication counts
--  and uses them to build the `U.Segd`.
replicates' :: PArray Int -> PArray a -> PArray a
replicates' reps arr
        = replicates (U.lengthsToSegd $ V.convert $ reps) arr


-- | Append two arrays.
append :: PArray a -> PArray a -> PArray a
append          = (V.++)


-- | Lifted append.
appendl :: PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl         = lift2 append


-- | Concatenation
concat :: PArray (PArray a) -> PArray a
concat          = join


-- | Lifted concatenation.
concatl :: PArray (PArray (PArray a)) -> PArray (PArray a)
concatl         = lift1 concat


-- | Impose a nesting structure on a flat array
unconcat :: PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat arr1 arr2
        = nestUSegd (takeUSegd arr1) arr2


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
nestUSegd :: U.Segd -> PArray a -> PArray (PArray a)
nestUSegd segd vec
        | U.elementsSegd segd     == V.length vec 
        = V.zipWith
                (\start len -> V.slice start len vec)
                (V.convert $ U.indicesSegd segd)
                (V.convert $ U.lengthsSegd segd)

        | otherwise
        = error $ unlines
                [ "Data.Array.Parallel.PArray.nestSegd: number of elements defined by "
                        ++ "segment descriptor and data array do not match"
                , " length of segment desciptor = " ++ (show $ U.elementsSegd segd)
                , " length of data array        = " ++ (show $ V.length vec)]
{-# NOINLINE nestUSegd #-}


-- Projections ----------------------------------------------------------------
-- | Take the length of an array
length :: PArray a -> Int
length          = V.length


-- | Take the length of some arrays.
lengthl :: PArray (PArray a) -> PArray Int
lengthl         = lift1 length


-- | Lookup a single element from the source array.
index :: PArray a -> Int -> a
index           = (V.!)


-- | Lookup a several elements from several source arrays.
indexl :: PArray (PArray a) -> PArray Int -> PArray a
indexl          = lift2 index


-- | Extract a range of elements from an array.
extract :: PArray a -> Int -> Int -> PArray a
extract vec start len
        = V.slice start len vec


-- | Segmented extract.
extracts :: Vector (PArray a) -> U.SSegd -> PArray a
extracts arrs ssegd
        = join
        $ V.zipWith3
                (\src start len -> extract (arrs V.! src) start len)
                (V.convert $ U.sourcesOfSSegd ssegd)
                (V.convert $ U.startsOfSSegd  ssegd)
                (V.convert $ U.lengthsOfSSegd ssegd)


-- | Wrapper for `extracts` that takes arrays of sources, starts and lengths of
--   the segments, and uses these to build the `U.SSegd`.
extracts' 
        :: Vector (PArray a) 
        -> PArray Int           -- ^ id of source array for each segment.
        -> PArray Int           -- ^ starting index of each segment in its source array.
        -> PArray Int           -- ^ length of each segment.
        -> PArray a
extracts' arrs sources starts lengths
 = let  segd    = U.lengthsToSegd $ V.convert lengths
        ssegd   = U.mkSSegd 
                        (V.convert $ starts)
                        (V.convert $ sources)
                        segd
   in   extracts arrs ssegd


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
slice :: Int -> Int -> PArray a -> PArray a
slice start len arr
        = extract arr start len


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
slicel :: PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel  = lift3 slice


-- | Take the segment descriptor from a nested array. This can cause index space
--   overflow if the number of elements in the result does not can not be
--   represented by a single machine word.
takeUSegd :: PArray (PArray a) -> U.Segd
takeUSegd vec
        = U.lengthsToSegd 
        $ V.convert
        $ V.map length vec
        

-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack    :: PArray a -> PArray Bool -> PArray a
pack xs bs
        | V.length xs /= V.length bs
        = die "pack" $ unlines
                [ "array length mismatch"
                , "  data  length = " ++ (show $ V.length xs)
                , "  flags length = " ++ (show $ V.length bs) ]

        | otherwise
        = V.ifilter (\i _ -> bs V.! i) xs


-- | Lifted pack.
packl :: PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl   = lift2 pack


-- | Filter an array based on some tags.
packByTag :: PArray a -> U.Array Tag -> Tag -> PArray a
packByTag xs tags tag
        | V.length xs /= U.length tags
        = die "packByTag" $ unlines
                [ "array length mismatch"
                , "  data  length = " ++ (show $ V.length xs)
                , "  flags length = " ++ (show $ U.length tags) ]

        | otherwise
        = V.ifilter (\i _ -> U.index (here "packByTag") tags i == tag) xs


-- | Combine two arrays based on a selector.
combine2 :: U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 tags vec1 vec2
 = let  go [] [] []                     = []
        go (0 : bs) (x : xs) ys         = x : go bs xs ys
        go (1 : bs) xs       (y : ys)   = y : go bs xs ys
        go _ _ _ = error "Data.Array.Parallel.PArray.combine: length mismatch"
 
   in   V.fromList
                $ go    (V.toList $ V.convert $ U.tagsSel2 tags)
                        (V.toList vec1)
                        (V.toList vec2)


-- Enumerations ---------------------------------------------------------------
-- | Construct a range of integers
enumFromTo :: Int -> Int -> PArray Int
enumFromTo      = V.enumFromTo


-- | Lifted enumeration
enumFromTol :: PArray Int -> PArray Int -> PArray (PArray Int)
enumFromTol     = lift2 enumFromTo


-- Tuples ---------------------------------------------------------------------
-- | O(n). Zip a pair of arrays into an array of pairs.
zip     :: PArray a -> PArray b -> PArray (a, b)
zip     = V.zip


-- | Lifted zip
zipl    :: PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl    = lift2 zip


-- | O(n). Unzip an array of pairs into a pair of arrays.
unzip   :: PArray (a, b) -> (PArray a, PArray b)
unzip   = V.unzip


-- | Lifted unzip
unzipl  :: PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
unzipl  = lift1 unzip


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
fromVector :: Vector a -> PArray a
fromVector = id


-- | Convert a `PArray` to a `Vector`        
toVector   :: PArray a -> Vector a
toVector   = id

-- | Convert a list to a `PArray`.
fromList   :: [a] -> PArray a
fromList   = V.fromList

-- | Convert a `PArray` to a list.
toList     :: PArray a -> [a]
toList     = V.toList


-- | Convert a `U.Array` to a `PArray`
fromUArray :: U.Elt a => U.Array a -> PArray a
fromUArray  = V.convert


-- | Convert a `PArray` to a `U.Array`
toUArray   :: U.Elt a => PArray a -> U.Array a
toUArray    = V.convert


-- | Convert a `U.Array` of tuples to a `PArray`
fromUArray2
        :: (U.Elt a, U.Elt b)
        => U.Array (a, b) -> PArray (a, b)
        
fromUArray2  = V.convert
