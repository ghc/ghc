
-- | Unvectorised parallel arrays.
--
--   * These operators may be used directly by unvectorised client programs.
--
--   * They are also used by the "Data.Array.Parallel.Lifted.Combinators"
--     module to define the closure converted versions that vectorised code
--     uses.
--
--   * In general, the operators here are all unsafe and don't do bounds checks.
--     The lifted versions also don't check that each of the argument arrays
--     have the same length.
---
--   TODO: check lengths properly in functions like zip, extracts
--
module Data.Array.Parallel.PArray
        ( PArray(..), PA
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
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.Base                 (Tag)
import Data.Vector                              (Vector)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Vector                    as V
import Control.Monad
import GHC.Exts (Int(I#), (+#))
import qualified Prelude                        as P
import Prelude hiding
        ( replicate, length, concat
        , enumFromTo
        , zip, unzip)

die fn str = error $ "Data.Array.Parallel.PArray: " ++ fn ++ " " ++ str


-- Array Instances ------------------------------------------------------------
instance PA a => A.Array PArray a where
 valid     = const True
 singleton = A.singleton

 length  (PArray _ vec)
        = V.length $ toVectorPA vec

 index (PArray _ vec) ix
        = (toVectorPA vec) V.! ix

 append (PArray n1# xs) (PArray n2# ys)
        = PArray (n1# +# n2#) 
        $ fromVectorPA (toVectorPA xs V.++ toVectorPA ys)

 toVector (PArray _ vec)
        = toVectorPA vec

 fromVector vec
  = case V.length vec of
        I# n# -> PArray n# (fromVectorPA vec)


-- | Lift a unary array operator.
lift1   :: (PA a, PA b)
        => (a -> b) -> PArray a -> PArray b
lift1 f (PArray n# vec)
        = PArray n# 
        $ fromVectorPA
        $ V.map f (toVectorPA vec)


-- | Lift a binary array operator.
lift2   :: (PA a, PA b, PA c) 
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c
lift2 f (PArray n1# vec1) (PArray n2# vec2)
        | I# n1# /= I# n2# 
        = die "lift2" "length mismatch"
 
        | otherwise
        = PArray n1# 
        $ fromVectorPA 
        $ V.zipWith f 
                (toVectorPA vec1)
                (toVectorPA vec2)


-- | Lift a trinary array operator
lift3   :: (PA a, PA b, PA c, PA d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d
lift3 f (PArray n1# vec1) (PArray n2# vec2) (PArray n3# vec3)
        |   I# n1# /= I# n2# 
         || I# n1# /= I# n3#
        = die "lift3" "length mismatch"
 
        | otherwise
        = PArray n1# 
        $ fromVectorPA
        $ V.zipWith3 f 
                (toVectorPA vec1)
                (toVectorPA vec2)
                (toVectorPA vec3)


-- Basics ---------------------------------------------------------------------
-- | Check that an array has a valid internal representation.
valid :: PArray a -> Bool
valid _ = True

-- | Force an array to normal form.
nf :: PArray a -> ()
nf _    = ()


-- Constructors ----------------------------------------------------------------
-- | O(1). An empty array.
empty :: PA a => PArray a
empty           = PArray 0# $ fromVectorPA V.empty


-- | O(1). Produce an array containing a single element.
singleton :: PA a => a -> PArray a
singleton x     = PArray 1# $ fromVectorPA $ V.singleton x


-- | O(n). Produce an array of singleton arrays.
singletonl :: PA a => PArray a -> PArray (PArray a)
singletonl = lift1 singleton


-- | O(n). Define an array of the given size, that maps all elements to the same
--         value.
replicate :: PA a => Int -> a -> PArray a
replicate n@(I# n#) x
        = PArray n# $ fromVectorPA $ V.replicate n x


-- | O(sum lengths). Lifted replicate.
replicatel :: PA a => PArray Int -> PArray a -> PArray (PArray a)
replicatel = lift2 replicate


-- | O(sum lengths). Segmented replicate.
replicates :: PA a => U.Segd -> PArray a -> PArray a
replicates segd (PArray n# pdata)
 | I# n# /= U.lengthSegd segd
 = die "replicates" $ unlines
        [ "segd length mismatch"
        , "  segd length  = " ++ show (U.lengthSegd segd)
        , "  array length = " ++ show (I# n#) ]

 | otherwise
 = let  !(I# n2#) = U.elementsSegd segd
   in   PArray n2# 
         $ fromVectorPA
         $ join $ V.zipWith V.replicate
                        (V.convert $ U.lengthsSegd segd)
                        (toVectorPA pdata)


-- | O(sum lengths). Wrapper for segmented replicate that takes replication counts
--  and uses them to build the `U.Segd`.
replicates' :: PA a => PArray Int -> PArray a -> PArray a
replicates' (PArray _ reps) arr
 = replicates (U.lengthsToSegd $ V.convert $ toVectorPA reps) arr


-- | Append two arrays.
append :: PA a => PArray a -> PArray a -> PArray a
append (PArray n1# xs) (PArray n2# ys)
        = PArray (n1# +# n2#) 
        $ fromVectorPA (toVectorPA xs V.++ toVectorPA ys)


-- | Lifted append.
appendl :: PA a => PArray (PArray a) -> PArray (PArray a) -> PArray (PArray a)
appendl = lift2 append


-- | Concatenation
concat :: PA a => PArray (PArray a) -> PArray a
concat (PArray _ xss)
 = let  xs       = join $ V.map A.toVector $ toVectorPA xss
        !(I# n') = V.length xs
   in   PArray n' $ fromVectorPA xs


-- | Lifted concatenation
concatl :: PA a => PArray (PArray (PArray a)) -> PArray (PArray a)
concatl = lift1 concat


-- | Impose a nesting structure on a flat array
unconcat :: (PA a, PA b) => PArray (PArray a) -> PArray b -> PArray (PArray b)
unconcat arr1 arr2
        = nestUSegd (takeUSegd arr1) arr2


-- | Create a nested array from a segment descriptor and some flat data.
--   The segment descriptor must represent as many elements as present
--   in the flat data array, else `error`
nestUSegd :: PA a => U.Segd -> PArray a -> PArray (PArray a)
nestUSegd segd (PArray n# pdata)
 | U.elementsSegd segd   == I# n#
 , I# n2#                <- U.lengthSegd segd
 = PArray n2#
 $ fromVectorPA 
 $ V.zipWith
        (\start len@(I# len#) -> PArray len# 
                        $ fromVectorPA $ V.slice start len (toVectorPA pdata))
        (V.convert $ U.indicesSegd segd)
        (V.convert $ U.lengthsSegd segd)

 | otherwise
 = error $ unlines
        [ "Data.Array.Parallel.PArray.nestSegd: number of elements defined by "
                ++ "segment descriptor and data array do not match"
        , " length of segment desciptor = " ++ show (U.elementsSegd segd)
        , " length of data array        = " ++ show (I# n#) ]
{-# NOINLINE nestUSegd #-}


-- Projections ----------------------------------------------------------------
-- | Take the length of an array
length :: PA a => PArray a -> Int
length (PArray n# _)    = I# n#


-- | Take the length of some arrays.
lengthl :: PA a => PArray (PArray a) -> PArray Int
lengthl = lift1 length


-- | Lookup a single element from the source array.
index :: PA a => PArray a -> Int -> a
index (PArray _ arr) ix
        = (toVectorPA arr) V.! ix


-- | Lookup a several elements from several source arrays.
indexl :: PA a => PArray (PArray a) -> PArray Int -> PArray a
indexl  = lift2 index


-- | Extract a range of elements from an array.
extract :: PA a => PArray a -> Int -> Int -> PArray a
extract (PArray _ vec) start len@(I# len#)
        = PArray len# 
        $ fromVectorPA
        $ V.slice start len (toVectorPA vec)


-- | Segmented extract.
extracts :: PA a => Vector (PArray a) -> U.SSegd -> PArray a
extracts arrs ssegd
        = concat
        $ fromVector
        $ V.zipWith3
                (\src start len -> extract (arrs V.! src) start len)
                (V.convert $ U.sourcesOfSSegd ssegd)
                (V.convert $ U.startsOfSSegd  ssegd)
                (V.convert $ U.lengthsOfSSegd ssegd)


-- | Wrapper for `extracts` that takes arrays of sources, starts and lengths of
--   the segments, and uses these to build the `U.SSegd`.
extracts' 
        :: PA a => Vector (PArray a) 
        -> PArray Int           -- ^ id of source array for each segment.
        -> PArray Int           -- ^ starting index of each segment in its source array.
        -> PArray Int           -- ^ length of each segment.
        -> PArray a
extracts' arrs (PArray _ sources) (PArray _ starts) (PArray _ lengths)
 = let  segd    = U.lengthsToSegd $ V.convert $ toVectorPA lengths
        ssegd   = U.mkSSegd 
                        (V.convert $ toVectorPA starts)
                        (V.convert $ toVectorPA sources)
                        segd
   in   extracts arrs ssegd


-- | Extract a range of elements from an arrary.
--   Like `extract` but with the parameters in a different order.
slice :: PA a => Int -> Int -> PArray a -> PArray a
slice start len arr
 = extract arr start len


-- | Extract some slices from some arrays.
--   The arrays of starting indices and lengths must themselves
--   have the same length.
slicel  :: PA a 
        => PArray Int -> PArray Int -> PArray (PArray a) -> PArray (PArray a)
slicel  = lift3 slice


-- | Take the segment descriptor from a nested array. This can cause index space
--   overflow if the number of elements in the result does not can not be
--   represented by a single machine word.
takeUSegd :: PA a => (PArray (PArray a)) -> U.Segd
takeUSegd (PArray _ pdata)
        = U.lengthsToSegd 
        $ V.convert
        $ V.map length 
        $ toVectorPA pdata
        

-- Pack and Combine -----------------------------------------------------------
-- | Select the elements of an array that have their tag set to True.
pack    :: PA a => PArray a -> PArray Bool -> PArray a
pack (PArray n1# xs) (PArray n2# bs)
 | I# n1# /= I# n2#
 = die "pack" $ unlines
        [ "array length mismatch"
        , "  data  length = " ++ show (I# n1#)
        , "  flags length = " ++ show (I# n2#) ]

 | otherwise
 = let  xs'      = V.ifilter (\i _ -> (toVectorPA bs) V.! i) $ toVectorPA xs
        !(I# n') = V.length xs'
   in   PArray n' $ fromVectorPA xs'

-- | Lifted pack.
packl :: PA a => PArray (PArray a) -> PArray (PArray Bool) -> PArray (PArray a)
packl   = lift2 pack


-- | Filter an array based on some tags.
packByTag :: PA a => PArray a -> U.Array Tag -> Tag -> PArray a
packByTag (PArray n1# xs) tags tag
 | I# n1# /= U.length tags
 = die "packByTag" $ unlines
        [ "array length mismatch"
        , "  data  length = " ++ show (I# n1#)
        , "  flags length = " ++ (show $ U.length tags) ]

 | otherwise
 = let  xs'      = V.ifilter (\i _ -> U.index "packByTag" tags i == tag) 
                 $ toVectorPA xs
        !(I# n') = V.length xs'
   in   PArray n' $ fromVectorPA xs'


-- | Combine two arrays based on a selector.
combine2 :: PA a => U.Sel2 -> PArray a -> PArray a -> PArray a
combine2 tags (PArray _ pdata1) (PArray _ pdata2)
 = let  
        go [] [] []                     = []
        go (0 : bs) (x : xs) ys         = x : go bs xs ys
        go (1 : bs) xs       (y : ys)   = y : go bs xs ys
        go _ _ _ = error "Data.Array.Parallel.PArray.combine: length mismatch"
 
        vec3    = V.fromList
                $ go    (V.toList $ V.convert $ U.tagsSel2 tags)
                        (V.toList $ toVectorPA pdata1)
                        (V.toList $ toVectorPA pdata2)
        !(I# n') = V.length vec3
   
    in  PArray n' $ fromVectorPA vec3


-- Enumerations ---------------------------------------------------------------
-- | Construct a range of integers
enumFromTo :: Int -> Int -> PArray Int
enumFromTo m n 
        = fromList [m..n]


-- | Lifted enumeration
enumFromTol :: PArray Int -> PArray Int -> PArray (PArray Int)
enumFromTol = lift2 enumFromTo


-- Tuples ---------------------------------------------------------------------
-- | O(n). Zip a pair of arrays into an array of pairs.
zip     :: (PA a, PA b) => PArray a -> PArray b -> PArray (a, b)
zip (PArray n1# pdata1) (PArray _ pdata2)
        = PArray n1# 
        $ fromVectorPA
        $ V.zip (toVectorPA pdata1) (toVectorPA pdata2)


-- | Lifted zip
zipl    :: (PA a, PA b) 
        => PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl    = lift2 zip


-- | O(n). Unzip an array of pairs into a pair of arrays.
unzip   :: (PA a, PA b) => PArray (a, b) -> (PArray a, PArray b)
unzip (PArray n# pdata)
 = let  (xs, ys)        = V.unzip $ toVectorPA pdata
   in   ( PArray n# $ fromVectorPA xs
        , PArray n# $ fromVectorPA ys)


-- | Lifted unzip
unzipl  :: (PA a, PA b) => PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
unzipl  = lift1 unzip


-- Conversions ----------------------------------------------------------------
-- | Convert a `Vector` to a `PArray`
fromVector :: PA a => Vector a -> PArray a
fromVector vec
 = let  !(I# n#) = V.length vec
   in   PArray n# $ fromVectorPA vec


-- | Convert a `PArray` to a `Vector`        
toVector   :: PA a => PArray a -> Vector a
toVector (PArray _ vec)
        = toVectorPA vec


-- | Convert a list to a `PArray`.
fromList :: PA a => [a] -> PArray a
fromList xx
 = let  !(I# n#) = P.length xx
   in   PArray n# (fromVectorPA $ V.fromList xx)


-- | Convert a `PArray` to a list.
toList     :: PA a => PArray a -> [a]
toList (PArray _ vec)
        = V.toList $ toVectorPA vec


-- | Convert a `U.Array` to a `PArray`
fromUArray :: (PA a, U.Elt a) => U.Array a -> PArray a
fromUArray uarr
 = let  !(I# n#) = U.length uarr
   in   PArray n# (fromVectorPA $ V.convert uarr)


-- | Convert a `PArray` to a `U.Array`
toUArray :: (PA a, U.Elt a) => PArray a -> U.Array a
toUArray (PArray _ vec)
        = V.convert $ toVectorPA vec


-- | Convert a `U.Array` of tuples to a `PArray`
fromUArray2
        :: (PA a, U.Elt a, PA b, U.Elt b)
        => U.Array (a, b) -> PArray (a, b)
        
fromUArray2 uarr
 = let  !(I# n#) = U.length uarr
   in   PArray n# $ fromVectorPA $ V.convert uarr
