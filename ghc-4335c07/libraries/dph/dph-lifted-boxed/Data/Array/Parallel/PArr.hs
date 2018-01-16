{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Unvectorised functions that work directly on parallel arrays of [::] type.
--   These are used internally in the DPH library, but the user never sees them.
--   Only vectorised functions are visible in the DPH client programs.
--
module Data.Array.Parallel.PArr 
        ( PArr
        , emptyPArr
        , replicatePArr
        , singletonPArr
        , indexPArr
        , headPArr
        , lengthPArr)
where
import GHC.ST
import GHC.Base
import GHC.PArr

-- | Construct an empty array, with no elements.
emptyPArr :: PArr a
emptyPArr = replicatePArr 0 undefined
{-# NOINLINE emptyPArr #-}


-- | Construct an array with a single element.
singletonPArr :: a -> PArr a
singletonPArr e = replicatePArr 1 e
{-# NOINLINE singletonPArr #-}


-- | Construct an array by replicating the given element some number of times.
replicatePArr :: Int -> a -> PArr a
replicatePArr n e  
 = runST (do
         marr# <- newArray n e
         mkPArr n marr#)
{-# NOINLINE replicatePArr #-}


-- | Take the length of an array.
lengthPArr :: PArr a -> Int
lengthPArr (PArr n _) = n
{-# NOINLINE lengthPArr #-}


-- | Lookup a single element from the source array.
indexPArr :: PArr e -> Int -> e
indexPArr (PArr n arr#) i@(I# i#)
  | i >= 0 && i < n 
  = case indexArray# arr# i# of (# e #) -> e

  | otherwise
  = error $  "indexPArr: out of bounds parallel array index; " 
          ++ "idx = " ++ show i ++ ", arr len = "
          ++ show n
{-# NOINLINE indexPArr #-}


-- | Take the first element of the source array, 
--   or `error` if there isn't one.
headPArr :: PArr a -> a
headPArr arr = indexPArr arr 0
{-# NOINLINE headPArr #-}


-------------------------------------------------------------------------------
-- | Internally used mutable boxed arrays
data MPArr s e = MPArr !Int (MutableArray# s e)


-- | Allocate a new mutable array that is pre-initialised with a given value
newArray :: Int -> e -> ST s (MPArr s e)
newArray n@(I# n#) e  = ST $ \s1# ->
  case newArray# n# e s1# of { (# s2#, marr# #) ->
  (# s2#, MPArr n marr# #)}
{-# INLINE newArray #-}


-- | Convert a mutable array into the external parallel array representation
mkPArr :: Int -> MPArr s e -> ST s (PArr e)
mkPArr n (MPArr _ marr#)  = ST $ \s1# ->
  case unsafeFreezeArray# marr# s1#   of { (# s2#, arr# #) ->
  (# s2#, PArr n arr# #) }
{-# INLINE mkPArr #-}

