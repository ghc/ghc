{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, BangPatterns, CPP #-}
{-# OPTIONS  -w #-}     -- TODO: enable warnings
#include "fusion-phases.h"

-- | Wrappers for primitives defined in @Data.Vector@.
--
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted".
--

--  This module doesn't have docs because the bindings are mostly just 
--  forwards for the Data.Vector functions. See there for details.
module Data.Array.Parallel.Unlifted.Sequential.Vector (

  -- * Array classes
  Unbox,

  -- * Array types
  Vector, MVector,

  -- * Streaming
  stream, unstream,

  -- * Basic operations
  length, null, empty, singleton, cons, units,
  replicate,
  (++), index,
  interleave, indexed, repeat, repeatS,

  -- * Subarrays
  slice,   unsafeSlice,
  extract, unsafeExtract,
  tail,
  take, drop, splitAt,

  -- * Permutations
  permute,
  bpermute,
  mbpermute,
  bpermuteDft,
  reverse,
  update,

  -- * Higher-order operations
  map, zipWith, zipWith3,
  filter, pack, 
  combine, combine2ByTag,
  foldl, foldl1, foldl1Maybe,
  fold, fold1, fold1Maybe,
  scanl, scanl1,
  scan, scan1,
  scanRes,

  -- * Searching
  elem, notElem,

  -- * Logical operations
  and, or, any, all,

  -- * Arithmetic operations
  sum,        product,
  maximum,    minimum,
  maximumBy,  minimumBy,
  maxIndex,   minIndex,
  maxIndexBy, minIndexBy,

  -- * Arrays of pairs
  zip,  unzip, fsts, snds,
  zip3, unzip3,

  -- * Enumerations
  enumFromTo,
  enumFromThenTo,
  enumFromStepLen,
  enumFromToEach,
  enumFromStepLenEach,

  -- * Searching
  find, findIndex,

  -- * Conversions to\/from lists
  toList, fromList,

  -- * Random arrays
  random, randomR,

  -- * Mutating operations
  new, copy,

  -- * Mutable vectors
  newM, unsafeFreeze, M.write, M.read, mpermute, mupdate,
  mdrop, mslice,

  -- * I\/O
  UIO(..)
) 
where
import Data.Array.Parallel.Unlifted.Stream.Segmented
import Data.Array.Parallel.Base ( Tag, checkEq, ST )
import qualified Data.Array.Parallel.Base          as B
import qualified Data.Vector.Unboxed               as V
import qualified Data.Vector.Unboxed.Mutable       as M
import qualified Data.Vector.Unboxed.Base          as VBase
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as MG
import qualified Data.Vector.Storable              as Storable
import qualified Data.Vector.Storable.Mutable      as MStorable
import qualified Data.Vector.Generic.New           as New
import qualified Data.Vector.Fusion.Bundle         as S
import qualified Data.Vector.Fusion.Bundle.Monadic as SM
import Data.Vector.Fusion.Stream.Monadic           ( Stream(..), Step(..) )
import Data.Vector.Fusion.Bundle.Size              ( Size(..) )
import Data.Vector.Generic                         ( stream, unstream )

import Data.Vector.Unboxed 
        hiding ( slice, zip, unzip, zip3, unzip3, foldl, foldl1, scanl, scanl1,
                 unsafeSlice )

import Prelude 
        hiding ( length, null,
                replicate, (++), repeat,
                tail, take, drop, splitAt,
                reverse,
                map, zipWith, zipWith3, filter,
                foldl, foldl1, scanl, scanl1,
                elem, notElem,
                and, or, any, all,
                sum, product,
                maximum, minimum,
                zip, unzip, zip3, unzip3,
                enumFromTo, enumFromThenTo )

import qualified Prelude
import qualified System.Random as R
import Foreign hiding ( new )
import System.IO
import Control.Monad hiding (empty)

here s = "Data.Array.Parallel.Unlifted.Sequential.Flat." Prelude.++ s

-------------------------------------------------------------------------------
new :: Unbox a => Int -> (forall s. MVector s a -> ST s ()) -> Vector a
new n p 
 = V.create 
 $ do   v <- M.new n
        p v
        return v
{-# INLINE new #-}


newM :: Unbox a => Int -> ST s (MVector s a)
newM = M.new
{-# INLINE newM #-}


-- Yield an array of units 
units :: Int -> Vector ()
units n = replicate n ()
{-# INLINE units #-}
                        

-- Interleave the elements of two arrays
interleave :: Unbox e => Vector e -> Vector e -> Vector e
interleave xs ys = unstream (interleaveS (stream xs) (stream ys))
{-# INLINE_U interleave #-}


-- Repeat an array @n@ times
repeat :: Unbox e => Int -> Vector e -> Vector e
repeat n xs = unstream (repeatS n xs)
{-# INLINE_U repeat #-}


repeatS :: Unbox e => Int -> Vector e -> S.Bundle v e
{-# INLINE_STREAM repeatS #-}
repeatS k !xs = SM.fromStream (Stream next (0,k)) (Exact (k*n))
  where
    !n = length xs

    {-# INLINE next #-}
    next (i,0) = return Done
    next (i,k) | i == n    = return $ Skip                     (0,k-1)
               | otherwise = return $ Yield (unsafeIndex xs i) (i+1,k)


-- Take a sub-range of a vector, avoiding copying.
slice :: Unbox a => String -> Vector a -> Int -> Int -> Vector a
slice here xs i n 
        = B.checkSlice here (V.length xs) i n
        $ V.unsafeSlice i n xs
{-# INLINE_U slice #-}


-- Take a sub-range of a vector, avoiding copying, without bounds checks.
unsafeSlice :: Unbox a => Vector a -> Int -> Int -> Vector a
unsafeSlice xs i n 
        = V.unsafeSlice i n xs
{-# INLINE_U unsafeSlice #-}


index :: Unbox a => String -> Vector a -> Int -> a
index here vec ix
        = B.check here (V.length vec) ix
        $ V.unsafeIndex vec ix
{-# INLINE_U index #-}


-- Copy out a subrange of a vector.
extract :: Unbox a => Vector a -> Int -> Int -> Vector a
extract xs i n
        = B.checkSlice "extract" (V.length xs) i n
        $ force (V.unsafeSlice i n xs)
{-# INLINE_U extract #-}


-- Copy out a subrange of a vector, without bounds checks.
unsafeExtract :: Unbox a => Vector a -> Int -> Int -> Vector a
unsafeExtract xs i n = force (V.unsafeSlice i n xs)
{-# INLINE_U unsafeExtract #-}


mupdate :: Unbox e => MVector s e -> Vector (Int,e) -> ST s ()
mupdate marr xs 
        = MG.update marr (stream xs)
{-# INLINE_U mupdate #-}


mpermute :: Unbox e => MVector s e -> Vector e -> Vector Int -> ST s ()
mpermute marr xs is 
        = MG.update marr (stream (zip is xs))
{-# INLINE_U mpermute #-}


permute :: Unbox e => Vector e -> Vector Int -> Vector e
{-# INLINE_U permute #-}
permute xs is 
 = create 
 $ do   v <- M.new (length xs)
        mpermute v xs is
        return v


bpermute :: Unbox e => Vector e -> Vector Int -> Vector e
bpermute = backpermute
{-# INLINE_U bpermute #-}


mbpermute :: (Unbox e, Unbox d) => (e -> d) -> Vector e -> Vector Int -> Vector d
mbpermute f es is
        = unstream (mbpermuteS f es (stream is))
{-# INLINE_STREAM mbpermute #-}


bpermuteS :: Unbox e => Vector e -> S.Bundle v Int -> S.Bundle v e
bpermuteS !a s 
        = S.map (a!) s
{-# INLINE_STREAM bpermuteS #-}


mbpermuteS:: Unbox e => (e -> d) -> Vector e -> S.Bundle v Int -> S.Bundle v d
mbpermuteS f !a 
        = S.map (f . (a!))
{-# INLINE_STREAM mbpermuteS #-}


-- Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
bpermuteDft :: Unbox e
	    => Int			        -- length of result array
	    -> (Int -> e)		        -- initialiser function
	    -> Vector (Int,e)	        	-- index-value pairs
	    -> Vector e
bpermuteDft n init
        = update (map init (enumFromN 0 n))
{-# INLINE_U bpermuteDft #-}


-- Extract all elements from an array according to a given flag array
pack:: Unbox e => Vector e -> Vector Bool -> Vector e
pack xs = map fst . filter snd . zip xs
{-# INLINE_U pack #-}


combine :: Unbox a
	 => Vector Bool -> Vector a -> Vector a -> Vector a
combine bs
        = combine2ByTag (map (\b -> if b then 0 else 1) bs)
{-# INLINE combine #-}


combine2ByTag :: Unbox a => Vector Tag -> Vector a -> Vector a -> Vector a
combine2ByTag ts xs ys
  = checkEq (here "combine2ByTag")
            ("tags length /= sum of args length")
            (length ts) (length xs + length ys)
  $ unstream (combine2ByTagS (stream ts) (stream xs) (stream ys))
{-# INLINE_U combine2ByTag #-}


-- Array reduction proceeding from the left
foldl :: Unbox a => (b -> a -> b) -> b -> Vector a -> b
foldl = foldl'
{-# INLINE_U foldl #-}


-- Array reduction proceeding from the left for non-empty arrays
foldl1 :: Unbox a => (a -> a -> a) -> Vector a -> a
foldl1 = foldl1'
{-# INLINE_U foldl1 #-}


-- Array reduction that requires an associative combination function with its
-- unit
fold :: Unbox a => (a -> a -> a) -> a -> Vector a -> a
fold = foldl
{-# INLINE_U fold #-}


-- Reduction of a non-empty array which requires an associative combination
-- function
fold1 :: Unbox a => (a -> a -> a) -> Vector a -> a
fold1 = foldl1
{-# INLINE_U fold1 #-}


foldl1Maybe :: Unbox a => (a -> a -> a) -> Vector a -> Maybe a
foldl1Maybe f xs = foldl' join Nothing xs
  where
    {-# INLINE join #-}
    join Nothing  y = Just $! y
    join (Just x) y = Just $! f x y
{-# INLINE_U foldl1Maybe #-}


fold1Maybe :: Unbox a => (a -> a -> a) -> Vector a -> Maybe a
fold1Maybe = foldl1Maybe
{-# INLINE_U fold1Maybe #-}


-- Prefix scan proceedings from left to right
scanl :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Vector a -> Vector b
scanl = prescanl'
{-# INLINE_U scanl #-}


-- Prefix scan of a non-empty array proceeding from left to right
scanl1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
scanl1 = scanl1'
{-# INLINE_U scanl1 #-}


-- Prefix scan proceeding from left to right that needs an associative
-- combination function with its unit
scan :: Unbox a => (a -> a -> a) -> a -> Vector a -> Vector a
scan = scanl
{-# INLINE_U scan #-}


-- Prefix scan of a non-empty array proceeding from left to right that needs
-- an associative combination function
scan1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
scan1 = scanl1
{-# INLINE_U scan1 #-}


scanRes :: Unbox a => (a -> a -> a) -> a -> Vector a -> (Vector a,a)
scanRes f z xs 
 = let  ys = scanl' f z xs
   in   (unsafeInit ys, unsafeLast ys)
{-# INLINE_U scanRes #-}


fsts :: (Unbox a, Unbox b) => Vector (a,b) -> Vector a
fsts (VBase.V_2 _ xs ys) = xs
{-# INLINE_STREAM fsts #-}


snds :: (Unbox a, Unbox b) => Vector (a,b) -> Vector b
snds (VBase.V_2 _ xs ys) = ys
{-# INLINE_STREAM snds #-}


zip :: (Unbox a, Unbox b) => Vector a -> Vector b -> Vector (a,b)
zip !xs !ys = V.zip xs ys
{-# INLINE_STREAM zip #-}


unzip :: (Unbox a, Unbox b) => Vector (a,b) -> (Vector a, Vector b)
unzip ps = V.unzip ps
{-# INLINE_STREAM unzip #-}


{-# RULES

"fsts/new/unstream [dph-prim-seq]" forall xs.
  fsts (G.new (New.unstream xs)) = V.map fst (G.new (New.unstream xs))

"snds/new/unstream [dph-prim-seq]" forall xs.
  snds (G.new (New.unstream xs)) = V.map snd (G.new (New.unstream xs))

"stream/zip [dph-prim-seq]" forall xs ys.
  G.stream (zip xs ys) = S.zip (G.stream xs) (G.stream ys)

  #-}


zip3    :: (Unbox a, Unbox b, Unbox c)
        => Vector a -> Vector b -> Vector c -> Vector (a,b,c)
zip3 !xs !ys !zs 
        = V.zip3 xs ys zs
{-# INLINE_STREAM zip3 #-}


unzip3  :: (Unbox a, Unbox b, Unbox c)
        => Vector (a,b,c) -> (Vector a, Vector b, Vector c)
unzip3 ps = V.unzip3 ps
{-# INLINE_STREAM unzip3 #-}


enumFromStepLen :: Int -> Int -> Int -> Vector Int
enumFromStepLen = enumFromStepN
{-# INLINE_U enumFromStepLen #-}


enumFromToEach :: Int -> Vector (Int,Int) -> Vector Int
enumFromToEach n 
        = unstream . enumFromToEachS n . stream
{-# INLINE_U enumFromToEach #-}


enumFromStepLenEach :: Int -> Vector Int -> Vector Int -> Vector Int -> Vector Int
enumFromStepLenEach len starts steps lens
        = unstream 
        $ enumFromStepLenEachS len 
        $ stream 
        $ V.zip3 starts steps lens
{-# INLINE_U enumFromStepLenEach #-}


random :: (Unbox a, R.Random a, R.RandomGen g) => Int -> g -> Vector a
random n = unstream . randomS n
{-# INLINE_U random #-}


randomR :: (Unbox a, R.Random a, R.RandomGen g) => Int -> (a,a) -> g -> Vector a
randomR n r = unstream . randomRS n r
{-# INLINE_U randomR #-}


randomS :: (R.RandomGen g, R.Random a) => Int -> g -> S.Bundle v a
randomS n g 
  = SM.fromStream (Stream step (g,n)) (Exact n)
  where
    {-# INLINE step #-}
    step (g,0) = return Done
    step (g,n) = let (x,g') = R.random g
                 in return $ Yield x (g',n-1)
{-# INLINE_STREAM randomS #-}


randomRS :: (R.RandomGen g, R.Random a) => Int -> (a,a) -> g -> S.Bundle v a
randomRS n r g
  = SM.fromStream (Stream step (g,n)) (Exact n)
  where
    {-# INLINE step #-}
    step (g,0) = return Done
    step (g,n) = let (x,g') = R.randomR r g
                 in return $ Yield x (g',n-1)
{-# INLINE_STREAM randomRS #-}


mdrop :: Unbox a => Int -> MVector s a -> MVector s a
mdrop = M.drop
{-# INLINE mdrop #-}


mslice :: Unbox a => Int -> Int -> MVector s a -> MVector s a
mslice i n xs
        = B.checkSlice "mslice" (M.length xs) i n
        $ M.unsafeSlice i n xs
{-# INLINE mslice #-}


-- IO Functions ---------------------------------------------------------------
hGetStorable 
        :: forall a. Storable a 
        => Handle -> IO (Storable.Vector a)
hGetStorable h =
  alloca $ \iptr ->
  do
    hGetBuf h iptr (sizeOf (undefined :: Int))
    n <- peek iptr
    v <- MStorable.unsafeNew n
    let bytes = sizeOf (undefined :: a) * MStorable.length v
    r <- MStorable.unsafeWith v $ \ptr -> hGetBuf h ptr bytes
    Storable.unsafeFreeze (MStorable.take r v)


hPutStorable 
        :: forall a. Storable a
        => Handle -> Storable.Vector a -> IO ()
hPutStorable h xs =
  alloca $ \iptr ->
  do
    poke iptr n 
    hPutBuf h iptr (sizeOf n)
    Storable.unsafeWith xs $ \ptr ->
      do
        hPutBuf h ptr (sizeOf (undefined :: a) * n)
        return ()
  where
    !n = Storable.length xs


class Unbox a => UIO a where
  hPut :: Handle -> Vector a -> IO ()
  hGet :: Handle -> IO (Vector a)


primPut :: (Unbox a, Storable a) => Handle -> Vector a -> IO ()
primPut h = hPutStorable h . Storable.convert
{-# INLINE primPut #-}


primGet :: (Unbox a, Storable a) => Handle -> IO (Vector a)
primGet = fmap convert . hGetStorable
{-# INLINE primGet #-}


instance UIO Int where
  {-# INLINE hPut #-}
  hPut = primPut

  {-# INLINE hGet #-}
  hGet = primGet


instance UIO Double where
  {-# INLINE hPut #-}
  hPut = primPut

  {-# INLINE hGet #-}
  hGet = primGet


instance (UIO a, UIO b) => UIO (a,b) where
  {-# INLINE hPut #-}
  hPut h xs = case V.unzip xs of
                (ys,zs) -> do hPut h ys
                              hPut h zs

  {-# INLINE hGet #-}
  hGet h = do xs <- hGet h
              ys <- hGet h
              return (V.zip xs ys)


-- Additional Unbox instances -------------------------------------------------
newtype instance MVector s Ordering = MV_Ordering (M.MVector s Word8)
newtype instance Vector    Ordering = V_Ordering  (V.Vector    Word8)

instance Unbox Ordering

instance MG.MVector MVector Ordering where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Ordering v)           
        = MG.basicLength v

  basicUnsafeSlice i n (MV_Ordering v)  
        = MV_Ordering $ MG.basicUnsafeSlice i n v

  basicOverlaps (MV_Ordering v1) (MV_Ordering v2) 
        = MG.basicOverlaps v1 v2

  basicUnsafeNew n                      
        = MV_Ordering `liftM` MG.basicUnsafeNew n

  basicUnsafeReplicate n x              
        = MV_Ordering `liftM` MG.basicUnsafeReplicate n (fromOrdering x)

  basicUnsafeRead (MV_Ordering v) i     
        = toOrdering `liftM` MG.basicUnsafeRead v i

  basicUnsafeWrite (MV_Ordering v) i x  
        = MG.basicUnsafeWrite v i (fromOrdering x)

  basicClear (MV_Ordering v) 
        = MG.basicClear v

  basicSet (MV_Ordering v) x
        = MG.basicSet v (fromOrdering x)

  basicUnsafeCopy (MV_Ordering v1) (MV_Ordering v2) 
        = MG.basicUnsafeCopy v1 v2

  basicUnsafeMove (MV_Ordering v1) (MV_Ordering v2) 
        = MG.basicUnsafeMove v1 v2

  basicUnsafeGrow (MV_Ordering v) n 
        = MV_Ordering `liftM` MG.basicUnsafeGrow v n


instance G.Vector Vector Ordering where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Ordering v) 
        = V_Ordering `liftM` G.basicUnsafeFreeze v

  basicUnsafeThaw (V_Ordering v) 
        = MV_Ordering `liftM` G.basicUnsafeThaw v

  basicLength (V_Ordering v) 
        = G.basicLength v

  basicUnsafeSlice i n (V_Ordering v) 
        = V_Ordering $ G.basicUnsafeSlice i n v

  basicUnsafeIndexM (V_Ordering v) i 
        = toOrdering `liftM` G.basicUnsafeIndexM v i

  basicUnsafeCopy (MV_Ordering mv) (V_Ordering v) 
        = G.basicUnsafeCopy mv v

  elemseq _ = seq


fromOrdering :: Ordering -> Word8
{-# INLINE fromOrdering #-}
fromOrdering LT = 0
fromOrdering EQ = 1
fromOrdering GT = 2

toOrdering :: Word8 -> Ordering
{-# INLINE toOrdering #-}
toOrdering 0 = LT
toOrdering 1 = EQ
toOrdering _ = GT


instance Unbox Integer
instance MG.MVector MVector Integer
instance G.Vector Vector Integer


