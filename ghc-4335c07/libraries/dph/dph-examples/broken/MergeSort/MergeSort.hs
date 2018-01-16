{-# LANGUAGE PArr, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module MergeSort 
	( sortCore,    sortCorePA
	, mergeCore,   mergeCorePA
	, flipPairs,   flipPairsPA
	, interleave,  interleavePA)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int
import qualified Prelude as P

-- Wrappers -------------------------------------------------------------------
sortCorePA :: PArray Int -> PArray Int
{-# NOINLINE sortCorePA #-}
sortCorePA ps
	= toPArrayP (sortCore (fromPArrayP ps))

mergeCorePA :: PArray Int -> PArray Int -> PArray Int
{-# NOINLINE mergeCorePA #-}
mergeCorePA arr1 arr2
	= toPArrayP (mergeCore (fromPArrayP arr1 +:+ fromPArrayP arr2))

flipPairsPA :: PArray Int -> PArray Int
{-# NOINLINE flipPairsPA #-}
flipPairsPA ps
	= toPArrayP (flipPairs (fromPArrayP ps))

interleavePA :: PArray Int -> PArray Int -> PArray Int
{-# NOINLINE interleavePA #-}
interleavePA arr1 arr2
	= toPArrayP (interleave (fromPArrayP arr1) (fromPArrayP arr2))


-------------------------------------------------------------------------------
-- | Batcher odd/even merge sort.
--   The length of the list must be a power of two, else loop.
sortCore :: [:Int:] -> [:Int:]
sortCore xx
	| len == 0	= [::]
	| len == 1	= xx
	| otherwise
	= let	half	= len `div` 2
		s1	= sliceP 0    half xx
		s2	= sliceP half len  xx
	  in	mergeCore (sortCore s1 +:+ sortCore s2)

	where len	= lengthP xx


-- | Batcher odd/even merge.
--   The two lists to merge are appended on the input.
--   The length of the lists must be a power of two, else loop.
mergeCore :: [:Int:] -> [:Int:]
mergeCore xx = mergeCore' xx (lengthP xx) 0 1

mergeCore' :: [:Int:] -> Int -> Int -> Int -> [:Int:]
mergeCore' xx len offset stride
	| len == 2
	= let 	x0	= get xx offset stride 0
		x1	= get xx offset stride 1
	  in if x1 < x0
		then [: x1, x0 :]
		else [: x0, x1 :]
	
	| otherwise
	= let	evens'	= mergeCore' xx (len `div` 2) offset            (stride * 2)
		odds'	= mergeCore' xx (len `div` 2) (offset + stride) (stride * 2)
		xx'	= interleave evens' odds'
		ixLast	= lengthP xx' - 1

	  in	[: xx' !: 0 :]  
	    +:+ (flipPairs (sliceP 1 ixLast xx'))
	    +:+ [: xx' !: ixLast :]


-- | Get an indexed value from an array using an offset and stride.
get :: [:Int:] -> Int -> Int -> Int -> Int
get xx offset stride ix
	= xx !: (offset + (ix * stride))


-- | For each consecutive pair of elements, 
--	if they are out of order then flip them so they are.
flipPairs  :: [:Int:] -> [:Int:]
flipPairs xx
 = concatP 
	[: if y < x then [: y, x :] else [: x, y :]
	|  (x, y) 	<- oddevens xx :]


-- | Interleave the elements of two arrays.
interleave :: [:Int:] -> [:Int:] -> [:Int:]
interleave xx yy
 = concatP [: [:x, y:] | (x, y) <- zipP xx yy :]


-- | Pair up the elements with odd an even indices
--   oddevens [: 1, 8, 3, 6, 2, 8 :] 
--	= [: (1, 8), (3, 6), (2, 8) :]
--
oddevens :: [:Int:] -> [:(Int, Int):]
oddevens xx
	= [: (x, xx !: (ix + 1) )	
			| (ix, x)	<- indexedP xx
			, mod ix 2 == 0 :]


