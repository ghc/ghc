module  Array ( 
    module Ix,  -- export all of Ix 
    Array, array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, amap, ixmap ) where
import Ix
import List((\\))
import GHCbase

-- Report note:
-- This module specifies the semantics of arrays only: it is not
-- intended as an efficient implementation.

infixl 9  !, //

--Report:data  (Ix a)    => Array a b = MkArray (a,a) (a -> b) deriving ()
-- in GHCbase:
-- data Ix ix => Array ix elt = Array (ix, ix) (Array# elt)

--type IPr = (Int, Int)

{-# GENERATE_SPECS array a{~,Int,IPr} b{} #-}
array		      :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

{-# GENERATE_SPECS listArray a{~,Int,IPr} b{} #-}
listArray	      :: (Ix a) => (a,a) -> [b] -> Array a b

{-# GENERATE_SPECS (!) a{~,Int,IPr} b{} #-}
(!)		      :: (Ix a) => Array a b -> a -> b

bounds		      :: (Ix a) => Array a b -> (a,a)

{-# GENERATE_SPECS indices a{~,Int,IPr} b{} #-}
indices		      :: (Ix a) => Array a b -> [a]

{-# GENERATE_SPECS elems a{~,Int,IPr} b{} #-}
elems		      :: (Ix a) => Array a b -> [b]

{-# GENERATE_SPECS assocs a{~,Int,IPr} b{} #-}
assocs		      :: (Ix a) => Array a b -> [(a,b)]

{-# GENERATE_SPECS (//) a{~,Int,IPr} b{} #-}
(//)		      :: (Ix a) => Array a b -> [(a,b)] -> Array a b

{-# GENERATE_SPECS accum a{~,Int,IPr} b{} c{} #-}
accum		      :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b

{-# GENERATE_SPECS accumArray a{~,Int,IPr} b{} c{} #-}
accumArray	      :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b

{-# GENERATE_SPECS amap a{~,Int,IPr} b{} c{} #-}
amap		      :: (Ix a) => (b -> c) -> Array a b -> Array a c

ixmap		      :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

-----------------------------------------------------------------------
{- "array", "!" and "bounds" are basic;
   the rest can be defined in terms of them
-}

bounds (Array b _)  = b

(Array bounds arr#) ! i
  = let n# = case (index bounds i) of { I# x -> x } -- index fails if out of range
    in
    case (indexArray# arr# n#) of
      Lift v -> v

#ifdef USE_FOLDR_BUILD
{-# INLINE array #-}
#endif
array ixs@(ix_start, ix_end) ivs =
   runST ( ST $ \ s ->
	case (newArray ixs arrEleBottom)	of { ST new_array_thing ->
	case (new_array_thing s)		of { (arr@(MutableArray _ arr#),s) ->
	let
         fill_one_in (S# s#) (i, v)
             = case index ixs  i		of { I# n# ->
	       case writeArray# arr# n# v s# 	of { s2#   ->
	       S# s2# }}
	in
	case (foldl fill_one_in s ivs) 		of { s@(S# _) -> 
	case (freezeArray arr)			of { ST freeze_array_thing ->
	freeze_array_thing s }}}})

arrEleBottom = error "(Array.!): undefined array element"

fill_it_in :: Ix ix => MutableArray s ix elt -> [(ix, elt)] -> ST s ()
fill_it_in arr lst
  = foldr fill_one_in (returnStrictlyST ()) lst
  where  -- **** STRICT **** (but that's OK...)
    fill_one_in (i, v) rst
      = writeArray arr i v `seqStrictlyST` rst

-----------------------------------------------------------------------
-- these also go better with magic: (//), accum, accumArray

old_array // ivs
  = runST (
	-- copy the old array:
	thawArray old_array		    `thenStrictlyST` \ arr# ->	
	-- now write the new elements into the new array:
	fill_it_in arr# ivs		    `seqStrictlyST`
	freezeArray arr#
    )
  where
    bottom = error "(Array.//): error in copying old array\n"

zap_with_f :: Ix ix => (elt -> elt2 -> elt) -> MutableArray s ix elt -> [(ix,elt2)] -> ST s ()
-- zap_with_f: reads an elem out first, then uses "f" on that and the new value

zap_with_f f arr lst
  = foldr zap_one (returnStrictlyST ()) lst
  where
    zap_one (i, new_v) rst
      = readArray  arr i		 `thenStrictlyST`  \ old_v ->
	writeArray arr i (f old_v new_v) `seqStrictlyST`
	rst

accum f arr ivs
  = runST (
	-- copy the old array:
	newArray (bounds arr) bottom	>>= \ arr# ->
	fill_it_in arr# (assocs arr)	>>

	-- now zap the elements in question with "f":
	zap_with_f f arr# ivs		>>
	freezeArray arr#
    )
  where
    bottom = error "Array.accum: error in copying old array\n"

accumArray f zero ixs ivs
  = runST (
	newArray ixs zero	>>= \ arr# ->
	zap_with_f f  arr# ivs	>>
	freezeArray arr#
    )

-----------------------------------------------------------------------

listArray b vs	      =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

#ifdef USE_FOLDR_BUILD
{-# INLINE indices #-}
{-# INLINE elems #-}
{-# INLINE assocs #-}
#endif

indices		      =  range . bounds

elems a               =  [a!i | i <- indices a]

assocs a              =  [(i, a!i) | i <- indices a]

amap f a              =  array b [(i, f (a!i)) | i <- range b]
                         where b = bounds a

ixmap b f a           =  array b [(i, a ! f i) | i <- range b]
