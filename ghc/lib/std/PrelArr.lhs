%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[PrelArr]{Module @PrelArr@}

Array implementation, @PrelArr@ exports the basic array
types and operations.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelArr where

import {-# SOURCE #-} PrelErr ( error )
import Ix
import PrelList (foldl)
import PrelST
import PrelBase
import PrelCCall
import PrelAddr
import PrelGHC

infixl 9  !, //
\end{code}

\begin{code}
{-# SPECIALISE array :: (Int,Int) -> [(Int,b)] -> Array Int b #-}
array		      :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

{-# SPECIALISE (!) :: Array Int b -> Int -> b #-}
(!)		      :: (Ix a) => Array a b -> a -> b

{-# SPECIALISE bounds :: Array Int b -> (Int,Int) #-}
bounds		      :: (Ix a) => Array a b -> (a,a)

{-# SPECIALISE (//) :: Array Int b -> [(Int,b)] -> Array Int b #-}
(//)		      :: (Ix a) => Array a b -> [(a,b)] -> Array a b

{-# SPECIALISE accum  :: (b -> c -> b) -> Array Int b -> [(Int,c)] -> Array Int b #-}
accum		      :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b

{-# SPECIALISE accumArray :: (b -> c -> b) -> b -> (Int,Int) -> [(Int,c)] -> Array Int b #-}
accumArray	      :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
\end{code}


%*********************************************************
%*							*
\subsection{The @Array@ types}
%*							*
%*********************************************************

\begin{code}
type IPr = (Int, Int)

data Ix ix => Array ix elt		= Array     	   (ix,ix) (Array# elt)
data Ix ix => ByteArray ix      	= ByteArray	   (ix,ix) ByteArray#
data Ix ix => MutableArray     s ix elt = MutableArray     (ix,ix) (MutableArray# s elt)
data Ix ix => MutableByteArray s ix     = MutableByteArray (ix,ix) (MutableByteArray# s)

instance CCallable (MutableByteArray s ix)
instance CCallable (MutableByteArray# s)

instance CCallable (ByteArray ix)
instance CCallable ByteArray#

data MutableVar s a = MutableVar (MutVar# s a)

instance Eq (MutableVar s a) where
	MutableVar v1# == MutableVar v2#
		= sameMutVar# v1# v2#

-- just pointer equality on arrays:
instance Eq (MutableArray s ix elt) where
	MutableArray _ arr1# == MutableArray _ arr2# 
		= sameMutableArray# arr1# arr2#

instance Eq (MutableByteArray s ix) where
	MutableByteArray _ arr1# == MutableByteArray _ arr2#
		= sameMutableByteArray# arr1# arr2#
\end{code}

%*********************************************************
%*							*
\subsection{Operations on mutable variables}
%*							*
%*********************************************************

\begin{code}
newVar   :: a -> ST s (MutableVar s a)
readVar  :: MutableVar s a -> ST s a
writeVar :: MutableVar s a -> a -> ST s ()

newVar init = ST $ \ s# ->
    case (newMutVar# init s#)     of { (# s2#, var# #) ->
    (# s2#, MutableVar var# #) }

readVar (MutableVar var#) = ST $ \ s# -> readMutVar# var# s#

writeVar (MutableVar var#) val = ST $ \ s# ->
    case writeMutVar# var# val s# of { s2# ->
    (# s2#, () #) }
\end{code}

%*********************************************************
%*							*
\subsection{Operations on immutable arrays}
%*							*
%*********************************************************

"array", "!" and "bounds" are basic; the rest can be defined in terms of them

\begin{code}
bounds (Array b _)  = b

(Array bounds arr#) ! i
  = let n# = case (index bounds i) of { I# x -> x } -- index fails if out of range
    in
    case (indexArray# arr# n#) of
      (# v #) -> v

{-# INLINE array #-}
array ixs ivs 
  = case rangeSize ixs				of { I# n ->
    runST ( ST $ \ s1 -> 
	case newArray# n arrEleBottom s1	of { (# s2, marr #) ->
	foldr (fill ixs marr) (done ixs marr) ivs s2
    })}

fill :: Ix ix => (ix,ix)  -> MutableArray# s elt
	      -> (ix,elt) -> STRep s a -> STRep s a
{-# INLINE fill #-}
fill ixs marr (i,v) next = \s1 -> case index ixs i	of { I# n ->
				  case writeArray# marr n v s1	of { s2 ->
				  next s2 }}

done :: Ix ix => (ix,ix) -> MutableArray# s elt
	      -> STRep s (Array ix elt)
{-# INLINE done #-}
done ixs marr = \s1 -> case unsafeFreezeArray# marr s1		of { (# s2, arr #) ->
		       (# s2, Array ixs arr #) }

arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"


-----------------------------------------------------------------------
-- these also go better with magic: (//), accum, accumArray

old_array // ivs
  = runST (do
	-- copy the old array:
	arr <- thawArray old_array
	-- now write the new elements into the new array:
	fill_it_in arr ivs
	freezeArray arr
    )

fill_it_in :: Ix ix => MutableArray s ix elt -> [(ix, elt)] -> ST s ()
fill_it_in arr lst
  = foldr fill_one_in (return ()) lst
  where  -- **** STRICT **** (but that's OK...)
    fill_one_in (i, v) rst
      = writeArray arr i v >> rst

zap_with_f :: Ix ix => (elt -> elt2 -> elt) -> MutableArray s ix elt -> [(ix,elt2)] -> ST s ()
-- zap_with_f: reads an elem out first, then uses "f" on that and the new value

zap_with_f f arr lst
  = foldr zap_one (return ()) lst
  where
    zap_one (i, new_v) rst = do
        old_v <- readArray  arr i
	writeArray arr i (f old_v new_v)
	rst

accum f old_array ivs
  = runST (do
	-- copy the old array:
	arr <- thawArray old_array
	-- now zap the elements in question with "f":
	zap_with_f f arr ivs
	freezeArray arr
    )

accumArray f zero ixs ivs
  = runST (do
	arr# <- newArray ixs zero
	zap_with_f f  arr# ivs
	freezeArray arr#
    )
\end{code}


%*********************************************************
%*							*
\subsection{Operations on mutable arrays}
%*							*
%*********************************************************

Idle ADR question: What's the tradeoff here between flattening these
datatypes into @MutableArray ix ix (MutableArray# s elt)@ and using
it as is?  As I see it, the former uses slightly less heap and
provides faster access to the individual parts of the bounds while the
code used has the benefit of providing a ready-made @(lo, hi)@ pair as
required by many array-related functions.  Which wins? Is the
difference significant (probably not).

Idle AJG answer: When I looked at the outputted code (though it was 2
years ago) it seems like you often needed the tuple, and we build
it frequently. Now we've got the overloading specialiser things
might be different, though.

\begin{code}
newArray :: Ix ix => (ix,ix) -> elt -> ST s (MutableArray s ix elt)
newCharArray, newIntArray, newWordArray, newAddrArray, newFloatArray, newDoubleArray
	 :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

{-# SPECIALIZE newArray      :: IPr       -> elt -> ST s (MutableArray s Int elt),
				(IPr,IPr) -> elt -> ST s (MutableArray s IPr elt)
  #-}
{-# SPECIALIZE newCharArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newWordArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (MutableByteArray s Int) #-}

newArray ixs init = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newArray# n# init s#)     of { (# s2#, arr# #) ->
    (# s2#, MutableArray ixs arr# #) }}

newCharArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newCharArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

newIntArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newIntArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

newWordArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newWordArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

newAddrArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newAddrArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

newFloatArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newFloatArray# n# s#)	  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

newDoubleArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newDoubleArray# n# s#)  of { (# s2#, barr# #) ->
    (# s2#, MutableByteArray ixs barr# #) }}

boundsOfArray     :: Ix ix => MutableArray s ix elt -> (ix, ix)  

{-# SPECIALIZE boundsOfArray     :: MutableArray s Int elt -> IPr #-}

boundsOfArray     (MutableArray     ixs _) = ixs

readArray   	:: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

readCharArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
readIntArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
readWordArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Word
readAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Addr
readFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> ST s Float
readDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> ST s Double

{-# SPECIALIZE readArray       :: MutableArray s Int elt -> Int -> ST s elt,
				  MutableArray s IPr elt -> IPr -> ST s elt
  #-}
{-# SPECIALIZE readCharArray   :: MutableByteArray s Int -> Int -> ST s Char #-}
{-# SPECIALIZE readIntArray    :: MutableByteArray s Int -> Int -> ST s Int #-}
{-# SPECIALIZE readAddrArray   :: MutableByteArray s Int -> Int -> ST s Addr #-}
--NO:{-# SPECIALIZE readFloatArray  :: MutableByteArray s Int -> Int -> ST s Float #-}
{-# SPECIALIZE readDoubleArray :: MutableByteArray s Int -> Int -> ST s Double #-}

readArray (MutableArray ixs arr#) n = ST $ \ s# ->
    case (index ixs n)	    	of { I# n# ->
    case readArray# arr# n# s#	of { (# s2#, r #) ->
    (# s2#, r #) }}

readCharArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, C# r# #) }}

readIntArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, I# r# #) }}

readWordArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readWordArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, W# r# #) }}

readAddrArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, A# r# #) }}

readFloatArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, F# r# #) }}

readDoubleArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n) 	    	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { (# s2#, r# #) ->
    (# s2#, D# r# #) }}

--Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
indexCharArray   :: Ix ix => ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => ByteArray ix -> ix -> Int
indexWordArray   :: Ix ix => ByteArray ix -> ix -> Word
indexAddrArray   :: Ix ix => ByteArray ix -> ix -> Addr
indexFloatArray  :: Ix ix => ByteArray ix -> ix -> Float
indexDoubleArray :: Ix ix => ByteArray ix -> ix -> Double

{-# SPECIALIZE indexCharArray   :: ByteArray Int -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: ByteArray Int -> Int -> Int #-}
{-# SPECIALIZE indexAddrArray   :: ByteArray Int -> Int -> Addr #-}
--NO:{-# SPECIALIZE indexFloatArray  :: ByteArray Int -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: ByteArray Int -> Int -> Double #-}

indexCharArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexWordArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexWordArray# barr# n# 	of { r# ->
    (W# r#)}}

indexAddrArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexAddrArray# barr# n# 	of { r# ->
    (A# r#)}}

indexFloatArray (ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (ByteArray ixs barr#) n
  = case (index ixs n) 	    	    	of { I# n# ->
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}

writeArray  	 :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
writeCharArray   :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
writeWordArray   :: Ix ix => MutableByteArray s ix -> ix -> Word -> ST s () 
writeAddrArray   :: Ix ix => MutableByteArray s ix -> ix -> Addr -> ST s () 
writeFloatArray  :: Ix ix => MutableByteArray s ix -> ix -> Float -> ST s () 
writeDoubleArray :: Ix ix => MutableByteArray s ix -> ix -> Double -> ST s () 

{-# SPECIALIZE writeArray  	:: MutableArray s Int elt -> Int -> elt -> ST s (),
				   MutableArray s IPr elt -> IPr -> elt -> ST s ()
  #-}
{-# SPECIALIZE writeCharArray   :: MutableByteArray s Int -> Int -> Char -> ST s () #-}
{-# SPECIALIZE writeIntArray    :: MutableByteArray s Int -> Int -> Int  -> ST s () #-}
{-# SPECIALIZE writeAddrArray   :: MutableByteArray s Int -> Int -> Addr -> ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: MutableByteArray s Int -> Int -> Float -> ST s () #-}
{-# SPECIALIZE writeDoubleArray :: MutableByteArray s Int -> Int -> Double -> ST s () #-}

writeArray (MutableArray ixs arr#) n ele = ST $ \ s# ->
    case index ixs n		    of { I# n# ->
    case writeArray# arr# n# ele s# of { s2# ->
    (# s2#, () #) }}

writeCharArray (MutableByteArray ixs barr#) n (C# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeIntArray (MutableByteArray ixs barr#) n (I# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    (# s2#, () #) }}

writeWordArray (MutableByteArray ixs barr#) n (W# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeWordArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeAddrArray (MutableByteArray ixs barr#) n (A# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    (# s2#, () #) }}

writeFloatArray (MutableByteArray ixs barr#) n (F# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    (# s2#, () #) }}

writeDoubleArray (MutableByteArray ixs barr#) n (D# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    (# s2#, () #) }}
\end{code}


%*********************************************************
%*							*
\subsection{Moving between mutable and immutable}
%*							*
%*********************************************************

\begin{code}
freezeArray	  :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)
freezeCharArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeIntArray    :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeWordArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeAddrArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALISE freezeArray :: MutableArray s Int elt -> ST s (Array Int elt),
			      MutableArray s IPr elt -> ST s (Array IPr elt)
  #-}
{-# SPECIALISE freezeCharArray :: MutableByteArray s Int -> ST s (ByteArray Int) #-}

freezeArray (MutableArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, Array ixs frozen# #) }}
  where
    freeze  :: MutableArray# s ele	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, Array# ele #)
    freeze m_arr# n# s#
      = case newArray# n# init s#	      of { (# s2#, newarr1# #) ->
	case copy 0# n# m_arr# newarr1# s2#   of { (# s3#, newarr2# #) ->
	unsafeFreezeArray# newarr2# s3#
	}}
      where
	init = error "freezeArray: element not copied"

	copy :: Int# -> Int#
	     -> MutableArray# s ele 
	     -> MutableArray# s ele
	     -> State# s
	     -> (# State# s, MutableArray# s ele #)

	copy cur# end# from# to# st#
	  | cur# ==# end#
	    = (# st#, to# #)
	  | otherwise
	    = case readArray#  from# cur#     st#  of { (# s1#, ele #) ->
	      case writeArray# to#   cur# ele s1# of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeCharArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze arr1# n# s1#
      = case (newCharArray# n# s1#)    	    of { (# s2#, newarr1# #) ->
	case copy 0# n# arr1# newarr1# s2#  of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# end# from# to# st#
	  | cur# ==# end#
	    = (# st#, to# #)
	  | otherwise
	    = case (readCharArray#  from# cur#     st#) of { (# s2#, ele #) ->
	      case (writeCharArray# to#   cur# ele s2#) of { s3# ->
	      copy (cur# +# 1#) end# from# to# s3#
	      }}

freezeIntArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze m_arr# n# s#
      = case (newIntArray# n# s#)    	     of { (# s2#, newarr1# #) ->
	case copy 0# n# m_arr# newarr1# s2#  of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# end# from# to# s1#
	  | cur# ==# end#
	    = (# s1#, to# #)
	  | otherwise
	    = case (readIntArray#  from# cur#     s1#) of { (# s2#, ele #) ->
	      case (writeIntArray# to#   cur# ele s2#) of { s3# ->
	      copy (cur# +# 1#) end# from# to# s3#
	      }}

freezeWordArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze m_arr# n# s1#
      = case (newWordArray# n# s1#)    	     of { (# s2#, newarr1# #) ->
	case copy 0# n# m_arr# newarr1# s2#  of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# end# from# to# st#
	  | cur# ==# end#  = (# st#, to# #)
	  | otherwise      =
	     case (readWordArray#  from# cur#     st#) of { (# s2#, ele #) ->
	     case (writeWordArray# to#   cur# ele s2#) of { s3# ->
	     copy (cur# +# 1#) end# from# to# s3#
	     }}

freezeAddrArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> (# State# s, ByteArray# #)

    freeze m_arr# n# s1#
      = case (newAddrArray# n# s1#)    	     of { (# s2#, newarr1# #) ->
	case copy 0# n# m_arr# newarr1# s2#  of { (# s3#, newarr2# #) ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> (# State# s, MutableByteArray# s #)

	copy cur# end# from# to# st#
	  | cur# ==# end#
	    = (# st#, to# #)
	  | otherwise
	    = case (readAddrArray#  from# cur#     st#)  of { (# st1#, ele #) ->
	      case (writeAddrArray# to#   cur# ele st1#) of { st2# ->
	      copy (cur# +# 1#) end# from# to# st2#
	      }}

unsafeFreezeArray     :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)  
unsafeFreezeByteArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALIZE unsafeFreezeByteArray :: MutableByteArray s Int -> ST s (ByteArray Int)
  #-}

unsafeFreezeArray (MutableArray ixs arr#) = ST $ \ s# ->
    case unsafeFreezeArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, Array ixs frozen# #) }

unsafeFreezeByteArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, ByteArray ixs frozen# #) }


--This takes a immutable array, and copies it into a mutable array, in a
--hurry.

{-# SPECIALISE thawArray :: Array Int elt -> ST s (MutableArray s Int elt),
			    Array IPr elt -> ST s (MutableArray s IPr elt)
  #-}

thawArray :: Ix ix => Array ix elt -> ST s (MutableArray s ix elt)
thawArray (Array ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case thaw arr# n# s# of { (# s2#, thawed# #) ->
    (# s2#, MutableArray ixs thawed# #)}}
  where
    thaw  :: Array# ele			-- the thing
	    -> Int#			-- size of thing to be thawed
	    -> State# s			-- the Universe and everything
	    -> (# State# s, MutableArray# s ele #)

    thaw arr1# n# s#
      = case newArray# n# init s#	      of { (# s2#, newarr1# #) ->
	copy 0# n# arr1# newarr1# s2# }
      where
	init = error "thawArray: element not copied"

	copy :: Int# -> Int#
	     -> Array# ele 
	     -> MutableArray# s ele
	     -> State# s
	     -> (# State# s, MutableArray# s ele #)

	copy cur# end# from# to# st#
	  | cur# ==# end#
	    = (# st#, to# #)
	  | otherwise
	    = case indexArray#  from# cur#        of { (# ele #) ->
	      case writeArray# to#   cur# ele st# of { s1# ->
	      copy (cur# +# 1#) end# from# to# s1#
	      }}

-- this is a quicker version of the above, just flipping the type
-- (& representation) of an immutable array. And placing a
-- proof obligation on the programmer.
unsafeThawArray :: Ix ix => Array ix elt -> ST s (MutableArray s ix elt)
unsafeThawArray (Array ixs arr#) = ST $ \ s# ->
   case unsafeThawArray# arr# s# of
      (# s2#, marr# #) -> (# s2#, MutableArray ixs marr# #)
\end{code}
