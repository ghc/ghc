%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[ArrBase]{Module @ArrBase@}

Array implementation, @ArrBase@ exports the basic array
types and operations.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module ArrBase where

import {-# SOURCE #-} GHCerr ( error )
import Ix
import PrelList (foldl)
import STBase
import PrelBase
import CCall
import Addr
import UnsafeST ( runST )
import GHC

infixl 9  !, //
\end{code}

\begin{code}
{-# GENERATE_SPECS array a{~,Int,IPr} b{} #-}
array		      :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

{-# GENERATE_SPECS (!) a{~,Int,IPr} b{} #-}
(!)		      :: (Ix a) => Array a b -> a -> b

bounds		      :: (Ix a) => Array a b -> (a,a)

{-# GENERATE_SPECS (//) a{~,Int,IPr} b{} #-}
(//)		      :: (Ix a) => Array a b -> [(a,b)] -> Array a b

{-# GENERATE_SPECS accum a{~,Int,IPr} b{} c{} #-}
accum		      :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b

{-# GENERATE_SPECS accumArray a{~,Int,IPr} b{} c{} #-}
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

-- A one-element mutable array:
type MutableVar s a = MutableArray s Int a

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
    case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    STret s2# (MutableArray vAR_IXS arr#) }
  where
    vAR_IXS = error "newVar: Shouldn't access `bounds' of a MutableVar\n"

readVar (MutableArray _ var#) = ST $ \ s# ->
    case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    STret s2# r }

writeVar (MutableArray _ var#) val = ST $ \ s# ->
    case writeArray# var# 0# val s# of { s2# ->
    STret s2# () }
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
      Lift v -> v

#ifdef USE_FOLDR_BUILD
{-# INLINE array #-}
#endif
array ixs@(ix_start, ix_end) ivs =
   runST ( ST $ \ s ->
	case (newArray ixs arrEleBottom)	of { ST new_array_thing ->
	case (new_array_thing s)		of { STret s# arr@(MutableArray _ arr#) ->
	let
	 fill_in s# [] = s#
	 fill_in s# ((i,v):ivs) =
		case (index ixs i)	      of { I# n# ->
		case writeArray# arr# n# v s# of { s2# -> 
		fill_in s2# ivs }}
	in

	case (fill_in s# ivs)	 		of { s# -> 
	case (freezeArray arr)			of { ST freeze_array_thing ->
	freeze_array_thing s# }}}})

arrEleBottom = error "(Array.!): undefined array element"

fill_it_in :: Ix ix => MutableArray s ix elt -> [(ix, elt)] -> ST s ()
fill_it_in arr lst
  = foldr fill_one_in (return ()) lst
  where  -- **** STRICT **** (but that's OK...)
    fill_one_in (i, v) rst
      = writeArray arr i v >> rst

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
  where
    bottom = error "(Array.//): error in copying old array\n"

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
  where
    bottom = error "Array.accum: error in copying old array\n"

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
newCharArray, newIntArray, newAddrArray, newFloatArray, newDoubleArray
	 :: Ix ix => (ix,ix) -> ST s (MutableByteArray s ix) 

{-# SPECIALIZE newArray      :: IPr       -> elt -> ST s (MutableArray s Int elt),
				(IPr,IPr) -> elt -> ST s (MutableArray s IPr elt)
  #-}
{-# SPECIALIZE newCharArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> ST s (MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> ST s (MutableByteArray s Int) #-}

newArray ixs init = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newArray# n# init s#)     of { StateAndMutableArray# s2# arr# ->
    STret s2# (MutableArray ixs arr#) }}

newCharArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newCharArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

newIntArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newIntArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

newAddrArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newAddrArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

newFloatArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newFloatArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

newDoubleArray ixs = ST $ \ s# ->
    case rangeSize ixs              of { I# n# ->
    case (newDoubleArray# n# s#)  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray ixs barr#) }}

boundsOfArray     :: Ix ix => MutableArray s ix elt -> (ix, ix)  
boundsOfByteArray :: Ix ix => MutableByteArray s ix -> (ix, ix)

{-# SPECIALIZE boundsOfArray     :: MutableArray s Int elt -> IPr #-}
{-# SPECIALIZE boundsOfByteArray :: MutableByteArray s Int -> IPr #-}

boundsOfArray     (MutableArray     ixs _) = ixs
boundsOfByteArray (MutableByteArray ixs _) = ixs

readArray   	:: Ix ix => MutableArray s ix elt -> ix -> ST s elt 

readCharArray   :: Ix ix => MutableByteArray s ix -> ix -> ST s Char 
readIntArray    :: Ix ix => MutableByteArray s ix -> ix -> ST s Int
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
    case readArray# arr# n# s#	of { StateAndPtr# s2# r ->
    STret s2# r }}

readCharArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { StateAndChar# s2# r# ->
    STret s2# (C# r#) }}

readIntArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { StateAndInt# s2# r# ->
    STret s2# (I# r#) }}

readAddrArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { StateAndAddr# s2# r# ->
    STret s2# (A# r#) }}

readFloatArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n)	    	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { StateAndFloat# s2# r# ->
    STret s2# (F# r#) }}

readDoubleArray (MutableByteArray ixs barr#) n = ST $ \ s# ->
    case (index ixs n) 	    	    	of { I# n# ->
    case readDoubleArray# barr# n# s#	of { StateAndDouble# s2# r# ->
    STret s2# (D# r#) }}

--Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
indexCharArray   :: Ix ix => ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => ByteArray ix -> ix -> Int
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

--Indexing off @Addrs@ is similar, and therefore given here.
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double

indexCharOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexCharOffAddr# addr# n# 	of { r# ->
    (C# r#)}}

indexIntOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexIntOffAddr# addr# n# 	of { r# ->
    (I# r#)}}

indexAddrOffAddr (A# addr#) n
  = case n  	    	    	    	of { I# n# ->
    case indexAddrOffAddr# addr# n# 	of { r# ->
    (A# r#)}}

indexFloatOffAddr (A# addr#) n
  = case n  	    		    	of { I# n# ->
    case indexFloatOffAddr# addr# n# 	of { r# ->
    (F# r#)}}

indexDoubleOffAddr (A# addr#) n
  = case n  	    	 	    	of { I# n# ->
    case indexDoubleOffAddr# addr# n# 	of { r# ->
    (D# r#)}}

writeArray  	 :: Ix ix => MutableArray s ix elt -> ix -> elt -> ST s () 
writeCharArray   :: Ix ix => MutableByteArray s ix -> ix -> Char -> ST s () 
writeIntArray    :: Ix ix => MutableByteArray s ix -> ix -> Int  -> ST s () 
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
    STret s2# () }}

writeCharArray (MutableByteArray ixs barr#) n (C# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    STret s2# () }}

writeIntArray (MutableByteArray ixs barr#) n (I# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    STret s2# () }}

writeAddrArray (MutableByteArray ixs barr#) n (A# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    STret s2# () }}

writeFloatArray (MutableByteArray ixs barr#) n (F# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    STret s2# () }}

writeDoubleArray (MutableByteArray ixs barr#) n (D# ele) = ST $ \ s# ->
    case (index ixs n)	    	    	    of { I# n# ->
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    STret s2# () }}
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
freezeAddrArray   :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeFloatArray  :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)
freezeDoubleArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALISE freezeArray :: MutableArray s Int elt -> ST s (Array Int elt),
			      MutableArray s IPr elt -> ST s (Array IPr elt)
  #-}
{-# SPECIALISE freezeCharArray :: MutableByteArray s Int -> ST s (ByteArray Int) #-}

freezeArray (MutableArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndArray# s2# frozen# ->
    STret s2# (Array ixs frozen#) }}
  where
    freeze  :: MutableArray# s ele	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndArray# s ele

    freeze arr# n# s#
      = case newArray# n# init s#	      of { StateAndMutableArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#     of { StateAndMutableArray# s3# newarr2# ->
	unsafeFreezeArray# newarr2# s3#
	}}
      where
	init = error "freezeArray: element not copied"

	copy :: Int# -> Int#
	     -> MutableArray# s ele -> MutableArray# s ele
	     -> State# s
	     -> StateAndMutableArray# s ele

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableArray# s# to#
	  | otherwise
	    = case readArray#  from# cur#     s#  of { StateAndPtr# s1# ele ->
	      case writeArray# to#   cur# ele s1# of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeCharArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newCharArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readCharArray#  from# cur#     s#)  of { StateAndChar# s1# ele ->
	      case (writeCharArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeIntArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newIntArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readIntArray#  from# cur#     s#)  of { StateAndInt# s1# ele ->
	      case (writeIntArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeAddrArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newAddrArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readAddrArray#  from# cur#     s#)  of { StateAndAddr# s1# ele ->
	      case (writeAddrArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeFloatArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# end# s#
      = case (newFloatArray# end# s#)   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readFloatArray#  from# cur#     s#)  of { StateAndFloat# s1# ele ->
	      case (writeFloatArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) from# to# s2#
	      }}

freezeDoubleArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }}
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newDoubleArray# n# s#)   	   of { StateAndMutableByteArray# s2# newarr1# ->
	case copy 0# n# arr# newarr1# s2#  of { StateAndMutableByteArray# s3# newarr2# ->
	unsafeFreezeByteArray# newarr2# s3#
	}}
      where
	copy :: Int# -> Int#
	     -> MutableByteArray# s -> MutableByteArray# s
	     -> State# s
	     -> StateAndMutableByteArray# s

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableByteArray# s# to#
	  | otherwise
	    = case (readDoubleArray#  from# cur#     s#)  of { StateAndDouble# s1# ele ->
	      case (writeDoubleArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

unsafeFreezeArray     :: Ix ix => MutableArray s ix elt -> ST s (Array ix elt)  
unsafeFreezeByteArray :: Ix ix => MutableByteArray s ix -> ST s (ByteArray ix)

{-# SPECIALIZE unsafeFreezeByteArray :: MutableByteArray s Int -> ST s (ByteArray Int)
  #-}

unsafeFreezeArray (MutableArray ixs arr#) = ST $ \ s# ->
    case unsafeFreezeArray# arr# s# of { StateAndArray# s2# frozen# ->
    STret s2# (Array ixs frozen#) }

unsafeFreezeByteArray (MutableByteArray ixs arr#) = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }


--This takes a immutable array, and copies it into a mutable array, in a
--hurry.

{-# SPECIALISE thawArray :: Array Int elt -> ST s (MutableArray s Int elt),
			    Array IPr elt -> ST s (MutableArray s IPr elt)
  #-}

thawArray :: Ix ix => Array ix elt -> ST s (MutableArray s ix elt)
thawArray (Array ixs arr#) = ST $ \ s# ->
    case rangeSize ixs     of { I# n# ->
    case thaw arr# n# s# of { StateAndMutableArray# s2# thawed# ->
    STret s2# (MutableArray ixs thawed#)}}
  where
    thaw  :: Array# ele			-- the thing
	    -> Int#			-- size of thing to be thawed
	    -> State# s			-- the Universe and everything
	    -> StateAndMutableArray# s ele

    thaw arr# n# s#
      = case newArray# n# init s#	      of { StateAndMutableArray# s2# newarr1# ->
	copy 0# n# arr# newarr1# s2# }
      where
	init = error "thawArray: element not copied"

	copy :: Int# -> Int#
	     -> Array# ele 
	     -> MutableArray# s ele
	     -> State# s
	     -> StateAndMutableArray# s ele

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableArray# s# to#
	  | otherwise
	    = case indexArray#  from# cur#       of { Lift ele ->
	      case writeArray# to#   cur# ele s# of { s1# ->
	      copy (cur# +# 1#) end# from# to# s1#
	      }}
\end{code}

%*********************************************************
%*							*
\subsection{Ghastly return types}
%*							*
%*********************************************************

\begin{code}
data StateAndArray#            s elt = StateAndArray#        (State# s) (Array# elt) 
data StateAndMutableArray#     s elt = StateAndMutableArray# (State# s) (MutableArray# s elt)
data StateAndByteArray#        s = StateAndByteArray#        (State# s) ByteArray# 
data StateAndMutableByteArray# s = StateAndMutableByteArray# (State# s) (MutableByteArray# s)
\end{code}
