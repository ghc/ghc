%
% (c) The AQUA Project, Glasgow University, 1994
%
\section[PreludeGlaST]{Basic ``state transformer'' monad, mutable arrays and variables}

See state-interface.verb, from which this is taken directly.

\begin{code}
#include "../../includes/platform.h"
#include "../../includes/GhcConstants.h"

module PreludeGlaST (
	PreludeGlaST.. ,
	_MutableArray(..),
	_MutableByteArray(..),
	ST(..),		-- it's a known GHC infelicity that synonyms must
	MutableVar(..),	-- be listed separately.

	--!! because this interface is now the "everything state-transformer"ish
	--!! interface, here is all the PreludePrimIO stuff
	
	-- PrimIO(..): no, the compiler already knows about it

	fixPrimIO,
	listPrimIO,
	mapAndUnzipPrimIO,
	mapPrimIO,
	returnPrimIO,
	seqPrimIO,
	thenPrimIO,
	unsafePerformPrimIO,
	unsafeInterleavePrimIO,
	forkPrimIO,

	-- all the Stdio stuff (this is how you get to it)
	-- (well, why not?)
	fclose, fdopen, fflush, fopen, fread, freopen,
	fwrite, _FILE(..),

	-- backward compatibility -- don't use!
	readChanPrimIO,
	appendChanPrimIO,
	appendFilePrimIO,
	getArgsPrimIO,
	
	--!! end of PreludePrimIO

	_ByteArray(..), Array(..) -- reexport *unabstractly*
    ) where

import PreludePrimIO	(
	fixPrimIO,
	listPrimIO,
	mapAndUnzipPrimIO,
	mapPrimIO,
	returnPrimIO,
	seqPrimIO,
	thenPrimIO,
	unsafePerformPrimIO,
	unsafeInterleavePrimIO,
--	forkPrimIO,
	readChanPrimIO,
	appendChanPrimIO,
	appendFilePrimIO,
	getArgsPrimIO
	)
import Stdio

import Cls
import Core
import IInt
import ITup2
import List		( map, null, foldr, (++) )
import PS		( _PackedString, _unpackPS )
import TyArray		( Array(..), _ByteArray(..) )
import TyComplex
import Text

infixr 9 `thenST`, `thenStrictlyST`, `seqST`, `seqStrictlyST`

type IPr = (Int, Int)
\end{code}

%************************************************************************
%*									*
\subsection[PreludeGlaST-ST-monad]{The state-transformer proper}
%*									*
%************************************************************************

\begin{code}
--BUILT-IN: type _ST s a	-- State transformer

type ST s a = _ST s a	-- so you don't need -fglasgow-exts

{-# INLINE returnST #-}
{-# INLINE returnStrictlyST #-}
{-# INLINE thenStrictlyST #-}
{-# INLINE seqStrictlyST #-}

returnST :: a -> _ST s a
returnST a s = (a, s)

thenST :: _ST s a -> (a -> _ST s b) -> _ST s b
thenST m k s = let (r,new_s) = m s
               in
               k r new_s

seqST :: _ST s a -> _ST s b -> _ST s b
seqST m1 m2 = m1 `thenST` (\ _ -> m2)


{-# GENERATE_SPECS returnStrictlyST a #-}
returnStrictlyST :: a -> _ST s a

{-# GENERATE_SPECS thenStrictlyST a b #-}
thenStrictlyST :: _ST s a -> (a -> _ST s b) -> _ST s b

{-# GENERATE_SPECS seqStrictlyST a b #-}
seqStrictlyST :: _ST s a -> _ST s b -> _ST s b


returnStrictlyST a s@(S# _) = (a, s)

thenStrictlyST m k s	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
  = case (m s) of { (r, new_s) ->
    k r new_s }

seqStrictlyST m k s	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
  = case (m s) of { (_, new_s) ->
    k new_s }


-- BUILT-IN: _runST (see Builtin.hs)

unsafeInterleaveST :: _ST s a -> _ST s a    -- ToDo: put in state-interface.tex
unsafeInterleaveST m s
  = let
	(r, new_s) = m s
    in
    (r, s)


fixST :: (a -> _ST s a) -> _ST s a
fixST k s = let ans = k r s
                (r,new_s) = ans
            in
            ans

listST :: [_ST s a] -> _ST s [a]
listST []     = returnST []
listST (m:ms) = m		`thenST` \ x  ->
		listST ms	`thenST` \ xs ->
		returnST (x:xs)

mapST :: (a -> _ST s b) -> [a] -> _ST s [b]
mapST f ms = listST (map f ms)

mapAndUnzipST :: (a -> _ST s (b,c)) -> [a] -> _ST s ([b],[c])
mapAndUnzipST f [] = returnST ([], [])
mapAndUnzipST f (m:ms)
  = f m			`thenST` \ ( r1,  r2) ->
    mapAndUnzipST f ms	`thenST` \ (rs1, rs2) ->
    returnST (r1:rs1, r2:rs2)

forkST :: ST s a -> ST s a

#ifndef __CONCURRENT_HASKELL__
forkST x = x
#else

forkST action s
 = let
    (r, new_s) = action s
   in
    new_s `_fork_` (r, s)
 where
    _fork_ x y = case (fork# x) of { 0# -> parError#; _ -> y }

#endif {- concurrent -}

forkPrimIO :: PrimIO a -> PrimIO a
forkPrimIO = forkST
\end{code}

%************************************************************************
%*									*
\subsection[PreludeGlaST-arrays]{Mutable arrays}
%*									*
%************************************************************************

Idle ADR question: What's the tradeoff here between flattening these
datatypes into @_MutableArray ix ix (MutableArray# s elt)@ and using
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
data _MutableArray     s ix elt = _MutableArray     (ix,ix) (MutableArray# s elt)
data _MutableByteArray s ix     = _MutableByteArray (ix,ix) (MutableByteArray# s)

instance _CCallable (_MutableByteArray s ix)
\end{code}

\begin{code}
newArray    :: Ix ix => (ix,ix) -> elt -> _ST s (_MutableArray s ix elt)
newCharArray, newIntArray, newAddrArray, newFloatArray, newDoubleArray
	    :: Ix ix => (ix,ix) -> _ST s (_MutableByteArray s ix) 

{-# SPECIALIZE newArray       :: IPr       -> elt -> _ST s (_MutableArray s Int elt),
				 (IPr,IPr) -> elt -> _ST s (_MutableArray s IPr elt)
  #-}
{-# SPECIALIZE newCharArray   :: IPr -> _ST s (_MutableByteArray s Int) #-}
{-# SPECIALIZE newIntArray    :: IPr -> _ST s (_MutableByteArray s Int) #-}
{-# SPECIALIZE newAddrArray   :: IPr -> _ST s (_MutableByteArray s Int) #-}
{-# SPECIALIZE newFloatArray  :: IPr -> _ST s (_MutableByteArray s Int) #-}
{-# SPECIALIZE newDoubleArray :: IPr -> _ST s (_MutableByteArray s Int) #-}

newArray ixs@(ix_start, ix_end) init (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else (index ixs ix_end) + 1) of { I# x -> x }
	-- size is one bigger than index of last elem
    in
    case (newArray# n# init s#)     of { StateAndMutableArray# s2# arr# ->
    (_MutableArray ixs arr#, S# s2#)}

newCharArray ixs@(ix_start, ix_end) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newCharArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray ixs barr#, S# s2#)}

newIntArray ixs@(ix_start, ix_end) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newIntArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray ixs barr#, S# s2#)}

newAddrArray ixs@(ix_start, ix_end) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newAddrArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray ixs barr#, S# s2#)}

newFloatArray ixs@(ix_start, ix_end) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case (newFloatArray# n# s#)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray ixs barr#, S# s2#)}

newDoubleArray ixs@(ix_start, ix_end) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
--    trace ("newDoubleArray:"++(show (I# n#))) (
    case (newDoubleArray# n# s#)  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray ixs barr#, S# s2#)}
--    )
\end{code}

\begin{code}
boundsOfArray     :: Ix ix => _MutableArray s ix elt -> (ix, ix)  
boundsOfByteArray :: Ix ix => _MutableByteArray s ix -> (ix, ix)

{-# SPECIALIZE boundsOfArray     :: _MutableArray s Int elt -> IPr #-}
{-# SPECIALIZE boundsOfByteArray :: _MutableByteArray s Int -> IPr #-}

boundsOfArray     (_MutableArray     ixs _) = ixs
boundsOfByteArray (_MutableByteArray ixs _) = ixs
\end{code}

\begin{code}
readArray   	:: Ix ix => _MutableArray s ix elt -> ix -> _ST s elt 

readCharArray   :: Ix ix => _MutableByteArray s ix -> ix -> _ST s Char 
readIntArray    :: Ix ix => _MutableByteArray s ix -> ix -> _ST s Int
readAddrArray   :: Ix ix => _MutableByteArray s ix -> ix -> _ST s _Addr
readFloatArray  :: Ix ix => _MutableByteArray s ix -> ix -> _ST s Float
readDoubleArray :: Ix ix => _MutableByteArray s ix -> ix -> _ST s Double

{-# SPECIALIZE readArray       :: _MutableArray s Int elt -> Int -> _ST s elt,
				  _MutableArray s IPr elt -> IPr -> _ST s elt
  #-}
{-# SPECIALIZE readCharArray   :: _MutableByteArray s Int -> Int -> _ST s Char #-}
{-# SPECIALIZE readIntArray    :: _MutableByteArray s Int -> Int -> _ST s Int #-}
{-# SPECIALIZE readAddrArray   :: _MutableByteArray s Int -> Int -> _ST s _Addr #-}
--NO:{-# SPECIALIZE readFloatArray  :: _MutableByteArray s Int -> Int -> _ST s Float #-}
{-# SPECIALIZE readDoubleArray :: _MutableByteArray s Int -> Int -> _ST s Double #-}

readArray (_MutableArray ixs arr#) n (S# s#)
  = case (index ixs n)	    	of { I# n# ->
    case readArray# arr# n# s#	of { StateAndPtr# s2# r ->
    (r, S# s2#)}}

readCharArray (_MutableByteArray ixs barr#) n (S# s#)
  = case (index ixs n)	    	    	of { I# n# ->
    case readCharArray# barr# n# s#	of { StateAndChar# s2# r# ->
    (C# r#, S# s2#)}}

readIntArray (_MutableByteArray ixs barr#) n (S# s#)
  = case (index ixs n)	    	    	of { I# n# ->
    case readIntArray# barr# n# s#	of { StateAndInt# s2# r# ->
    (I# r#, S# s2#)}}

readAddrArray (_MutableByteArray ixs barr#) n (S# s#)
  = case (index ixs n)	    	    	of { I# n# ->
    case readAddrArray# barr# n# s#	of { StateAndAddr# s2# r# ->
    (A# r#, S# s2#)}}

readFloatArray (_MutableByteArray ixs barr#) n (S# s#)
  = case (index ixs n)	    	    	of { I# n# ->
    case readFloatArray# barr# n# s#	of { StateAndFloat# s2# r# ->
    (F# r#, S# s2#)}}

readDoubleArray (_MutableByteArray ixs barr#) n (S# s#)
  = case (index ixs n) 	    	    	of { I# n# ->
--    trace ("readDoubleArray:"++(show (I# n#))) (
    case readDoubleArray# barr# n# s#	of { StateAndDouble# s2# r# ->
    (D# r#, S# s2#)}}
\end{code}

Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
\begin{code}
indexCharArray   :: Ix ix => _ByteArray ix -> ix -> Char 
indexIntArray    :: Ix ix => _ByteArray ix -> ix -> Int
indexAddrArray   :: Ix ix => _ByteArray ix -> ix -> _Addr
indexFloatArray  :: Ix ix => _ByteArray ix -> ix -> Float
indexDoubleArray :: Ix ix => _ByteArray ix -> ix -> Double

{-# SPECIALIZE indexCharArray   :: _ByteArray Int -> Int -> Char #-}
{-# SPECIALIZE indexIntArray    :: _ByteArray Int -> Int -> Int #-}
{-# SPECIALIZE indexAddrArray   :: _ByteArray Int -> Int -> _Addr #-}
--NO:{-# SPECIALIZE indexFloatArray  :: _ByteArray Int -> Int -> Float #-}
{-# SPECIALIZE indexDoubleArray :: _ByteArray Int -> Int -> Double #-}

indexCharArray (_ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexCharArray# barr# n# 	of { r# ->
    (C# r#)}}

indexIntArray (_ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexIntArray# barr# n# 	of { r# ->
    (I# r#)}}

indexAddrArray (_ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexAddrArray# barr# n# 	of { r# ->
    (A# r#)}}

indexFloatArray (_ByteArray ixs barr#) n
  = case (index ixs n)	    	    	of { I# n# ->
    case indexFloatArray# barr# n# 	of { r# ->
    (F# r#)}}

indexDoubleArray (_ByteArray ixs barr#) n
  = case (index ixs n) 	    	    	of { I# n# ->
--    trace ("indexDoubleArray:"++(show (I# n#))) (
    case indexDoubleArray# barr# n# 	of { r# ->
    (D# r#)}}
\end{code}

Indexing off @_Addrs@ is similar, and therefore given here.
\begin{code}
indexCharOffAddr   :: _Addr -> Int -> Char
indexIntOffAddr    :: _Addr -> Int -> Int
indexAddrOffAddr   :: _Addr -> Int -> _Addr
indexFloatOffAddr  :: _Addr -> Int -> Float
indexDoubleOffAddr :: _Addr -> Int -> Double

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
\end{code}

\begin{code}
writeArray  	 :: Ix ix => _MutableArray s ix elt -> ix -> elt -> _ST s () 
writeCharArray   :: Ix ix => _MutableByteArray s ix -> ix -> Char -> _ST s () 
writeIntArray    :: Ix ix => _MutableByteArray s ix -> ix -> Int  -> _ST s () 
writeAddrArray   :: Ix ix => _MutableByteArray s ix -> ix -> _Addr -> _ST s () 
writeFloatArray  :: Ix ix => _MutableByteArray s ix -> ix -> Float -> _ST s () 
writeDoubleArray :: Ix ix => _MutableByteArray s ix -> ix -> Double -> _ST s () 

{-# SPECIALIZE writeArray  	:: _MutableArray s Int elt -> Int -> elt -> _ST s (),
				   _MutableArray s IPr elt -> IPr -> elt -> _ST s ()
  #-}
{-# SPECIALIZE writeCharArray   :: _MutableByteArray s Int -> Int -> Char -> _ST s () #-}
{-# SPECIALIZE writeIntArray    :: _MutableByteArray s Int -> Int -> Int  -> _ST s () #-}
{-# SPECIALIZE writeAddrArray   :: _MutableByteArray s Int -> Int -> _Addr -> _ST s () #-}
--NO:{-# SPECIALIZE writeFloatArray  :: _MutableByteArray s Int -> Int -> Float -> _ST s () #-}
{-# SPECIALIZE writeDoubleArray :: _MutableByteArray s Int -> Int -> Double -> _ST s () #-}

writeArray (_MutableArray ixs arr#) n ele (S# s#)
  = case index ixs n		    of { I# n# ->
    case writeArray# arr# n# ele s# of { s2# ->
    ((), S# s2#)}}

writeCharArray (_MutableByteArray ixs barr#) n (C# ele) (S# s#)
  = case (index ixs n)	    	    	    of { I# n# ->
    case writeCharArray# barr# n# ele s#    of { s2#   ->
    ((), S# s2#)}}

writeIntArray (_MutableByteArray ixs barr#) n (I# ele) (S# s#)
  = case (index ixs n)	    	    	    of { I# n# ->
    case writeIntArray# barr# n# ele s#     of { s2#   ->
    ((), S# s2#)}}

writeAddrArray (_MutableByteArray ixs barr#) n (A# ele) (S# s#)
  = case (index ixs n)	    	    	    of { I# n# ->
    case writeAddrArray# barr# n# ele s#    of { s2#   ->
    ((), S# s2#)}}

writeFloatArray (_MutableByteArray ixs barr#) n (F# ele) (S# s#)
  = case (index ixs n)	    	    	    of { I# n# ->
    case writeFloatArray# barr# n# ele s#   of { s2#   ->
    ((), S# s2#)}}

writeDoubleArray (_MutableByteArray ixs barr#) n (D# ele) (S# s#)
  = case (index ixs n)	    	    	    of { I# n# ->
--    trace ("writeDoubleArray:"++(show (I# n#))) (
    case writeDoubleArray# barr# n# ele s#  of { s2#   ->
    ((), S# s2#)}}
\end{code}

\begin{code}
freezeArray 	  :: Ix ix => _MutableArray s ix elt -> _ST s (Array ix elt)
freezeCharArray   :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)
freezeIntArray    :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)
freezeAddrArray   :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)
freezeFloatArray  :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)
freezeDoubleArray :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)

{-# SPECIALISE freezeArray :: _MutableArray s Int elt -> _ST s (Array Int elt),
			      _MutableArray s IPr elt -> _ST s (Array IPr elt)
  #-}
{-# SPECIALISE freezeCharArray :: _MutableByteArray s Int -> _ST s (_ByteArray Int) #-}

freezeArray (_MutableArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else (index ixs ix_end) + 1) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndArray# s2# frozen# ->
    (_Array ixs frozen#, S# s2#)}
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
	init = error "freezeArr: element not copied"

	copy :: Int# -> Int#
	     -> MutableArray# s ele -> MutableArray# s ele
	     -> State# s
	     -> StateAndMutableArray# s ele

	copy cur# end# from# to# s#
	  | cur# ==# end#
	    = StateAndMutableArray# s# to#
	  | True
	    = case readArray#  from# cur#     s#  of { StateAndPtr# s1# ele ->
	      case writeArray# to#   cur# ele s1# of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeCharArray (_MutableByteArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
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
	  | True
	    = case (readCharArray#  from# cur#     s#)  of { StateAndChar# s1# ele ->
	      case (writeCharArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeIntArray (_MutableByteArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
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
	  | True
	    = case (readIntArray#  from# cur#     s#)  of { StateAndInt# s1# ele ->
	      case (writeIntArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeAddrArray (_MutableByteArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
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
	  | True
	    = case (readAddrArray#  from# cur#     s#)  of { StateAndAddr# s1# ele ->
	      case (writeAddrArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeFloatArray (_MutableByteArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
  where
    freeze  :: MutableByteArray# s	-- the thing
	    -> Int#			-- size of thing to be frozen
	    -> State# s			-- the Universe and everything
	    -> StateAndByteArray# s

    freeze arr# n# s#
      = case (newFloatArray# n# s#)    	   of { StateAndMutableByteArray# s2# newarr1# ->
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
	  | True
	    = case (readFloatArray#  from# cur#     s#)  of { StateAndFloat# s1# ele ->
	      case (writeFloatArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}

freezeDoubleArray (_MutableByteArray ixs@(ix_start, ix_end) arr#) (S# s#)
  = let n# = case (if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)) of { I# x -> x }
    in
    case freeze arr# n# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
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
	  | True
	    = case (readDoubleArray#  from# cur#     s#)  of { StateAndDouble# s1# ele ->
	      case (writeDoubleArray# to#   cur# ele s1#) of { s2# ->
	      copy (cur# +# 1#) end# from# to# s2#
	      }}
\end{code}

\begin{code}
unsafeFreezeArray     :: Ix ix => _MutableArray s ix elt -> _ST s (Array ix elt)  
unsafeFreezeByteArray :: Ix ix => _MutableByteArray s ix -> _ST s (_ByteArray ix)

{-# SPECIALIZE unsafeFreezeByteArray :: _MutableByteArray s Int -> _ST s (_ByteArray Int)
  #-}

unsafeFreezeArray (_MutableArray ixs arr#) (S# s#)
  = case unsafeFreezeArray# arr# s# of { StateAndArray# s2# frozen# ->
    (_Array ixs frozen#, S# s2#) }

unsafeFreezeByteArray (_MutableByteArray ixs arr#) (S# s#)
  = case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
\end{code}

\begin{code}
sameMutableArray     :: _MutableArray s ix elt -> _MutableArray s ix elt -> Bool
sameMutableByteArray :: _MutableByteArray s ix -> _MutableByteArray s ix -> Bool

sameMutableArray (_MutableArray _ arr1#) (_MutableArray _ arr2#)
  = sameMutableArray# arr1# arr2#

sameMutableByteArray (_MutableByteArray _ arr1#) (_MutableByteArray _ arr2#)
  = sameMutableByteArray# arr1# arr2#
\end{code}

%************************************************************************
%*									*
\subsection[PreludeGlaST-variables]{Variables}
%*									*
%************************************************************************

\begin{code}
type MutableVar s a = _MutableArray s Int a
\end{code}

\begin{code}
newVar   :: a -> _ST s (MutableVar s a)
readVar  :: MutableVar s a -> _ST s a
writeVar :: MutableVar s a -> a -> _ST s ()
sameVar  :: MutableVar s a -> MutableVar s a -> Bool

{- MUCH GRATUITOUS INEFFICIENCY: WDP 95/09:

newVar init    s = newArray (0,0) init s
readVar v      s = readArray v 0 s
writeVar v val s = writeArray v 0 val s
sameVar v1 v2    = sameMutableArray v1 v2
-}

newVar init (S# s#)
  = case (newArray# 1# init s#)     of { StateAndMutableArray# s2# arr# ->
    (_MutableArray vAR_IXS arr#, S# s2#) }
  where
    vAR_IXS = error "Shouldn't access `bounds' of a MutableVar\n"

readVar (_MutableArray _ var#) (S# s#)
  = case readArray# var# 0# s#	of { StateAndPtr# s2# r ->
    (r, S# s2#) }

writeVar (_MutableArray _ var#) val (S# s#)
  = case writeArray# var# 0# val s# of { s2# ->
    ((), S# s2#) }

sameVar (_MutableArray _ var1#) (_MutableArray _ var2#)
  = sameMutableArray# var1# var2#
\end{code}
