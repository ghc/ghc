%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[PrelArr]{Module @PrelArr@}

Array implementation, @PrelArr@ exports the basic array
types and operations.

For byte-arrays see @PrelByteArr@.

\begin{code}
{-# OPTIONS -fcompiling-prelude -fno-implicit-prelude #-}

module PrelArr where

import {-# SOURCE #-} PrelErr ( error )
import PrelList (foldl)
import PrelEnum
import PrelNum
import PrelST
import PrelBase
import PrelAddr
import PrelGHC
import PrelShow

infixl 9  !, //

default ()
\end{code}


%*********************************************************
%*							*
\subsection{The @Ix@ class}
%*							*
%*********************************************************

\begin{code}
class  (Ord a) => Ix a  where
    range		:: (a,a) -> [a]
    index, unsafeIndex	:: (a,a) -> a -> Int
    inRange		:: (a,a) -> a -> Bool

	-- Must specify one of index, unsafeIndex
    index b i | inRange b i = unsafeIndex b i
	      | otherwise   = error "Error in array index"
    unsafeIndex b i = index b i
\end{code}


%*********************************************************
%*							*
\subsection{Instances of @Ix@}
%*							*
%*********************************************************

\begin{code}
-- abstract these errors from the relevant index functions so that
-- the guts of the function will be small enough to inline.

{-# NOINLINE indexError #-}
indexError :: Show a => (a,a) -> a -> String -> b
indexError rng i tp
  = error (showString "Ix{" . showString tp . showString "}.index: Index " .
           showParen True (showsPrec 0 i) .
	   showString " out of range " $
	   showParen True (showsPrec 0 rng) "")

----------------------------------------------------------------------
instance  Ix Char  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = fromEnum i - fromEnum m

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Char"

    inRange (m,n) i	=  m <= i && i <= n

----------------------------------------------------------------------
instance  Ix Int  where
    {-# INLINE range #-}
	-- The INLINE stops the build in the RHS from getting inlined,
	-- so that callers can fuse with the result of range
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i = i - m

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Int"

    {-# INLINE inRange #-}
    inRange (I# m,I# n) (I# i) =  m <=# i && i <=# n

----------------------------------------------------------------------
instance  Ix Integer  where
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (m,_n) i   = fromInteger (i - m)

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Integer"

    inRange (m,n) i	=  m <= i && i <= n


----------------------------------------------------------------------
instance Ix Bool where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Bool"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix Ordering where -- as derived
    {-# INLINE range #-}
    range (m,n) = [m..n]

    {-# INLINE unsafeIndex #-}
    unsafeIndex (l,_) i = fromEnum i - fromEnum l

    index b i | inRange b i =  unsafeIndex b i
	      | otherwise   =  indexError b i "Ordering"

    inRange (l,u) i = fromEnum i >= fromEnum l && fromEnum i <= fromEnum u

----------------------------------------------------------------------
instance Ix () where
    {-# INLINE range #-}
    range   ((), ())    = [()]
    {-# INLINE unsafeIndex #-}
    unsafeIndex   ((), ()) () = 0
    {-# INLINE inRange #-}
    inRange ((), ()) () = True
    {-# INLINE index #-}
    index b i = unsafeIndex b i


----------------------------------------------------------------------
instance (Ix a, Ix b) => Ix (a, b) where -- as derived
    {-# SPECIALISE instance Ix (Int,Int) #-}

    {- INLINE range #-}
    range ((l1,l2),(u1,u2)) =
      [ (i1,i2) | i1 <- range (l1,u1), i2 <- range (l2,u2) ]

    {- INLINE unsafeIndex #-}
    unsafeIndex ((l1,l2),(u1,u2)) (i1,i2) =
      unsafeIndex (l1,u1) i1 * unsafeRangeSize (l2,u2) + unsafeIndex (l2,u2) i2

    {- INLINE inRange #-}
    inRange ((l1,l2),(u1,u2)) (i1,i2) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2

    -- Default method for index

----------------------------------------------------------------------
instance  (Ix a1, Ix a2, Ix a3) => Ix (a1,a2,a3)  where
    {-# SPECIALISE instance Ix (Int,Int,Int) #-}

    range ((l1,l2,l3),(u1,u2,u3)) =
        [(i1,i2,i3) | i1 <- range (l1,u1),
                      i2 <- range (l2,u2),
                      i3 <- range (l3,u3)]

    unsafeIndex ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))

    inRange ((l1,l2,l3),(u1,u2,u3)) (i1,i2,i3) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3

    -- Default method for index

----------------------------------------------------------------------
instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
      [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                       i2 <- range (l2,u2),
                       i3 <- range (l3,u3),
                       i4 <- range (l4,u4)]

    unsafeIndex ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1)))

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4

    -- Default method for index

instance  (Ix a1, Ix a2, Ix a3, Ix a4, Ix a5) => Ix (a1,a2,a3,a4,a5)  where
    range ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) =
      [(i1,i2,i3,i4,i5) | i1 <- range (l1,u1),
                          i2 <- range (l2,u2),
                          i3 <- range (l3,u3),
                          i4 <- range (l4,u4),
                          i5 <- range (l5,u5)]

    unsafeIndex ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      unsafeIndex (l5,u5) i5 + unsafeRangeSize (l5,u5) * (
      unsafeIndex (l4,u4) i4 + unsafeRangeSize (l4,u4) * (
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))))

    inRange ((l1,l2,l3,l4,l5),(u1,u2,u3,u4,u5)) (i1,i2,i3,i4,i5) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3 && inRange (l4,u4) i4 && 
      inRange (l5,u5) i5

    -- Default method for index
\end{code}


%********************************************************
%*							*
\subsection{Size of @Ix@ interval}
%*							*
%********************************************************

The @rangeSize@ operator returns the number of elements
in the range for an @Ix@ pair.

\begin{code}
{-# SPECIALISE unsafeRangeSize :: (Int,Int) -> Int #-}
{-# SPECIALISE unsafeRangeSize :: ((Int,Int),(Int,Int)) -> Int #-}
unsafeRangeSize :: (Ix a) => (a,a) -> Int
unsafeRangeSize b@(_l,h) = unsafeIndex b h + 1

{-# SPECIALISE rangeSize :: (Int,Int) -> Int #-}
{-# SPECIALISE rangeSize :: ((Int,Int),(Int,Int)) -> Int #-}
rangeSize :: (Ix a) => (a,a) -> Int
rangeSize b@(_l,h) | inRange b h = unsafeIndex b h + 1
		   | otherwise   = 0

-- Note that the following is NOT right
--	rangeSize (l,h) | l <= h    = index b h + 1
--			| otherwise = 0
--
-- Because it might be the case that l<h, but the range
-- is nevertheless empty.  Consider
--	((1,2),(2,1))
-- Here l<h, but the second index ranges from 2..1 and
-- hence is empty
\end{code}



%*********************************************************
%*							*
\subsection{The @Array@ types}
%*							*
%*********************************************************

\begin{code}
type IPr = (Int, Int)

data Ix ix => Array     ix elt = Array   ix ix (Array# elt)
data Ix ix => STArray s ix elt = STArray ix ix (MutableArray# s elt)


data STRef s a = STRef (MutVar# s a)

instance Eq (STRef s a) where
	STRef v1# == STRef v2#
		= sameMutVar# v1# v2#

-- just pointer equality on arrays:
instance Eq (STArray s ix elt) where
	STArray _ _ arr1# == STArray _ _ arr2# 
		= sameMutableArray# arr1# arr2#
\end{code}

%*********************************************************
%*							*
\subsection{Operations on mutable variables}
%*							*
%*********************************************************

\begin{code}
newSTRef   :: a -> ST s (STRef s a)
readSTRef  :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()

newSTRef init = ST $ \ s# ->
    case (newMutVar# init s#)     of { (# s2#, var# #) ->
    (# s2#, STRef var# #) }

readSTRef (STRef var#) = ST $ \ s# -> readMutVar# var# s#

writeSTRef (STRef var#) val = ST $ \ s# ->
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
bounds		      :: (Ix a) => Array a b -> (a,a)
{-# INLINE bounds #-}
bounds (Array l u _)  = (l,u)

assocs		      :: (Ix a) => Array a b -> [(a,b)]
{-# INLINE assocs #-}	-- Want to fuse the list comprehension
assocs a              =  [(i, a!i) | i <- indices a]

indices		      :: (Ix a) => Array a b -> [a]
{-# INLINE indices #-}
indices		      =  range . bounds

{-# SPECIALISE amap :: (b -> c) -> Array Int b -> Array Int c #-}
amap		      :: (Ix a) => (b -> c) -> Array a b -> Array a c
amap f a              =  array b [(i, f (a!i)) | i <- range b]
                         where b = bounds a

{-# SPECIALISE (!) :: Array Int b -> Int -> b #-}
(!)		      :: (Ix a) => Array a b -> a -> b
(Array l u arr#) ! i
  = let n# = case (index (l,u) i) of { I# x -> x } -- index fails if out of range
    in
    case (indexArray# arr# n#) of
      (# v #) -> v


array		      :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
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
done (l,u) marr = \s1 -> 
   case unsafeFreezeArray# marr s1 of { (# s2, arr #) ->
   (# s2, Array l u arr #) }

arrEleBottom :: a
arrEleBottom = error "(Array.!): undefined array element"


-----------------------------------------------------------------------
-- These also go better with magic: (//), accum, accumArray
-- *** NB *** We INLINE them all so that their foldr's get to the call site

(//)		      :: (Ix a) => Array a b -> [(a,b)] -> Array a b
{-# INLINE (//) #-}
old_array // ivs
  = runST (do
	-- copy the old array:
	arr <- thawSTArray old_array
	-- now write the new elements into the new array:
	fill_it_in arr ivs
	freezeSTArray arr
    )

fill_it_in :: Ix ix => STArray s ix elt -> [(ix, elt)] -> ST s ()
{-# INLINE fill_it_in #-}
fill_it_in arr lst = foldr (fill_one_in arr) (return ()) lst
	 -- **** STRICT **** (but that's OK...)

fill_one_in arr (i, v) rst = writeSTArray arr i v >> rst

zap_with_f :: Ix ix => (elt -> elt2 -> elt) -> STArray s ix elt -> [(ix,elt2)] -> ST s ()
-- zap_with_f: reads an elem out first, then uses "f" on that and the new value
{-# INLINE zap_with_f #-}

zap_with_f f arr lst
  = foldr (zap_one f arr) (return ()) lst

zap_one f arr (i, new_v) rst = do
        old_v <- readSTArray arr i
	writeSTArray arr i (f old_v new_v)
	rst

accum		      :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
{-# INLINE accum #-}
accum f old_array ivs
  = runST (do
	-- copy the old array:
	arr <- thawSTArray old_array
	-- now zap the elements in question with "f":
	zap_with_f f arr ivs
	freezeSTArray arr
    )


accumArray	      :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
{-# INLINE accumArray #-}
accumArray f zero ixs ivs
  = runST (do
	arr <- newSTArray ixs zero
	zap_with_f f arr ivs
	freezeSTArray arr
    )
\end{code}


%*********************************************************
%*							*
\subsection{Array instances}
%*							*
%*********************************************************


\begin{code}
instance Ix a => Functor (Array a) where
  fmap = amap

instance  (Ix a, Eq b)  => Eq (Array a b)  where
    a == a'  	        =  assocs a == assocs a'
    a /= a'  	        =  assocs a /= assocs a'

instance  (Ix a, Ord b) => Ord (Array a b)  where
    compare a b = compare (assocs a) (assocs b)

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )
    showList = showList__ (showsPrec 0)

{-
instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ])
    readList = readList__ (readsPrec 0)
-}
\end{code}


%*********************************************************
%*							*
\subsection{Operations on mutable arrays}
%*							*
%*********************************************************

Idle ADR question: What's the tradeoff here between flattening these
datatypes into @STArray ix ix (MutableArray# s elt)@ and using
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
newSTArray :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)

{-# SPECIALIZE newSTArray :: IPr       -> elt -> ST s (STArray s Int elt),
			     (IPr,IPr) -> elt -> ST s (STArray s IPr elt)
  #-}
newSTArray (l,u) init = ST $ \ s# ->
    case rangeSize (l,u)          of { I# n# ->
    case (newArray# n# init s#)   of { (# s2#, arr# #) ->
    (# s2#, STArray l u arr# #) }}



boundsSTArray     :: Ix ix => STArray s ix elt -> (ix, ix)  
{-# SPECIALIZE boundsSTArray :: STArray s Int elt -> IPr #-}
boundsSTArray     (STArray     l u _) = (l,u)

readSTArray   	:: Ix ix => STArray s ix elt -> ix -> ST s elt 
{-# SPECIALIZE readSTArray :: STArray s Int elt -> Int -> ST s elt,
			      STArray s IPr elt -> IPr -> ST s elt
  #-}

readSTArray (STArray l u arr#) n = ST $ \ s# ->
    case (index (l,u) n)		of { I# n# ->
    case readArray# arr# n# s#		of { (# s2#, r #) ->
    (# s2#, r #) }}

writeSTArray  	 :: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
{-# SPECIALIZE writeSTArray :: STArray s Int elt -> Int -> elt -> ST s (),
			       STArray s IPr elt -> IPr -> elt -> ST s ()
  #-}

writeSTArray (STArray l u arr#) n ele = ST $ \ s# ->
    case index (l,u) n		    	    of { I# n# ->
    case writeArray# arr# n# ele s# 	    of { s2# ->
    (# s2#, () #) }}
\end{code}


%*********************************************************
%*							*
\subsection{Moving between mutable and immutable}
%*							*
%*********************************************************

\begin{code}
freezeSTArray	  :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
{-# SPECIALISE freezeSTArray :: STArray s Int elt -> ST s (Array Int elt),
			      STArray s IPr elt -> ST s (Array IPr elt)
  #-}

freezeSTArray (STArray l u arr#) = ST $ \ s# ->
    case rangeSize (l,u)     of { I# n# ->
    case freeze arr# n# s# of { (# s2#, frozen# #) ->
    (# s2#, Array l u frozen# #) }}
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

unsafeFreezeSTArray     :: Ix ix => STArray s ix elt -> ST s (Array ix elt)  
unsafeFreezeSTArray (STArray l u arr#) = ST $ \ s# ->
    case unsafeFreezeArray# arr# s# of { (# s2#, frozen# #) ->
    (# s2#, Array l u frozen# #) }

--This takes a immutable array, and copies it into a mutable array, in a
--hurry.

thawSTArray :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
{-# SPECIALISE thawSTArray :: Array Int elt -> ST s (STArray s Int elt),
			      Array IPr elt -> ST s (STArray s IPr elt)
  #-}

thawSTArray (Array l u arr#) = ST $ \ s# ->
    case rangeSize (l,u) of { I# n# ->
    case thaw arr# n# s# of { (# s2#, thawed# #) ->
    (# s2#, STArray l u thawed# #)}}
  where
    thaw  :: Array# ele			-- the thing
	    -> Int#			-- size of thing to be thawed
	    -> State# s			-- the Universe and everything
	    -> (# State# s, MutableArray# s ele #)

    thaw arr1# n# s#
      = case newArray# n# init s#	      of { (# s2#, newarr1# #) ->
	copy 0# n# arr1# newarr1# s2# }
      where
	init = error "thawSTArray: element not copied"

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
unsafeThawSTArray :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
unsafeThawSTArray (Array l u arr#) = ST $ \ s# ->
   case unsafeThawArray# arr# s# of
      (# s2#, marr# #) -> (# s2#, STArray l u marr# #)
\end{code}
