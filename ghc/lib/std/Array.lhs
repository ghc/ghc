%
% (c) The AQUA Project, Glasgow University, 1994-1999
%

\section[Array]{Module @Array@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module  Array 

    ( 
      module Ix			-- export all of Ix 
    , Array 			-- Array type is abstract

    , array	    -- :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
    , listArray     -- :: (Ix a) => (a,a) -> [b] -> Array a b
    , (!)           -- :: (Ix a) => Array a b -> a -> b
    , bounds        -- :: (Ix a) => Array a b -> (a,a)
    , indices       -- :: (Ix a) => Array a b -> [a]
    , elems         -- :: (Ix a) => Array a b -> [b]
    , assocs        -- :: (Ix a) => Array a b -> [(a,b)]
    , accumArray    -- :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
    , (//)          -- :: (Ix a) => Array a b -> [(a,b)] -> Array a b
    , accum         -- :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
    , ixmap         -- :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a b

    -- Array instances:
    --
    --   Ix a => Functor (Array a)
    --   (Ix a, Eq b)  => Eq   (Array a b)
    --   (Ix a, Ord b) => Ord  (Array a b)
    --   (Ix a, Show a, Show b) => Show (Array a b)
    --   (Ix a, Read a, Read b) => Read (Array a b)
    -- 

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.

    ) where

#ifndef __HUGS__
import Ix
import PrelList
import PrelShow
import PrelArr		-- Most of the hard work is done here
import PrelBase
#else
import PrimPrel ( PrimArray
		, runST
		, primNewArray
	        , primWriteArray
		, primReadArray
		, primUnsafeFreezeArray
		, primIndexArray
		)
import Ix
import List( (\\) )
#endif

infixl 9  !, //
\end{code}

#ifndef __HUGS__


%*********************************************************
%*							*
\subsection{Definitions of array, !, bounds}
%*							*
%*********************************************************

\begin{code}


{-# SPECIALISE listArray :: (Int,Int) -> [b] -> Array Int b #-}
listArray	      :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs	      =  array b (zip (range b) vs)

{-# INLINE elems #-}
elems		      :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

ixmap		      :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a           =  array b [(i, a ! f i) | i <- range b]
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations for Array type}
%*							*
%*********************************************************


#else
\begin{code}
data Array ix elt = Array (ix,ix) (PrimArray elt)

array :: Ix a => (a,a) -> [(a,b)] -> Array a b
array ixs@(ix_start, ix_end) ivs = runST (do
  { mut_arr <- primNewArray (rangeSize ixs) arrEleBottom
  ; mapM_ (\ (i,v) -> primWriteArray mut_arr (index ixs i) v) ivs 
  ; arr <- primUnsafeFreezeArray mut_arr
  ; return (Array ixs arr)
  }
  )
 where
  arrEleBottom = error "(Array.!): undefined array element"

listArray               :: Ix a => (a,a) -> [b] -> Array a b
listArray b vs          =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

(!)	                :: Ix a => Array a b -> a -> b
(Array bounds arr) ! i  = primIndexArray arr (index bounds i)

bounds                  :: Ix a => Array a b -> (a,a)
bounds (Array b _)      =  b

indices           :: Ix a => Array a b -> [a]
indices	          = range . bounds

elems             :: Ix a => Array a b -> [b]
elems a           =  [a!i | i <- indices a]

assocs	          :: Ix a => Array a b -> [(a,b)]
assocs a          =  [(i, a!i) | i <- indices a]

(//)              :: Ix a => Array a b -> [(a,b)] -> Array a b
a // us           =  array (bounds a)
                        ([(i,a!i) | i <- indices a \\ [i | (i,_) <- us]]
                         ++ us)

accum             :: Ix a => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
accum f           =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])

accumArray        :: Ix a => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
accumArray f z b  =  accum f (array b [(i,z) | i <- range b])

ixmap	          :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a       =  array b [(i, a ! f i) | i <- range b]


instance (Ix a) => Functor (Array a) where
    fmap f a = array (bounds a) [(i, f(a!i)) | i <- indices a]

instance (Ix a, Eq b) => Eq (Array a b) where
    a == a'   =   assocs a == assocs a'

instance (Ix a, Ord b) => Ord (Array a b) where
    a <= a'   =   assocs a <= assocs a'


instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	     (\r -> [(array b as, u) | ("array",s) <- lex r,
				       (b,t)       <- reads s,
				       (as,u)      <- reads t   ])

\end{code}
#endif
