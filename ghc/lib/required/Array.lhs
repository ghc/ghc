%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Array]{Module @Array@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module  Array ( 
    module Ix,			-- export all of Ix 
    Array, 			-- Array type abstractly

    array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, amap, ixmap
  ) where

import Ix
import PrelList
import PrelRead
import ArrBase		-- Most of the hard work is done here
import PrelBase

infixl 9  !, //
\end{code}



%*********************************************************
%*							*
\subsection{Definitions of array, !, bounds}
%*							*
%*********************************************************

\begin{code}

#ifdef USE_FOLDR_BUILD
{-# INLINE indices #-}
{-# INLINE elems #-}
{-# INLINE assocs #-}
#endif

{-# GENERATE_SPECS listArray a{~,Int,IPr} b{} #-}
listArray	      :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs	      =  array b (zipWith (\ a b -> (a,b)) (range b) vs)

{-# GENERATE_SPECS indices a{~,Int,IPr} b{} #-}
indices		      :: (Ix a) => Array a b -> [a]
indices		      =  range . bounds

{-# GENERATE_SPECS elems a{~,Int,IPr} b{} #-}
elems		      :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

{-# GENERATE_SPECS assocs a{~,Int,IPr} b{} #-}
assocs		      :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

{-# GENERATE_SPECS amap a{~,Int,IPr} b{} c{} #-}
amap		      :: (Ix a) => (b -> c) -> Array a b -> Array a c
amap f a              =  array b [(i, f (a!i)) | i <- range b]
                         where b = bounds a

ixmap		      :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c
ixmap b f a           =  array b [(i, a ! f i) | i <- range b]
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations for Array type}
%*							*
%*********************************************************

\begin{code}
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

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	   (\r -> [(array b as, u) | ("array",s) <- lex r,
				     (b,t)       <- reads s,
				     (as,u)      <- reads t   ])
    readList = readList__ (readsPrec 0)
\end{code}
