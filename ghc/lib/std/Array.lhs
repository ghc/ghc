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

import Ix
import PrelList
import PrelArr		-- Most of the hard work is done here
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

{-# SPECIALISE listArray :: (Int,Int) -> [b] -> Array Int b #-}
listArray	      :: (Ix a) => (a,a) -> [b] -> Array a b
listArray b vs	      =  array b (zip (range b) vs)

{-# SPECIALISE indices :: Array Int b -> [Int] #-}
indices		      :: (Ix a) => Array a b -> [a]
indices		      =  range . bounds

{-# SPECIALISE elems :: Array Int b -> [b] #-}
elems		      :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]

{-# SPECIALISE assocs :: Array Int b -> [(Int,b)] #-}
assocs		      :: (Ix a) => Array a b -> [(a,b)]
assocs a              =  [(i, a!i) | i <- indices a]

{-# SPECIALISE amap :: (b -> c) -> Array Int b -> Array Int c #-}
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
