%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Parallel]{Parallel Constructs}

\begin{code}
module Parallel (par, seq -- re-exported
#if defined(__GRANSIM__)
	, parGlobal, parLocal, parAt, parAtForNow     
#endif
    ) where

import ConcBase	( par )

#if defined(__GRANSIM__)

{-# INLINE parGlobal #-}
parGlobal   :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal    :: Int -> Int -> Int -> Int -> a -> b -> b
parAt	    :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c

parGlobal (I# w) (I# g) (I# s) (I# p) x y = case (parGlobal# x w g s p y) of { 0# -> parError; _ -> y }
parLocal  (I# w) (I# g) (I# s) (I# p) x y = case (parLocal#  x w g s p y) of { 0# -> parError; _ -> y }

parAt       (I# w) (I# g) (I# s) (I# p) v x y = case (parAt#       x v w g s p y) of { 0# -> parError; _ -> y }
parAtForNow (I# w) (I# g) (I# s) (I# p) v x y = case (parAtForNow# x v w g s p y) of { 0# -> parError; _ -> y }

#endif

-- Maybe parIO and the like could be added here later.
\end{code}
