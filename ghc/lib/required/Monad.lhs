%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[Monad]{Module @Monad@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Monad (
    Functor(..), 
    Monad(..), MonadZero(..), MonadPlus(..),

    -- Prelude monad functions
    accumulate, sequence, 
    mapM, mapM_, guard, filter, concat, applyM,

    -- Standard Monad interface:
    join,           -- :: (Monad m) => m (m a) -> m a
    mapAndUnzipM,   -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
    zipWithM,       -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
    foldM,          -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 
    when,           -- :: (Monad m) => Bool -> m () -> m ()
    unless,         -- :: (Monad m) => Bool -> m () -> m ()
    ap,             -- :: (Monad m) => (m (a -> b)) -> (m a) -> m b
    liftM, liftM2,  
    liftM3, liftM4, 
    liftM5
  ) where

import PrelList
import PrelTup
import PrelBase
\end{code}

%*********************************************************
%*							*
\subsection{Functions mandated by the Prelude}
%*							*
%*********************************************************

\begin{code}
accumulate      :: Monad m => [m a] -> m [a] 
accumulate []     = return []
accumulate (m:ms) = do { x <- m; xs <- accumulate ms; return (x:xs) }

sequence        :: Monad m => [m a] -> m () 
sequence        =  foldr (>>) (return ())

mapM            :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as       =  accumulate (map f as)

mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as      =  sequence (map f as)

guard           :: MonadZero m => Bool -> m ()
guard p         =  if p then return () else zero

-- This subsumes the list-based filter function.

filter          :: MonadZero m => (a -> Bool) -> m a -> m a
filter p        =  applyM (\x -> if p x then return x else zero)

-- This subsumes the list-based concat function.

concat          :: MonadPlus m => [m a] -> m a
concat          =  foldr (++) zero
 
applyM          :: Monad m => (a -> m b) -> m a -> m b
applyM f x      =  x >>= f
\end{code}


%*********************************************************
%*							*
\subsection{Other monad functions}
%*							*
%*********************************************************

\begin{code}
join             :: (Monad m) => m (m a) -> m a
join x           = x >>= id

mapAndUnzipM     :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs = accumulate (map f xs) >>= return . unzip

zipWithM         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = accumulate (zipWith f xs ys)

foldM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     = return a
foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs

unless 		 :: (Monad m) => Bool -> m () -> m ()
unless p s 	 =  if p then return () else s

when 		 :: (Monad m) => Bool -> m () -> m ()
when p s	 =  if p then s else return ()

ap :: (Monad m) => m (a->b) -> m a -> m b
ap = liftM2 ($)

liftM	:: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2	:: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3	:: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM4	:: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM5	:: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r

liftM f m1	        = do { x1 <- m1; return (f x1) }
liftM2 f m1 m2 		= do { x1 <- m1; x2 <- m2; return (f x1 x2) }
liftM3 f m1 m2 m3 	= do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
liftM4 f m1 m2 m3 m4	= do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

\end{code}
