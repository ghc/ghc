%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelMaybe]{Module @PrelMaybe@}

The @Maybe@ type.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelMaybe where

import PrelBase

data  Maybe a  =  Nothing | Just a	deriving (Eq, Ord, Show {- Read -})

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return              = Just
    fail _		= Nothing

\end{code}




