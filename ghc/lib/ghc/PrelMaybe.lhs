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
maybe n f Nothing  = n
maybe n f (Just x) = f x

instance  Functor Maybe  where
    map f Nothing       = Nothing
    map f (Just a)      = Just (f a)

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= k      = Nothing

    (Just x) >>  k      = k
    Nothing  >>  k      = Nothing

    return              = Just

instance  MonadZero Maybe  where
    zero                = Nothing

instance  MonadPlus Maybe  where
    Nothing ++ ys       = ys
    xs      ++ ys       = xs
\end{code}




