% ------------------------------------------------------------------------------
% $Id: Maybe.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1992-2000
%

\section[GHC.Maybe]{Module @GHC.Maybe@}

The @Maybe@ type.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Maybe where

import GHC.Base
\end{code}

%*********************************************************
%*							*
\subsection{Maybe type}
%*							*
%*********************************************************

\begin{code}
data  Maybe a  =  Nothing | Just a	deriving (Eq, Ord)

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


%*********************************************************
%*							*
\subsection{Either type}
%*							*
%*********************************************************

\begin{code}
data  Either a b  =  Left a | Right b	deriving (Eq, Ord )

either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
\end{code}




