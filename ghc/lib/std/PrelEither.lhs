%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
%
\section[PrelEither]{Module @PrelEither@}

The @Either@ Type.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelEither where

import PrelBase

data  Either a b  =  Left a | Right b	deriving (Eq, Ord, Show {- Read -} )

either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
\end{code}
