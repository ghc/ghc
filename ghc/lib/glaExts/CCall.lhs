%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[CCall]{Module @CCall@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module CCall (
	CCallable(..), CReturnable(..),
	Word(..)
   ) where

import PrelBase
import GHC
\end{code}

%*********************************************************
%*							*
\subsection{Classes @CCallable@ and @CReturnable@}
%*							*
%*********************************************************

\begin{code}
instance CCallable Char
instance CCallable   Char#
instance CReturnable Char

instance CCallable   Int
instance CCallable   Int#
instance CReturnable Int

-- DsCCall knows how to pass strings...
instance CCallable   [Char]

instance CCallable   Float
instance CCallable   Float#
instance CReturnable Float

instance CCallable   Double
instance CCallable   Double#
instance CReturnable Double

data Word = W# Word# 	deriving (Eq, Ord) -- Glasgow extension

instance CCallable Word
instance CCallable Word#
instance CReturnable Word

instance CReturnable () -- Why, exactly?
\end{code}

