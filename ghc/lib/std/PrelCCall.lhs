%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelCCall]{Module @PrelCCall@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelCCall (
	CCallable(..),
	CReturnable(..),

	Word(..),
	Word64(..),
	Int64(..)
   ) where

import PrelBase
import PrelGHC
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

data Word = W# Word# 	   deriving (Eq, Ord) -- Glasgow extension

instance CCallable Word
instance CCallable Word#
instance CReturnable Word


data Word64 = W64# Word64# --deriving (Eq, Ord) -- Glasgow extension
data Int64  = I64# Int64#  --deriving (Eq, Ord) -- Glasgow extension

instance CCallable   Word64
instance CCallable   Word64#
instance CReturnable Word64

instance CCallable   Int64
instance CCallable   Int64#
instance CReturnable Int64

instance CReturnable () -- Why, exactly?
\end{code}

