%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelCCall]{Module @PrelCCall@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelCCall (
	CCallable(..),
	CReturnable(..)
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

instance CReturnable () -- Why, exactly?
\end{code}

