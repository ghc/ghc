%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Addr]{Module @Addr@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Addr (
	Addr(..), -- ToDo: nullAddr,
   ) where

import GHC
import PrelBase
import STBase
import CCall
\end{code}

\begin{code}
data Addr = A# Addr# 	deriving (Eq, Ord) -- Glasgow extension

nullAddr = ``NULL'' :: Addr

instance CCallable Addr
instance CCallable Addr#
instance CReturnable Addr
\end{code}



