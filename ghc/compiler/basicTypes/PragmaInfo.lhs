%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PragmaInfo]{@PragmaInfos@: The user's pragma requests}

\begin{code}
module PragmaInfo where

#include "HsVersions.h"

\end{code}

\begin{code}
data PragmaInfo
  = NoPragmaInfo

  | IWantToBeINLINEd

  | IMustNotBeINLINEd	-- Used by the simplifier to prevent looping
			-- on recursive definitions

  | IMustBeINLINEd	-- Absolutely must inline; used for PrimOps only
\end{code}
