%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PragmaInfo]{@PragmaInfos@: The user's pragma requests}

\begin{code}
#include "HsVersions.h"

module PragmaInfo where

import Ubiq
\end{code}

\begin{code}
data PragmaInfo
  = NoPragmaInfo
  | IWantToBeINLINEd
\end{code}
