%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[LIE]{Id instance environment}

This is not really an ``environment.''

\begin{code}
#include "HsVersions.h"

module LIE (
	LIE,		-- abstract type
	mkLIE, nullLIE, unitLIE, unMkLIE, plusLIE,

	-- imported things so this module's interface is self-contained
	Inst
    ) where

import Inst		( Inst )
import Outputable
import Util
\end{code}

%************************************************************************
%*									*
\subsection[LIE-building]{Building LIEs}
%*									*
%************************************************************************

\begin{code}
data LIE = MkLIE [Inst]

mkLIE = MkLIE

nullLIE   = MkLIE []
unitLIE x = MkLIE [x]

unMkLIE :: LIE -> [Inst]
unMkLIE (MkLIE insts) = insts

plusLIE :: LIE -> LIE -> LIE
plusLIE (MkLIE lie1) (MkLIE lie2)
  = MkLIE (lie1 ++ lie2)
\end{code}
