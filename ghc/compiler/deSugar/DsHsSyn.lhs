%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[DsHsSyn]{Haskell abstract syntax---added things for desugarer}

\begin{code}
module DsHsSyn where

#include "HsVersions.h"

import HsSyn		( OutPat(..), MonoBinds(..) )
import TcHsSyn		( TypecheckedPat,
			  TypecheckedMonoBinds )

import Id		( idType, Id )
import Type     	( Type )
\end{code}

