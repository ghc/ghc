%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Usage]{The @Usage@ datatype}

\begin{code}
#include "HsVersions.h"

module Usage (
	GenUsage(..), Usage(..), UVar(..), UVarEnv(..),
	usageOmega, pprUVar, duffUsage,
	nullUVarEnv, mkUVarEnv, addOneToUVarEnv,
	growUVarEnvList, isNullUVarEnv, lookupUVarEnv,
	eqUVar, eqUsage
) where

IMP_Ubiq(){-uitous-}

import Pretty	( Pretty(..), PrettyRep, ppPStr, ppBeside )
import UniqFM	( emptyUFM, listToUFM, addToUFM, lookupUFM,
		  plusUFM, sizeUFM, UniqFM
		)
import Unique	( Unique{-instances-} )
import Util	( panic )
\end{code}

\begin{code}
data GenUsage uvar
  = UsageVar uvar
  | UsageOne
  | UsageOmega

type UVar  = Unique
type Usage = GenUsage UVar

usageOmega = UsageOmega

duffUsage :: GenUsage uvar
duffUsage = panic "Usage of non-Type kind doesn't make sense"
\end{code}

%************************************************************************
%*									*
\subsection{Environments}
%*									*
%************************************************************************

\begin{code}
type UVarEnv a = UniqFM a

nullUVarEnv	:: UVarEnv a
mkUVarEnv	:: [(UVar, a)] -> UVarEnv a
addOneToUVarEnv :: UVarEnv a -> UVar -> a -> UVarEnv a
growUVarEnvList :: UVarEnv a -> [(UVar, a)] -> UVarEnv a
isNullUVarEnv   :: UVarEnv a -> Bool
lookupUVarEnv   :: UVarEnv a -> UVar -> Maybe a

nullUVarEnv	= emptyUFM
mkUVarEnv	= listToUFM
addOneToUVarEnv = addToUFM
lookupUVarEnv   = lookupUFM

growUVarEnvList env pairs = plusUFM env (listToUFM pairs)
isNullUVarEnv   env       = sizeUFM env == 0
\end{code}

%************************************************************************
%*									*
\subsection{Equality on usages}
%*									*
%************************************************************************

Equaltity (with respect to an environment mapping usage variables
to equivalent usage variables).

\begin{code}
eqUVar :: UVarEnv UVar -> UVar -> UVar -> Bool
eqUVar uve u1 u2 =
  u1 == u2 ||
  case lookupUVarEnv uve u1 of
    Just u -> u == u2
    Nothing -> False

eqUsage :: UVarEnv UVar -> Usage -> Usage -> Bool
eqUsage uve (UsageVar u1) (UsageVar u2) = eqUVar uve u1 u2
eqUsage uve UsageOne      UsageOne   = True
eqUsage uve UsageOmega    UsageOmega = True
eqUsage _ _ _ = False
\end{code}

%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
instance Eq u => Eq (GenUsage u) where
  (UsageVar u1) == (UsageVar u2) = u1 == u2
  UsageOne      == UsageOne	 = True
  UsageOmega    == UsageOmega	 = True
  _		== _		 = False
\end{code}

\begin{code}
instance Outputable uvar => Outputable (GenUsage uvar) where
    ppr sty UsageOne	 = ppPStr SLIT("UsageOne")
    ppr sty UsageOmega	 = ppPStr SLIT("UsageOmega")
    ppr sty (UsageVar u) = pprUVar sty u

pprUVar sty u = ppBeside (ppPStr SLIT("u")) (ppr sty u)
\end{code}
