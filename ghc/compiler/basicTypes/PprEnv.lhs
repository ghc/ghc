%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprEnv]{The @PprEnv@ type}

\begin{code}
module PprEnv (
	PprEnv,		-- 
	BindingSite(..),

	initPprEnv,

	pCon, pBndr, pOcc, pSCC, 
	pTy, pTyVarO
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Const ( Con )

import Var		( GenId, GenTyVar )
import CostCentre	( CostCentre )
import Type  		( GenType )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

\begin{code}
data PprEnv bndr flexi
  = PE	{
	pCon :: Con        -> SDoc,
	pSCC :: CostCentre -> SDoc,

	pTyVarO :: GenTyVar flexi -> SDoc,	-- to print tyvar occurrences
	pTy     :: GenType flexi -> SDoc,	-- to print types

	pBndr :: BindingSite -> bndr -> SDoc,	-- to print value binders
	pOcc  :: GenId flexi -> SDoc		-- to print value occurrences
   }
\end{code}

@BindingSite@ is used to tell the thing that prints binder what
language construct is binding the identifier.

\begin{code}
data BindingSite = LambdaBind | CaseBind | LetBind
\end{code}

\begin{code}
initPprEnv
	:: Maybe (Con -> SDoc)
	-> Maybe (CostCentre -> SDoc)
	-> Maybe (GenTyVar flexi -> SDoc)
	-> Maybe (GenType flexi -> SDoc)
	-> Maybe (BindingSite -> bndr -> SDoc)
	-> Maybe (GenId flexi -> SDoc)
	-> PprEnv bndr flexi

-- you can specify all the printers individually; if
-- you don't specify one, you get bottom

initPprEnv p c tvo ty bndr occ
  = PE (demaybe p)
       (demaybe c)
       (demaybe tvo)
       (demaybe ty)
       (demaybe bndr)
       (demaybe occ)
  where
    demaybe Nothing  = bottom
    demaybe (Just x) = x

    bottom = panic "PprEnv.initPprEnv: unspecified printing function"
\end{code}

