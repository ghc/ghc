%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprEnv]{The @PprEnv@ type}

\begin{code}
module PprEnv (
	PprEnv,		-- 
	BindingSite(..),

	initPprEnv,

	pBndr, pOcc, pSCC, 
	pTy, pTyVarO
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} DataCon ( DataCon )

import Var		( Id, TyVar )
import CostCentre	( CostCentre )
import Type  		( Type )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

\begin{code}
data PprEnv bndr
  = PE	{
	pSCC :: CostCentre -> SDoc,

	pTyVarO :: TyVar -> SDoc,	-- to print tyvar occurrences
	pTy     :: Type -> SDoc,	-- to print types

	pBndr :: BindingSite -> bndr -> SDoc,	-- to print value binders
	pOcc  :: Id -> SDoc		-- to print value occurrences
   }
\end{code}

@BindingSite@ is used to tell the thing that prints binder what
language construct is binding the identifier.

\begin{code}
data BindingSite = LambdaBind | CaseBind | LetBind
\end{code}

\begin{code}
initPprEnv
	:: Maybe (CostCentre -> SDoc)
	-> Maybe (TyVar -> SDoc)
	-> Maybe (Type -> SDoc)
	-> Maybe (BindingSite -> bndr -> SDoc)
	-> Maybe (Id -> SDoc)
	-> PprEnv bndr

-- you can specify all the printers individually; if
-- you don't specify one, you get bottom

initPprEnv c tvo ty bndr occ
  = PE (demaybe c)
       (demaybe tvo)
       (demaybe ty)
       (demaybe bndr)
       (demaybe occ)
  where
    demaybe Nothing  = bottom
    demaybe (Just x) = x

    bottom = panic "PprEnv.initPprEnv: unspecified printing function"
\end{code}

