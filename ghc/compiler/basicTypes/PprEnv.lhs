%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprEnv]{The @PprEnv@ type}

\begin{code}
module PprEnv (
	PprEnv{-abstract-}, 
	BindingSite(..),

	initPprEnv,

	pCon, pLit, pValBndr, pOcc, pPrim, pSCC, 
	pTy, pTyVarB, pTyVarO
	
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Id ( Id )
import {-# SOURCE #-} PrimOp ( PrimOp )
import {-# SOURCE #-} CostCentre ( CostCentre )

import Type  		( GenType )
import TyVar 		( GenTyVar   )
import Literal          ( Literal )
import Outputable
import Unique		( Unique )
import UniqFM		( emptyUFM, UniqFM )
\end{code}

%************************************************************************
%*									*
\subsection{Public interfaces for Core printing (excluding instances)}
%*									*
%************************************************************************

\begin{code}
data PprEnv flexi bndr occ
  = PE	(Literal    -> SDoc)
	(Id	    -> SDoc)
	(PrimOp     -> SDoc)
	(CostCentre -> SDoc)

	(GenTyVar flexi -> SDoc)	-- to print tyvar binders
	(GenTyVar flexi -> SDoc)	-- to print tyvar occurrences
	(GenType flexi -> SDoc)		-- to print types

	(BindingSite -> bndr -> SDoc)	-- to print val_bdrs
	(occ 		     -> SDoc)	-- to print bindees

\end{code}

@BindingSite@ is used to tell the thing that prints binder what
language construct is binding the identifier.

\begin{code}
data BindingSite = LambdaBind | CaseBind | LetBind
\end{code}

\begin{code}
initPprEnv
	:: Maybe (Literal -> SDoc)
	-> Maybe (Id -> SDoc)
	-> Maybe (PrimOp  -> SDoc)
	-> Maybe (CostCentre -> SDoc)
	-> Maybe (GenTyVar flexi -> SDoc)
	-> Maybe (GenTyVar flexi -> SDoc)
	-> Maybe (GenType flexi -> SDoc)
	-> Maybe (BindingSite -> bndr -> SDoc)
	-> Maybe (occ -> SDoc)
	-> PprEnv flexi bndr occ

-- you can specify all the printers individually; if
-- you don't specify one, you get bottom

initPprEnv l d p c tvb tvo ty val_bndr occ
  = PE (demaybe l)
       (demaybe d)
       (demaybe p)
       (demaybe c)
       (demaybe tvb)
       (demaybe tvo)
       (demaybe ty)
       (demaybe val_bndr)
       (demaybe occ)
  where
    demaybe Nothing  = bottom
    demaybe (Just x) = x

    bottom = panic "PprEnv.initPprEnv: unspecified printing function"
\end{code}

\begin{code}
pLit	 (PE pp  _  _  _  _  _   _  _  _) = pp
pCon	 (PE  _ pp  _  _  _  _   _  _  _) = pp
pPrim	 (PE  _  _ pp  _  _  _   _  _  _) = pp
pSCC	 (PE  _  _  _ pp  _  _   _  _  _) = pp
	     			    
pTyVarB	 (PE  _  _  _  _  pp _   _  _  _) = pp
pTyVarO	 (PE  _  _  _  _  _  pp  _  _  _) = pp
pTy      (PE  _  _  _  _  _  _   pp _  _) = pp
      	     			    
pValBndr (PE  _  _  _  _  _  _   _ pp  _) = pp
pOcc     (PE  _  _  _  _  _  _   _ _  pp) = pp
\end{code}
