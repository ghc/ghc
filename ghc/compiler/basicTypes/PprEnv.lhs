%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprEnv]{The @PprEnv@ type}

\begin{code}
#include "HsVersions.h"

module PprEnv (
	PprEnv{-abstract-},

	initPprEnv,

	pCon, pLit, pMajBndr, pMinBndr, pOcc, pPrim, pSCC, pStyle,
	pTy, pTyVarB, pTyVarO, pUVar, pUse
	
    ) where

IMP_Ubiq(){-uitous-}

import Pretty		( Doc )
import Outputable
import Unique		( Unique )
import UniqFM		( emptyUFM, UniqFM )
import Util		( panic )
#if __GLASGOW_HASKELL__ >= 202
import {-# SOURCE #-}   Type  ( GenType )
import {-# SOURCE #-}   TyVar ( TyVar   )
import {-# SOURCE #-}   Id ( Id )
import Outputable       ( PprStyle )
import Literal          ( Literal )
import Usage            ( GenUsage, SYN_IE(Usage) )
import {-# SOURCE #-}   PrimOp (PrimOp)
import {-# SOURCE #-}   CostCentre ( CostCentre )
#endif

\end{code}

For tyvars and uvars, we {\em do} normally use these homogenized
names; for values, we {\em don't}.  In printing interfaces, though,
we use homogenized value names, so that interfaces don't wobble
uncontrollably from changing Unique-based names.

\begin{code}
data PprEnv tyvar uvar bndr occ
  = PE	PprStyle		-- stored for safe keeping

	(Literal    -> Doc)	-- Doing these this way saves
	(Id    -> Doc)	-- carrying around a PprStyle
	(PrimOp     -> Doc)
	(CostCentre -> Doc)

	(tyvar -> Doc)	-- to print tyvar binders
	(tyvar -> Doc)	-- to print tyvar occurrences

	(uvar -> Doc)	-- to print usage vars

	(bndr -> Doc)	-- to print "major" val_bdrs
	(bndr -> Doc)	-- to print "minor" val_bdrs
	(occ  -> Doc)	-- to print bindees

	(GenType tyvar uvar -> Doc)
	(GenUsage uvar -> Doc)
\end{code}

\begin{code}
initPprEnv
	:: PprStyle
	-> Maybe (Literal -> Doc)
	-> Maybe (Id -> Doc)
	-> Maybe (PrimOp  -> Doc)
	-> Maybe (CostCentre -> Doc)
	-> Maybe (tyvar -> Doc)
	-> Maybe (tyvar -> Doc)
	-> Maybe (uvar -> Doc)
	-> Maybe (bndr -> Doc)
	-> Maybe (bndr -> Doc)
	-> Maybe (occ -> Doc)
	-> Maybe (GenType tyvar uvar -> Doc)
	-> Maybe (GenUsage uvar -> Doc)
	-> PprEnv tyvar uvar bndr occ

-- you can specify all the printers individually; if
-- you don't specify one, you get bottom

initPprEnv sty l d p c tvb tvo uv maj_bndr min_bndr occ ty use
  = PE sty
       (demaybe l)
       (demaybe d)
       (demaybe p)
       (demaybe c)
       (demaybe tvb)
       (demaybe tvo)
       (demaybe uv)
       (demaybe maj_bndr)
       (demaybe min_bndr)
       (demaybe occ)
       (demaybe ty)
       (demaybe use)
  where
    demaybe Nothing  = bottom
    demaybe (Just x) = x

    bottom = panic "PprEnv.initPprEnv: unspecified printing function"

{-
initPprEnv sty pmaj pmin pocc
  = PE	(ppr sty)   -- for a Literal
	(ppr sty)   -- for a DataCon
	(ppr sty)   -- for a PrimOp
	(\ cc -> text (showCostCentre sty True cc)) -- CostCentre

	(ppr sty)   -- for a tyvar
	(ppr sty)   -- for a usage var

	pmaj pmin pocc -- for GenIds in various guises

	(ppr sty)   -- for a Type
	(ppr sty)   -- for a Usage
-}
\end{code}

\begin{code}
pStyle	 (PE s  _  _  _  _  _  _  _  _  _  _  _  _) = s
pLit	 (PE _ pp  _  _  _  _  _  _  _  _  _  _  _) = pp
pCon	 (PE _	_ pp  _  _  _  _  _  _  _  _  _  _) = pp
pPrim	 (PE _	_  _ pp  _  _  _  _  _  _  _  _  _) = pp
pSCC	 (PE _	_  _  _ pp  _  _  _  _  _  _  _  _) = pp
	     					 
pTyVarB	 (PE _	_  _  _  _  pp _  _  _  _  _  _  _) = pp
pTyVarO	 (PE _	_  _  _  _  _  pp _  _  _  _  _  _) = pp
pUVar	 (PE _	_  _  _  _  _  _  pp _  _  _  _  _) = pp
      	     					 
pMajBndr (PE _	_  _  _  _  _  _  _ pp  _  _  _  _) = pp
pMinBndr (PE _	_  _  _  _  _  _  _  _ pp  _  _  _) = pp
pOcc     (PE _	_  _  _  _  _  _  _  _  _ pp  _  _) = pp
	     		       	 
pTy      (PE _	_  _  _  _  _  _  _  _  _  _ pp  _) = pp
pUse	 (PE _	_  _  _  _  _  _  _  _  _  _  _ pp) = pp
\end{code}
