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
	pTy, pTyVar, pUVar, pUse
    ) where

import Ubiq{-uitous-}

import Id		( DataCon(..) )
import Pretty		( Pretty(..) )
import Util		( panic )
\end{code}

For tyvars and uvars, we {\em do} normally use these homogenized
names; for values, we {\em don't}.  In printing interfaces, though,
we use homogenized value names, so that interfaces don't wobble
uncontrollably from changing Unique-based names.

\begin{code}
data PprEnv tyvar uvar bndr occ
  = PE	PprStyle		-- stored for safe keeping

	(Literal    -> Pretty)	-- Doing these this way saves
	(DataCon    -> Pretty)	-- carrying around a PprStyle
	(PrimOp     -> Pretty)
	(CostCentre -> Pretty)

	(tyvar -> Pretty)	-- to print tyvars
	(uvar -> Pretty)	-- to print usage vars

	(bndr -> Pretty)	-- to print "major" val_bdrs
	(bndr -> Pretty)	-- to print "minor" val_bdrs
	(occ  -> Pretty)	-- to print bindees

	(GenType tyvar uvar -> Pretty)
	(GenUsage uvar -> Pretty)
\end{code}

\begin{code}
initPprEnv
	:: PprStyle
	-> Maybe (Literal -> Pretty)
	-> Maybe (DataCon -> Pretty)
	-> Maybe (PrimOp  -> Pretty)
	-> Maybe (CostCentre -> Pretty)
	-> Maybe (tyvar -> Pretty)
	-> Maybe (uvar -> Pretty)
	-> Maybe (bndr -> Pretty)
	-> Maybe (bndr -> Pretty)
	-> Maybe (occ -> Pretty)
	-> Maybe (GenType tyvar uvar -> Pretty)
	-> Maybe (GenUsage uvar -> Pretty)
	-> PprEnv tyvar uvar bndr occ

-- you can specify all the printers individually; if
-- you don't specify one, you get bottom

initPprEnv sty l d p c tv uv maj_bndr min_bndr occ ty use
  = PE sty
       (demaybe l)
       (demaybe d)
       (demaybe p)
       (demaybe c)
       (demaybe tv)
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
	(\ cc -> ppStr (showCostCentre sty True cc)) -- CostCentre

	(ppr sty)   -- for a tyvar
	(ppr sty)   -- for a usage var

	pmaj pmin pocc -- for GenIds in various guises

	(ppr sty)   -- for a Type
	(ppr sty)   -- for a Usage
-}
\end{code}

\begin{code}
pStyle	 (PE s  _  _  _  _  _  _  _  _  _  _  _) = s
pLit	 (PE _ pp  _  _  _  _  _  _  _  _  _  _) = pp
pCon	 (PE _	_ pp  _  _  _  _  _  _  _  _  _) = pp
pPrim	 (PE _	_  _ pp  _  _  _  _  _  _  _  _) = pp
pSCC	 (PE _	_  _  _ pp  _  _  _  _  _  _  _) = pp
	     				       
pTyVar	 (PE _	_  _  _  _ pp  _  _  _  _  _  _) = pp
pUVar	 (PE _	_  _  _  _  _ pp  _  _  _  _  _) = pp
      	     				       
pMajBndr (PE _	_  _  _  _  _  _ pp  _  _  _  _) = pp
pMinBndr (PE _	_  _  _  _  _  _  _ pp  _  _  _) = pp
pOcc     (PE _	_  _  _  _  _  _  _  _ pp  _  _) = pp
	     		       
pTy      (PE _	_  _  _  _  _  _  _  _  _ pp  _) = pp
pUse	 (PE _	_  _  _  _  _  _  _  _  _  _ pp) = pp
\end{code}
