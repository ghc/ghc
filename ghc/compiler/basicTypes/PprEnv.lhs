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
	pTy, pTyVar, pUVar, pUse,
	
	NmbrEnv(..),
	NmbrM(..), initNmbr,
	returnNmbr, thenNmbr,
	mapNmbr, mapAndUnzipNmbr
--	nmbr1, nmbr2, nmbr3
--	rnumValVar,   rnumTyVar,   rnumUVar,
--	lookupValVar, lookupTyVar, lookupUVar
    ) where

import Ubiq{-uitous-}

import Pretty		( Pretty(..) )
import Unique		( initRenumberingUniques )
import UniqFM		( emptyUFM )
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
	(Id    -> Pretty)	-- carrying around a PprStyle
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
	-> Maybe (Id -> Pretty)
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

We tend to {\em renumber} everything before printing, so that
we get consistent Uniques on everything from run to run.
\begin{code}
data NmbrEnv
  = NmbrEnv	Unique	-- next "Unique" to give out for a value
		Unique	-- ... for a tyvar
		Unique	-- ... for a usage var
		(UniqFM Id)	-- mapping for value vars we know about
		(UniqFM TyVar)	-- ... for tyvars
		(UniqFM Unique{-UVar-})	-- ... for usage vars

type NmbrM a = NmbrEnv -> (NmbrEnv, a)

initNmbr :: NmbrM a -> a
initNmbr m
  = let
	(v1,t1,u1)    = initRenumberingUniques
	init_nmbr_env = NmbrEnv v1 t1 u1 emptyUFM emptyUFM emptyUFM
    in
    snd (m init_nmbr_env)

returnNmbr x nenv = (nenv, x)

thenNmbr m k nenv
  = let
	(nenv2, res) = m nenv
    in
    k res nenv2

mapNmbr f []     = returnNmbr []
mapNmbr f (x:xs)
  = f x		    `thenNmbr` \ r  ->
    mapNmbr f xs    `thenNmbr` \ rs ->
    returnNmbr (r:rs)

mapAndUnzipNmbr f [] = returnNmbr ([],[])
mapAndUnzipNmbr f (x:xs)
  = f x			    `thenNmbr` \ (r1,  r2)  ->
    mapAndUnzipNmbr f xs    `thenNmbr` \ (rs1, rs2) ->
    returnNmbr (r1:rs1, r2:rs2)

{-
nmbr1 nenv thing x1
  = let
	(nenv1, new_x1) = x1 nenv
    in
    (nenv1, thing new_x1)

nmbr2 nenv thing x1 x2
  = let
	(nenv1, new_x1) = x1 nenv
	(nenv2, new_x2) = x2 nenv1
    in
    (nenv2, thing new_x1 new_x2)

nmbr3 nenv thing x1 x2 x3
  = let
	(nenv1, new_x1) = x1 nenv
	(nenv2, new_x2) = x2 nenv1
	(nenv3, new_x3) = x3 nenv2
    in
    (nenv3, thing new_x1 new_x2 new_x3)
-}

rnumValVar = panic "rnumValVar"
rnumTyVar = panic "rnumTyVar"
rnumUVar = panic "rnumUVar"
lookupValVar = panic "lookupValVar"
lookupTyVar = panic "lookupTyVar"
lookupUVar = panic "lookupUVar"
\end{code}
