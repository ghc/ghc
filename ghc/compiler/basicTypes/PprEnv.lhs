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
	pTy, pTyVarB, pTyVarO, pUVar, pUse,
	
	NmbrEnv(..),
	SYN_IE(NmbrM), initNmbr,
	returnNmbr, thenNmbr,
	mapNmbr, mapAndUnzipNmbr
--	nmbr1, nmbr2, nmbr3
--	rnumValVar,   rnumTyVar,   rnumUVar,
--	lookupValVar, lookupTyVar, lookupUVar
    ) where

IMP_Ubiq(){-uitous-}

import Pretty		( Doc )
import Outputable
import Unique		( initRenumberingUniques, Unique )
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
