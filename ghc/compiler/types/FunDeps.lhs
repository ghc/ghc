It's better to read it as: "if we know these, then we're going to know these"

\begin{code}
module FunDeps(oclose, instantiateFundeps, instantiateFdTys, instantiateFdClassTys, pprFundeps) where

#include "HsVersions.h"

import Inst		(getDictClassTys)
import Class		(classTvsFds)
import Type		(getTyVar_maybe, tyVarsOfType)
import Outputable	(interppSP, ptext, empty, hsep, punctuate, comma)
import UniqSet		(elementOfUniqSet, addOneToUniqSet,
			 uniqSetToList, unionManyUniqSets)
import List		(elemIndex)
import Maybe		(catMaybes)
import FastString

oclose fds vs =
    case oclose1 fds vs of
      (vs', False) -> vs'
      (vs', True) -> oclose fds vs'

oclose1 [] vs = (vs, False)
oclose1 (fd@(ls, rs):fds) vs =
    if osubset ls vs then
	(vs'', b1 || b2)
    else
	vs'b1
    where
	vs'b1@(vs', b1) = oclose1 fds vs
	(vs'', b2) = ounion rs vs'

osubset [] vs = True
osubset (u:us) vs = if u `elementOfUniqSet` vs then osubset us vs else False

ounion [] ys = (ys, False)
ounion (x:xs) ys =
    if x `elementOfUniqSet` ys then (ys', b) else (addOneToUniqSet ys' x, True)
    where
	(ys', b) = ounion xs ys

-- instantiate fundeps to type variables
instantiateFundeps dict =
    map (\(xs, ys) -> (unionMap getTyVars xs, unionMap getTyVars ys)) fdtys
    where
	fdtys = instantiateFdTys dict
	getTyVars ty = tyVarsOfType ty
	unionMap f xs = uniqSetToList (unionManyUniqSets (map f xs))

-- instantiate fundeps to types
instantiateFdTys dict = instantiateFdClassTys clas ts
    where (clas, ts) = getDictClassTys dict
instantiateFdClassTys clas ts =
    map (lookupInstFundep tyvars ts) fundeps
    where
	(tyvars, fundeps) = classTvsFds clas
	lookupInstFundep tyvars ts (us, vs) =
	    (lookupInstTys tyvars ts us, lookupInstTys tyvars ts vs)
lookupInstTys tyvars ts = map (lookupInstTy tyvars ts)
lookupInstTy tyvars ts u = ts !! i
    where Just i = elemIndex u tyvars

pprFundeps [] = empty
pprFundeps fds = hsep (ptext SLIT("|") : punctuate comma (map ppr_fd fds))

ppr_fd (us, vs) = hsep [interppSP us, ptext SLIT("->"), interppSP vs]

\end{code}
