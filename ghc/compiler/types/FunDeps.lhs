%
% (c) The GRASP/AQUA Project, Glasgow University, 2000
%
\section[FunDeps]{FunDeps - functional dependencies}

It's better to read it as: "if we know these, then we're going to know these"

\begin{code}
module FunDeps (
	oclose,
        instantiateFdClassTys,
        tyVarFunDep,
        pprFundeps
    ) where

#include "HsVersions.h"

import Var		( TyVar )
import Class		( Class, FunDep, classTvsFds )
import Type		( Type, tyVarsOfTypes )
import Outputable	( Outputable, SDoc, interppSP, ptext, empty, hsep, punctuate, comma )
import UniqSet
import VarSet
import VarEnv
import Unique		( Uniquable )
import List		( elemIndex )
import Util		( zipEqual )
\end{code}


\begin{code}
oclose :: Uniquable a => [FunDep a] -> UniqSet a -> UniqSet a
-- (oclose fds tvs) closes the set of type variables tvs, 
-- wrt the functional dependencies fds.  The result is a superset
-- of the argument set.
--
-- For example,
--	oclose [a -> b] {a}     = {a,b}
--	oclose [a b -> c] {a}   = {a}
--	oclose [a b -> c] {a,b} = {a,b,c}
-- If all of the things on the left of an arrow are in the set, add
-- the things on the right of that arrow.

oclose fds vs =
    case oclose1 fds vs of
      (vs', False) -> vs'
      (vs', True)  -> oclose fds vs'

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
ounion (x:xs) ys
    | x `elementOfUniqSet` ys = (ys', b)
    | otherwise		      = (addOneToUniqSet ys' x, True)
    where
	(ys', b) = ounion xs ys

instantiateFdClassTys :: Class -> [Type] -> [FunDep Type]
-- Get the FDs of the class, and instantiate them
instantiateFdClassTys clas tys
  = [(map lookup us, map lookup vs) | (us,vs) <- fundeps]
  where
    (tyvars, fundeps) = classTvsFds clas
    env       = mkVarEnv (zipEqual "instantiateFdClassTys" tyvars tys)
    lookup tv = lookupVarEnv_NF env tv

tyVarFunDep :: [FunDep Type] -> [FunDep TyVar]
tyVarFunDep fdtys 
  = [(getTyvars xs, getTyvars ys) | (xs, ys) <- fdtys]
  where 
    getTyvars = varSetElems . tyVarsOfTypes

pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps [] = empty
pprFundeps fds = hsep (ptext SLIT("|") : punctuate comma (map ppr_fd fds))

ppr_fd (us, vs) = hsep [interppSP us, ptext SLIT("->"), interppSP vs]
\end{code}
