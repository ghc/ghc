\begin{code}
module TcImprove ( tcImprove ) where

#include "HsVersions.h"

import Name		( Name )
import Class		( Class, FunDep, className, classInstEnv, classExtraBigSig )
import Unify		( unifyTyListsX, matchTys )
import Subst		( mkSubst, substTy )
import TcMonad
import TcType		( TcType, TcTyVar, TcTyVarSet, zonkTcType, zonkTcTypes )
import TcUnify		( unifyTauTyLists )
import Inst		( LIE, Inst, LookupInstResult(..),
			  lookupInst, getFunDepsOfLIE, getIPsOfLIE,
			  zonkLIE, zonkFunDeps {- for debugging -} )
import InstEnv		( InstEnv )		-- Reqd for 4.02; InstEnv is a synonym, and
						-- 4.02 doesn't "see" it soon enough
import VarSet		( VarSet, emptyVarSet, unionVarSet )
import VarEnv		( emptyVarEnv )
import FunDeps		( instantiateFdClassTys )
import Outputable
import List		( elemIndex, nub )
\end{code}

\begin{code}
tcImprove :: LIE -> TcM s ()
-- Do unifications based on functional dependencies in the LIE
tcImprove lie 
  | null nfdss = returnTc ()
  | otherwise  = iterImprove nfdss
  where
	nfdss, clas_nfdss, inst_nfdss, ip_nfdss :: [(TcTyVarSet, Name, [FunDep TcType])]
	nfdss = ip_nfdss ++ clas_nfdss ++ inst_nfdss

	cfdss :: [(Class, [FunDep TcType])]
	cfdss = getFunDepsOfLIE lie
	clas_nfdss = map (\(c, fds) -> (emptyVarSet, className c, fds)) cfdss

	classes = nub (map fst cfdss)
	inst_nfdss = concatMap getInstNfdssOf classes

	ips = getIPsOfLIE lie
	ip_nfdss = map (\(n, ty) -> (emptyVarSet, n, [([], [ty])])) ips

{- Example: we have
	class C a b c  |  a->b where ...
	instance C Int Bool c 

   Given the LIE 	FD C (Int->t)
   we get	clas_nfdss = [({}, C, [Int->t,     t->Int])
		inst_nfdss = [({c}, C, [Int->Bool, Bool->Int])]

   Another way would be to flatten a bit
   we get	clas_nfdss = [({}, C, Int->t), ({}, C, t->Int)]
		inst_nfdss = [({c}, C, Int->Bool), ({c}, C, Bool->Int)]

   iterImprove then matches up the C and Int, and unifies t <-> Bool
-}

getInstNfdssOf :: Class -> [(TcTyVarSet, Name, [FunDep TcType])]
getInstNfdssOf clas 
  = [ (free, nm, instantiateFdClassTys clas ts)
    | (free, ts, i) <- classInstEnv clas
    ]
  where
	nm = className clas

iterImprove :: [(VarSet, Name, [FunDep TcType])] -> TcM s ()
iterImprove [] = returnTc ()
iterImprove cfdss
  = selfImprove pairImprove cfdss	`thenTc` \ change2 ->
    if {- change1 || -} change2 then
	iterImprove cfdss
    else
	returnTc ()

-- ZZ this will do a lot of redundant checking wrt instances
-- it would do to make this operate over two lists, the first
-- with only clas_nfds and ip_nfds, and the second with everything
-- control would otherwise mimic the current loop, so that the
-- caller could control whether the redundant inst improvements
-- were avoided
-- you could then also use this to check for consistency of new instances

-- selfImprove is really just doing a cartesian product of all the fds
selfImprove f [] = returnTc False
selfImprove f (nfds : nfdss)
  = mapTc (f nfds) nfdss	`thenTc` \ changes ->
    selfImprove f nfdss		`thenTc` \ rest_changed ->
    returnTc (or changes || rest_changed)

pairImprove (free1, n1, fds1) (free2, n2, fds2)
  = if n1 == n2 then
	checkFds (free1 `unionVarSet` free2) fds1 fds2
    else
	returnTc False

checkFds free [] [] = returnTc False
checkFds free (fd1 : fd1s) (fd2 : fd2s) =
    checkFd free fd1 fd2	`thenTc` \ change ->
    checkFds free fd1s fd2s	`thenTc` \ changes ->
    returnTc (change || changes)
--checkFds _ _ = returnTc False

checkFd free (t_x, t_y) (s_x, s_y)
  -- we need to zonk each time because unification
  -- may happen at any time
  = zonkUnifyTys free t_x s_x `thenTc` \ msubst ->
    case msubst of
      Just subst ->
	let t_y' = map (substTy (mkSubst emptyVarEnv subst)) t_y
	    s_y' = map (substTy (mkSubst emptyVarEnv subst)) s_y
	in
	    zonkEqTys t_y' s_y' `thenTc` \ eq ->
	    if eq then
		-- they're the same, nothing changes...
		returnTc False
	    else
		-- ZZ what happens if two instance vars unify?
		unifyTauTyLists t_y' s_y' `thenTc_`
		-- if we get here, something must have unified
		returnTc True
      Nothing ->
	returnTc False

zonkEqTys ts1 ts2
  = mapTc zonkTcType ts1 `thenTc` \ ts1' ->
    mapTc zonkTcType ts2 `thenTc` \ ts2' ->
    returnTc (ts1' == ts2')

zonkMatchTys ts1 free ts2
  = mapTc zonkTcType ts1 `thenTc` \ ts1' ->
    mapTc zonkTcType ts2 `thenTc` \ ts2' ->
    -- pprTrace "zMT" (ppr (ts1', free, ts2')) $
    case matchTys free ts2' ts1' of
      Just (subst, []) -> -- pprTrace "zMT match!" empty $
			  returnTc (Just subst)
      Nothing -> returnTc Nothing

zonkUnifyTys free ts1 ts2
  = mapTc zonkTcType ts1 `thenTc` \ ts1' ->
    mapTc zonkTcType ts2 `thenTc` \ ts2' ->
    -- pprTrace "zMT" (ppr (ts1', free, ts2')) $
    case unifyTyListsX free ts2' ts1' of
      Just subst -> returnTc (Just subst)
      Nothing    -> returnTc Nothing
\end{code}
