\begin{code}
module TcImprove ( tcImprove ) where

#include "HsVersions.h"

import Name		( Name )
import Type		( Type, tyVarsOfTypes )
import Class		( className, classInstEnv, classExtraBigSig )
import Unify		( unifyTyListsX, matchTys )
import Subst		( mkSubst, substTy )
import TcMonad
import TcType		( zonkTcType, zonkTcTypes )
import TcUnify		( unifyTauTyLists )
import Inst		( Inst, LookupInstResult(..),
			  lookupInst, isDict, getFunDepsOfLIE, getIPsOfLIE,
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
tcImprove lie =
    if null cfdss then
	returnTc ()
    else
	-- zonkCfdss cfdss `thenTc` \ cfdss' ->
	-- pprTrace "tcI" (ppr cfdss') $
	iterImprove nfdss
    where
	cfdss = getFunDepsOfLIE lie
	clas_nfdss = map (\(c, fds) -> (emptyVarSet, className c, fds)) cfdss
	classes = nub (map fst cfdss)
	inst_nfdss = concatMap getInstNfdssOf classes
	ips = getIPsOfLIE lie
	ip_nfdss = map (\(n, ty) -> (emptyVarSet, n, [([], [ty])])) ips
	nfdss = ip_nfdss ++ clas_nfdss ++ inst_nfdss

getInstNfdssOf clas = nfdss
    where
	nm = className clas
	ins = classInstEnv clas
	mk_nfds (free, ts, i) = (free, nm, instantiateFdClassTys clas ts)
	nfdss = map mk_nfds ins

iterImprove :: [(VarSet, Name, [([Type],[Type])])] -> TcM s ()
iterImprove [] = returnTc ()
iterImprove cfdss
  = -- zonkCfdss cfdss `thenTc` \ cfdss' ->
    -- pprTrace "iterI" (ppr cfdss') $
    -- instImprove cfdss			`thenTc` \ change1 ->
    selfImprove pairImprove cfdss	`thenTc` \ change2 ->
    if {- change1 || -} change2 then
	iterImprove cfdss
    else
	returnTc ()

-- ZZ debugging...
zonkCfdss ((c, fds) : cfdss)
  = zonkFunDeps fds `thenTc` \ fds' ->
    zonkCfdss cfdss `thenTc` \ cfdss' ->
    returnTc ((c, fds') : cfdss')
zonkCfdss [] = returnTc []

{-
instImprove (cfds@(clas, fds) : cfdss)
  = instImprove1 cfds ins	`thenTc` \ changed ->
    instImprove cfdss		`thenTc` \ rest_changed ->
    returnTc (changed || rest_changed)
  where ins = classInstEnv clas
instImprove [] = returnTc False

instImprove1 cfds@(clas, fds1) ((free, ts, i) : ins)
  = -- pprTrace "iI1" (ppr (free, ts, i)) $
    checkFds fds1 free fds2	`thenTc` \ changed ->
    instImprove1 cfds ins	`thenTc` \ rest_changed ->
    returnTc (changed || rest_changed)
  where fds2 = instantiateFdClassTys clas ts
instImprove1 _ _ = returnTc False
-}

-- ZZ this will do a lot of redundant checking wrt instances
-- it would do to make this operate over two lists, the first
-- with only clas_nfds and ip_nfds, and the second with everything
-- control would otherwise mimic the current loop, so that the
-- caller could control whether the redundant inst improvements
-- were avoided
-- you could then also use this to check for consistency of new instances
selfImprove f [] = returnTc False
selfImprove f (nfds : nfdss)
  = mapTc (f nfds) nfdss	`thenTc` \ changes ->
    anyTc changes		`thenTc` \ changed ->
    selfImprove f nfdss		`thenTc` \ rest_changed ->
    returnTc (changed || rest_changed)

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
      Just subst {- (subst, []) -} -> -- pprTrace "zMT match!" empty $
			  returnTc (Just subst)
      Nothing -> returnTc Nothing
\end{code}

Utilities:

A monadic version of the standard Prelude `or' function.
\begin{code}
anyTc bs = foldrTc (\a b -> returnTc (a || b)) False bs
\end{code}
