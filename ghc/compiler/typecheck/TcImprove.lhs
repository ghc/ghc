\begin{code}
module TcImprove ( tcImprove ) where

#include "HsVersions.h"

import Type		( tyVarsOfTypes )
import Class		( classInstEnv, classExtraBigSig )
import Unify		( matchTys )
import Subst		( mkSubst, substTy )
import TcMonad
import TcType		( zonkTcType, zonkTcTypes )
import TcUnify		( unifyTauTyLists )
import Inst		( Inst, LookupInstResult(..),
			  lookupInst, isDict, getDictClassTys, getFunDepsOfLIE,
			  zonkLIE, zonkFunDeps {- for debugging -} )
import InstEnv		( InstEnv )		-- Reqd for 4.02; InstEnv is a synonym, and
						-- 4.02 doesn't "see" it soon enough
import VarSet		( emptyVarSet )
import VarEnv		( emptyVarEnv )
import FunDeps		( instantiateFdClassTys )
import Bag		( bagToList )
import Outputable
import List		( elemIndex )
\end{code}

Improvement goes here.

\begin{code}
tcImprove lie =
    if null cfdss then
	returnTc ()
    else
	-- zonkCfdss cfdss `thenTc` \ cfdss' ->
	-- pprTrace "tcI" (ppr cfdss') $
	iterImprove cfdss
    where cfdss = getFunDepsOfLIE lie

iterImprove [] = returnTc ()
iterImprove cfdss
  = -- zonkCfdss cfdss `thenTc` \ cfdss' ->
    -- pprTrace "iterI" (ppr cfdss') $
    instImprove cfdss			`thenTc` \ change1 ->
    selfImprove pairImprove cfdss	`thenTc` \ change2 ->
    if change1 || change2 then
	iterImprove cfdss
    else
	returnTc ()

-- ZZ debugging...
zonkCfdss ((c, fds) : cfdss)
  = zonkFunDeps fds `thenTc` \ fds' ->
    zonkCfdss cfdss `thenTc` \ cfdss' ->
    returnTc ((c, fds') : cfdss')
zonkCfdss [] = returnTc []

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

selfImprove f [] = returnTc False
selfImprove f (cfds : cfdss)
  = mapTc (f cfds) cfdss	`thenTc` \ changes ->
    anyTc changes		`thenTc` \ changed ->
    selfImprove f cfdss		`thenTc` \ rest_changed ->
    returnTc (changed || rest_changed)

pairImprove (clas1, fds1) (clas2, fds2)
  = if clas1 == clas2 then
	checkFds fds1 emptyVarSet fds2
    else
	returnTc False

checkFds [] free [] = returnTc False
checkFds (fd1 : fd1s) free (fd2 : fd2s) =
    checkFd fd1 free fd2	`thenTc` \ change ->
    checkFds fd1s free fd2s	`thenTc` \ changes ->
    returnTc (change || changes)
--checkFds _ _ = returnTc False

checkFd (t_x, t_y) free (s_x, s_y)
  -- we need to zonk each time because unification
  -- may happen at any time
  = zonkMatchTys t_x free s_x `thenTc` \ msubst ->
    case msubst of
      Just subst ->
	let s_y' = map (substTy (mkSubst emptyVarEnv subst)) s_y in
	    zonkEqTys t_y s_y' `thenTc` \ eq ->
	    if eq then
		-- they're the same, nothing changes...
		returnTc False
	    else
		unifyTauTyLists t_y s_y' `thenTc_`
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
\end{code}

Utilities:

A monadic version of the standard Prelude `or' function.
\begin{code}
anyTc bs = foldrTc (\a b -> returnTc (a || b)) False bs
\end{code}
