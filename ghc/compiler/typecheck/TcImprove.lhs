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
			  lookupInst, isDict, getDictClassTys, getFunDeps,
			  zonkLIE {- for debugging -} )
import VarSet		( emptyVarSet )
import VarEnv		( emptyVarEnv )
import FunDeps		( instantiateFdClassTys )
import Bag		( bagToList )
import Outputable
import List		( elemIndex )
import Maybe		( catMaybes )
\end{code}

Improvement goes here.

\begin{code}
tcImprove lie
  = let cfdss = catMaybes (map getFunDeps (bagToList lie)) in
    iterImprove cfdss

iterImprove cfdss
  = instImprove cfdss			`thenTc` \ change1 ->
    selfImprove pairImprove cfdss	`thenTc` \ change2 ->
    if change1 || change2 then
	iterImprove cfdss
    else
	returnTc ()

instImprove (cfds@(clas, fds) : cfdss)
  = instImprove1 cfds ins
  where ins = classInstEnv clas
instImprove [] = returnTc False

instImprove1 cfds@(clas, fds1) ((free, ts, _) : ins)
  = checkFds fds1 free fds2	`thenTc` \ changed ->
    instImprove1 cfds ins	`thenTc` \ rest_changed ->
    returnTc (changed || rest_changed)
  where fds2 = instantiateFdClassTys clas ts
instImprove1 _ _ = returnTc False

selfImprove f [] = returnTc False
selfImprove f (cfds : cfdss)
  = mapTc (f cfds) cfdss	`thenTc` \ changes ->
    orTc changes		`thenTc` \ changed ->
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
	    zonkMatchTys t_y free s_y `thenTc` \ msubst2 ->
		case msubst2 of
		  Just _ ->
		    -- they're the same, nothing changes
		    returnTc False
		  Nothing ->
		    unifyTauTyLists t_y s_y' `thenTc_`
		    -- if we get here, something must have unified
		    returnTc True
      Nothing ->
	returnTc False

zonkMatchTys ts1 free ts2
  = mapTc zonkTcType ts1 `thenTc` \ ts1' ->
    mapTc zonkTcType ts2 `thenTc` \ ts2' ->
    --returnTc (ts1' == ts2')
    case matchTys free ts2' ts1' of
      Just (subst, []) -> returnTc (Just subst)
      Nothing -> returnTc Nothing

{-
instImprove clas fds =
    pprTrace "class inst env" (ppr (clas, classInstEnv clas)) $
    zonkFunDeps fds `thenTc` \ fds' ->
    pprTrace "lIEFDs" (ppr (clas, fds')) $
    case lookupInstEnvFDs clas fds' of
      Nothing -> returnTc ()
      Just (t_y, s_y) ->
	pprTrace "lIEFDs result" (ppr (t_y, s_y)) $
	unifyTauTyLists t_y s_y

lookupInstEnvFDs clas fds
  = find env
  where
    env = classInstEnv clas
    (ctvs, fds, _, _, _, _) = classExtraBigSig clas
    find [] = Nothing
    find ((tpl_tyvars, tpl, val) : rest)
      = let tplx = concatMap (\us -> thingy tpl us ctvs) (map fst fds)
	    tply = concatMap (\vs -> thingy tpl vs ctvs) (map snd fds)
        in
	    case matchTys tpl_tyvars tplx tysx of
	      Nothing -> find rest
	      Just (tenv, leftovers) ->
		let subst = mkSubst (tyVarsOfTypes tys) tenv
		in
		    -- this is the list of things that
		    -- need to be unified
		    Just (map (substTy subst) tply, tysy)
    tysx = concatMap (\us -> thingy tys us ctvs) (map fst fds)
    tysy = concatMap (\vs -> thingy tys vs ctvs) (map snd fds)
    thingy f us ctvs
      = map (f !!) is
	where is = map (\u -> let Just i = elemIndex u ctvs in i) us
-}

{-
  = let (clas, tys) = getDictClassTys dict
    in
	-- first, do instance-based improvement
	instImprove clas tys `thenTc_`
	-- OK, now do pairwise stuff
	mapTc (f clas tys) dicts `thenTc` \ changes ->
	foldrTc (\a b -> returnTc (a || b)) False changes `thenTc` \ changed ->
	allDictPairs f dicts `thenTc` \ rest_changed ->
	returnTc (changed || rest_changed)
-}

\end{code}

Utilities:

A monadic version of the standard Prelude `or' function.
\begin{code}
orTc bs = foldrTc (\a b -> returnTc (a || b)) False bs
\end{code}
