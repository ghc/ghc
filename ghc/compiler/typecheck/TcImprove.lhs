\begin{code}
module TcImprove ( tcImprove ) where

#include "HsVersions.h"

import Name		( Name )
import Class		( Class, FunDep, className )
import Unify		( unifyTyListsX )
import Subst		( mkSubst, emptyInScopeSet, substTy )
import TcEnv		( tcGetInstEnv )
import InstEnv	( classInstEnv )
import TcMonad
import TcType		( TcType, TcTyVarSet, zonkTcType )
import TcUnify		( unifyTauTyLists )
import Inst		( LIE, getFunDepsOfLIE, getIPsOfLIE )
import VarSet		( VarSet, emptyVarSet, unionVarSet )
import FunDeps		( instantiateFdClassTys )
import List		( nub )
\end{code}

\begin{code}
tcImprove :: LIE -> TcM ()
-- Do unifications based on functional dependencies in the LIE
tcImprove lie 
  = tcGetInstEnv 	`thenNF_Tc` \ inst_env ->
    let
	nfdss, clas_nfdss, inst_nfdss, ip_nfdss :: [(TcTyVarSet, Name, [FunDep TcType])]
	nfdss = ip_nfdss ++ clas_nfdss ++ inst_nfdss

	cfdss :: [(Class, [FunDep TcType])]
	cfdss      = getFunDepsOfLIE lie
	clas_nfdss = [(emptyVarSet, className c, fds) | (c,fds) <- cfdss]

	classes    = nub (map fst cfdss)
	inst_nfdss = [ (free, className c, instantiateFdClassTys c ts)
		     | c <- classes,
		       (free, ts, i) <- classInstEnv inst_env c
		     ]

	ip_nfdss = [(emptyVarSet, n, [([], [ty])]) | (n,ty) <- getIPsOfLIE lie]

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

    in
    iterImprove nfdss


iterImprove :: [(VarSet, Name, [FunDep TcType])] -> TcM ()
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
	let full_subst = mkSubst emptyInScopeSet subst
  	    t_y' = map (substTy full_subst) t_y
	    s_y' = map (substTy full_subst) s_y
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

zonkUnifyTys free ts1 ts2
  = mapTc zonkTcType ts1 `thenTc` \ ts1' ->
    mapTc zonkTcType ts2 `thenTc` \ ts2' ->
    -- pprTrace "zMT" (ppr (ts1', free, ts2')) $
    case unifyTyListsX free ts2' ts1' of
      Just subst -> returnTc (Just subst)
      Nothing    -> returnTc Nothing
\end{code}
