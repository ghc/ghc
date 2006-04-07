%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[SATMonad]{The Static Argument Transformation pass Monad}
%*									*
%************************************************************************

96/03: We aren't using the static-argument transformation right now.

\begin{code}
module SATMonad where

#include "HsVersions.h"

import Panic		( panic )

junk_from_SATMonad = panic "SATMonad.junk"

{- LATER: to end of file:

module SATMonad (
	SATInfo(..), updSAEnv,
	SatM(..), initSAT, emptyEnvSAT,
	returnSAT, thenSAT, thenSAT_, mapSAT, getSATInfo, newSATName,
	getArgLists, Arg(..), insSAEnv, saTransform,

	SATEnv(..), isStatic, dropStatics
    ) where

import Type		( mkTyVarTy, mkSigmaTy,
			  splitSigmaTy, splitFunTys,
			  glueTyArgs, substTy,
			  InstTyEnv(..)
			)
import MkId		( mkSysLocal )
import Id		( idType, idName, mkLocalId )
import UniqSupply
import Util

infixr 9 `thenSAT`, `thenSAT_`
\end{code}

%************************************************************************
%*									*
\subsection{Static Argument Transformation Environment}
%*									*
%************************************************************************

\begin{code}
type SATEnv = IdEnv SATInfo

type SATInfo = ([Arg Type],[Arg Id])

data Arg a = Static a | NotStatic
    deriving Eq

delOneFromSAEnv v us env
  = ((), delVarEnv env v)

updSAEnv :: Maybe (Id,SATInfo) -> SatM ()
updSAEnv Nothing
  = returnSAT ()
updSAEnv (Just (b,(tyargs,args)))
  = getSATInfo b      `thenSAT` (\ r ->
    case r of
      Nothing		   -> returnSAT ()
      Just (tyargs',args') -> delOneFromSAEnv b `thenSAT_`
			      insSAEnv b (checkArgs tyargs tyargs',
					  checkArgs args args')
    )

checkArgs as [] = notStatics (length as)
checkArgs [] as = notStatics (length as)
checkArgs (a:as) (a':as') | a == a' = a:checkArgs as as'
checkArgs (_:as) (_:as') = NotStatic:checkArgs as as'

notStatics :: Int -> [Arg a]
notStatics n = nOfThem n NotStatic

insSAEnv :: Id -> SATInfo -> SatM ()
insSAEnv b info us env
  = ((), extendVarEnv env b info)
\end{code}

%************************************************************************
%*									*
\subsection{Static Argument Transformation Monad}
%*									*
%************************************************************************

Two items of state to thread around: a UniqueSupply and a SATEnv.

\begin{code}
type SatM result
  =  UniqSupply -> SATEnv -> (result, SATEnv)

initSAT :: SatM a -> UniqSupply -> a

initSAT f us = fst (f us emptyVarEnv)

thenSAT m k us env
  = case splitUniqSupply us	of { (s1, s2) ->
    case m s1 env   	    	of { (m_result, menv) ->
    k m_result s2 menv }}

thenSAT_ m k us env
  = case splitUniqSupply us	of { (s1, s2) ->
    case m s1 env   	    	of { (_, menv) ->
    k s2 menv }}

emptyEnvSAT :: SatM ()
emptyEnvSAT us _ = ((), emptyVarEnv)

returnSAT v us env = (v, env)

mapSAT f [] 	= returnSAT []
mapSAT f (x:xs)
  = f x		`thenSAT` \ x'	->
    mapSAT f xs	`thenSAT` \ xs' ->
    returnSAT (x':xs')
\end{code}

%************************************************************************
%*									*
\subsection{Utility Functions}
%*									*
%************************************************************************

\begin{code}
getSATInfo :: Id -> SatM (Maybe SATInfo)
getSATInfo var us env
  = (lookupVarEnv env var, env)

newSATName :: Id -> Type -> SatM Id
newSATName id ty us env
  = case (getUnique us) of { unique ->
    let
	new_name = mkCompoundName SLIT("$sat") unique (idName id)
    in
    (mkLocalId new_name ty, env) }

getArgLists :: CoreExpr -> ([Arg Type],[Arg Id])
getArgLists expr
  = let
	(tvs, lambda_bounds, body) = collectBinders expr
    in
    ([ Static (mkTyVarTy tv) | tv <- tvs ],
     [ Static v		     | v <- lambda_bounds ])

dropArgs :: CoreExpr -> CoreExpr
dropArgs (Lam   _ e)	= dropArgs e
dropArgs (CoTyLam _ e)	= dropArgs e
dropArgs e		= e
\end{code}

We implement saTransform using shadowing of binders, that is
we transform
map = \f as -> case as of
		 [] -> []
		 (a':as') -> let x = f a'
				 y = map f as'
			     in x:y
to
map = \f as -> let map = \f as -> map' as
	       in let rec map' = \as -> case as of
					  [] -> []
					  (a':as') -> let x = f a'
							  y = map f as'
						      in x:y
		  in map' as

the inner map should get inlined and eliminated.
\begin{code}
saTransform :: Id -> CoreExpr -> SatM CoreBinding
saTransform binder rhs
  = getSATInfo binder `thenSAT` \ r ->
    case r of
      -- [Andre] test: do it only if we have more than one static argument.
      --Just (tyargs,args) | any isStatic args
      Just (tyargs,args) | (filter isStatic args) `lengthExceeds` 1
	-> newSATName binder (new_ty tyargs args)  `thenSAT` \ binder' ->
	   mkNewRhs binder binder' tyargs args rhs `thenSAT` \ new_rhs ->
	   trace ("SAT "++ show (length (filter isStatic args))) (
	   returnSAT (NonRec binder new_rhs)
	   )
      _ -> returnSAT (Rec [(binder, rhs)])
  where
    mkNewRhs binder binder' tyargs args rhs
      = let
	    non_static_args :: [Id]
	    non_static_args
	       = get_nsa args (snd (getArgLists rhs))
	       where
		 get_nsa :: [Arg a] -> [Arg a] -> [a]
		 get_nsa [] _ = []
		 get_nsa _ [] = []
		 get_nsa (NotStatic:args) (Static v:as) = v:get_nsa args as
		 get_nsa (_:args)	  (_:as)	=   get_nsa args as

	    local_body = foldl App (Var binder')
				[VarArg a | a <- non_static_args]

	    nonrec_rhs = origLams local_body

	    -- HACK! The following is a fake SysLocal binder with
	    --  *the same* unique as binder.
	    -- the reason for this is the following:
	    -- this binder *will* get inlined but if it happen to be
	    -- a top level binder it is never removed as dead code,
	    -- therefore we have to remove that information (of it being
	    -- top-level or exported somehow.)
	    -- A better fix is to use binder directly but with the TopLevel
	    -- tag (or Exported tag) modified.
	    fake_binder = mkSysLocal SLIT("sat")
			    (getUnique binder)
			    (idType binder)
	    rec_body = mkValLam non_static_args
			       ( Let (NonRec fake_binder nonrec_rhs)
				 {-in-} (dropArgs rhs))
	in
	returnSAT (
	    origLams (Let (Rec [(binder',rec_body)]) {-in-} local_body)
	)
      where
	origLams = origLams' rhs
		 where
		   origLams' (Lam v e)     e' = Lam   v  (origLams' e e')
		   origLams' (CoTyLam ty e)  e' = CoTyLam ty (origLams' e e')
		   origLams' _		     e' = e'

    new_ty tyargs args
      = substTy (mk_inst_tyenv tyargs tv_tmpl)
		      (mkSigmaTy tv_tmpl' dict_tys' tau_ty')
      where
	-- get type info for the local function:
	(tv_tmpl, dict_tys, tau_ty) = (splitSigmaTy . idType) binder
	(reg_arg_tys, res_type)	    = splitFunTys tau_ty

	-- now, we drop the ones that are
	-- static, that is, the ones we will not pass to the local function
	tv_tmpl'     = dropStatics tyargs tv_tmpl

	(args1, args2) = splitAtList dict_tys args
	dict_tys'    = dropStatics args1 dict_tys
	reg_arg_tys' = dropStatics args2 reg_arg_tys

	tau_ty'	     = glueTyArgs reg_arg_tys' res_type

	mk_inst_tyenv []		    _ = emptyVarEnv
	mk_inst_tyenv (Static s:args) (t:ts)  = extendVarEnv (mk_inst_tyenv args ts) t s
	mk_inst_tyenv (_:args)	    (_:ts)    = mk_inst_tyenv args ts

dropStatics [] t = t
dropStatics (Static _:args) (t:ts) = dropStatics args ts
dropStatics (_:args)	    (t:ts) = t:dropStatics args ts

isStatic :: Arg a -> Bool
isStatic NotStatic = False
isStatic _	   = True
-}
\end{code}
