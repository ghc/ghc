%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprTyVar, pprTyVarBndr, pprTyVarBndrs,
	TyCon, pprTyCon, showTyCon,
	pprType, pprParendType,
	pprMaybeTy,
	getTyDescription,
	pprConstraint, pprTheta,

	nmbrType, nmbrGlobalType
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import Type		( GenType(..), Type, ThetaType, splitFunTys, splitDictTy_maybe,
			  splitForAllTys, splitSigmaTy, splitRhoTy, splitAppTys )
import TyVar		( GenTyVar(..), TyVar, cloneTyVar )
import TyCon		( TyCon, NewOrData, isFunTyCon, isTupleTyCon, tyConArity )
import Class		( Class )
import Kind		( GenKind(..), isBoxedTypeKind, pprParendKind )

-- others:
import CmdLineOpts	( opt_PprUserLength )
import Maybes		( maybeToBool )
import Name		( nameString, pprOccName, getOccString, OccName, NamedThing(..) )
import Outputable
import PprEnv
import BasicTypes	( Unused )
import UniqFM		( UniqFM, addToUFM, emptyUFM, lookupUFM  )
import Unique		( Unique, Uniquable(..), pprUnique, 
			  incrUnique, listTyConKey, initTyVarUnique 
			)
import Util
\end{code}

\begin{code}
instance Outputable (GenType flexi) where
    ppr ty = pprType ty

instance Outputable TyCon where
    ppr tycon = pprTyCon tycon

instance Outputable Class where
    -- we use pprIfaceClass for printing in interfaces
    ppr clas = ppr (getName clas)

instance Outputable (GenTyVar flexi) where
    ppr tv = pprTyVar tv
\end{code}

%************************************************************************
%*									*
\subsection[Type]{@Type@}
%*									*
%************************************************************************

Precedence
~~~~~~~~~~
@ppr_ty@ takes an @Int@ that is the precedence of the context.
The precedence levels are:
\begin{description}
\item[tOP_PREC]   No parens required.
\item[fUN_PREC]   Left hand argument of a function arrow.
\item[tYCON_PREC] Argument of a type constructor.
\end{description}


\begin{code}
tOP_PREC    = (0 :: Int)
fUN_PREC    = (1 :: Int)
tYCON_PREC  = (2 :: Int)

maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty
\end{code}

@pprType@ is the std @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

\begin{code}
pprType, pprParendType :: GenType flexi -> SDoc

pprType       ty = ppr_ty init_ppr_env tOP_PREC   ty
pprParendType ty = ppr_ty init_ppr_env tYCON_PREC ty

pprConstraint :: Class -> [GenType flexi] -> SDoc
pprConstraint clas tys = hsep [ppr clas, hsep (map (pprParendType) tys)]

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (hsep (punctuate comma (map ppr_dict theta)))
	       where
		 ppr_dict (c,tys) = pprConstraint c tys

pprMaybeTy :: Maybe (GenType flexi) -> SDoc
pprMaybeTy Nothing   = char '*'
pprMaybeTy (Just ty) = pprParendType ty
\end{code}

\begin{code}
ppr_ty :: PprEnv flexi bndr occ -> Int
       -> GenType flexi
       -> SDoc

ppr_ty env ctxt_prec (TyVarTy tyvar)
  = pTyVarO env tyvar

	-- TUPLE CASE
ppr_ty env ctxt_prec (TyConApp tycon tys)
  |  isTupleTyCon tycon
  && length tys == tyConArity tycon		-- no magic if partially applied
  = parens tys_w_commas
  where
    tys_w_commas = hsep (punctuate comma (map (ppr_ty env tOP_PREC) tys))

	-- LIST CASE
ppr_ty env ctxt_prec (TyConApp tycon [ty])
  |  uniqueOf tycon == listTyConKey
  = brackets (ppr_ty env tOP_PREC ty)

	-- DICTIONARY CASE, prints {C a}
	-- This means that instance decls come out looking right in interfaces
	-- and that in turn means they get "gated" correctly when being slurped in
ppr_ty env ctxt_prec ty@(TyConApp tycon tys)
  | maybeToBool maybe_dict
  = braces (ppr_dict env tYCON_PREC ctys)
  where
    Just ctys = maybe_dict
    maybe_dict = splitDictTy_maybe ty
  
	-- NO-ARGUMENT CASE (=> no parens)
ppr_ty env ctxt_prec (TyConApp tycon [])
  = ppr_tycon env tycon

	-- GENERAL CASE
ppr_ty env ctxt_prec (TyConApp tycon tys)
  = maybeParen ctxt_prec tYCON_PREC (hsep [ppr_tycon env tycon, tys_w_spaces])
  where
    tys_w_spaces = hsep (map (ppr_ty env tYCON_PREC) tys)


ppr_ty env ctxt_prec ty@(ForAllTy _ _)
  = getPprStyle $ \ sty -> 
    let
    	(tyvars, rho_ty) = splitForAllTys ty
    	(theta, body_ty) | show_context = splitRhoTy rho_ty
			 | otherwise    = ([], rho_ty)
    
    	pp_tyvars = brackets (hsep (map (pTyVarB env) tyvars))
    	pp_body   = ppr_ty env tOP_PREC body_ty
    
    	show_forall  = not (userStyle sty)
    	show_context = ifaceStyle sty || userStyle sty
    in
    if show_forall then
       maybeParen ctxt_prec fUN_PREC $
       sep [ ptext SLIT("_forall_"), pp_tyvars, 
	     ppr_theta env theta, ptext SLIT("=>"), pp_body
       ]

    else if null theta then
       ppr_ty env ctxt_prec body_ty

    else
       maybeParen ctxt_prec fUN_PREC $
       sep [ppr_theta env theta, ptext SLIT("=>"), pp_body]

ppr_ty env ctxt_prec (FunTy ty1 ty2)
    -- We fiddle the precedences passed to left/right branches,
    -- so that right associativity comes out nicely...
  = maybeParen ctxt_prec fUN_PREC (sep (ppr_ty env fUN_PREC ty1 : pp_rest))
  where
    (arg_tys, result_ty) = splitFunTys ty2
    pp_rest = [ ptext SLIT("-> ") <> ppr_ty env fUN_PREC ty | ty <- arg_tys ++ [result_ty] ]

ppr_ty env ctxt_prec (AppTy ty1 ty2)
  = maybeParen ctxt_prec tYCON_PREC $
    ppr_ty env tOP_PREC ty1 <+> ppr_ty env tYCON_PREC ty2

ppr_ty env ctxt_prec (SynTy ty expansion)
  = ppr_ty env ctxt_prec ty

ppr_theta env []    = empty
ppr_theta env theta = braces (hsep (punctuate comma (map (ppr_dict env tOP_PREC) theta)))

ppr_dict env ctxt (clas, tys) = ppr_class env clas <+> 
				hsep (map (ppr_ty env tYCON_PREC) tys)
\end{code}

\begin{code}
init_ppr_env
  = initPprEnv b b b b (Just pprTyVarBndr) (Just ppr) b b b
  where
    b = panic "PprType:init_ppr_env"

ppr_tycon  env tycon = ppr tycon
ppr_class  env clas  = ppr clas
\end{code}

%************************************************************************
%*									*
\subsection[TyVar]{@TyVar@}
%*									*
%************************************************************************

\begin{code}
pprTyVar (TyVar uniq kind maybe_name _)
  = case maybe_name of
      	-- If the tyvar has a name we can safely use just it, I think
	Just n  -> pprOccName (getOccName n) <> ifPprDebug pp_debug
	Nothing -> pprUnique uniq
  where
    pp_debug = text "_" <> pp_kind <> pprUnique uniq

    pp_kind = case kind of
		TypeKind        -> char 'o'
		BoxedTypeKind   -> char 't'
		UnboxedTypeKind -> char 'u'
		ArrowKind _ _   -> char 'a'
\end{code}

We print type-variable binders with their kinds in interface files.

\begin{code}
pprTyVarBndr tyvar@(TyVar uniq kind name _)
  = getPprStyle $ \ sty ->
    if ifaceStyle sty && not (isBoxedTypeKind kind) then
        hcat [pprTyVar tyvar, text " :: ", pprParendKind kind]
	-- See comments with ppDcolon in PprCore.lhs
    else
        pprTyVar tyvar

pprTyVarBndrs tyvars = hsep (map pprTyVarBndr tyvars)
\end{code}

%************************************************************************
%*									*
\subsection[TyCon]{@TyCon@}
%*									*
%************************************************************************

ToDo; all this is suspiciously like getOccName!

\begin{code}
showTyCon :: TyCon -> String
showTyCon tycon = showSDoc (pprTyCon tycon)

pprTyCon :: TyCon -> SDoc
pprTyCon tycon = ppr (getName tycon)
\end{code}



%************************************************************************
%*									*
\subsection{Mumbo jumbo}
%*									*
%************************************************************************

Grab a name for the type. This is used to determine the type
description for profiling.
\begin{code}
getTyDescription :: Type -> String

getTyDescription ty
  = case (splitSigmaTy ty) of { (_, _, tau_ty) ->
    case tau_ty of
      TyVarTy _	       -> "*"
      AppTy fun _      -> getTyDescription fun
      FunTy _ res      -> '-' : '>' : fun_result res
      TyConApp tycon _ -> getOccString tycon
      SynTy ty1 _      -> getTyDescription ty1
      ForAllTy _ ty    -> getTyDescription ty
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other
\end{code}



%************************************************************************
%*									*
\subsection{Renumbering types}
%*									*
%************************************************************************

We tend to {\em renumber} everything before printing, so that we get
consistent Uniques on everything from run to run.


\begin{code}
nmbrGlobalType :: Type -> Type		-- Renumber a top-level type
nmbrGlobalType ty = nmbrType (\tyvar -> tyvar) initTyVarUnique ty

nmbrType :: (TyVar -> TyVar)		-- Mapping for free vars
	 -> Unique
	 -> Type
	 -> Type

nmbrType tyvar_env uniq ty
  = initNmbr tyvar_env uniq (nmbrTy ty)

nmbrTy :: Type -> NmbrM Type

nmbrTy (TyVarTy tv)
  = lookupTyVar tv    `thenNmbr` \ new_tv ->
    returnNmbr (TyVarTy new_tv)

nmbrTy (AppTy t1 t2)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (AppTy new_t1 new_t2)

nmbrTy (TyConApp tc tys)
  = nmbrTys tys		`thenNmbr` \ new_tys ->
    returnNmbr (TyConApp tc new_tys)

nmbrTy (SynTy ty1 ty2)
  = nmbrTy ty1	    `thenNmbr` \ new_ty1 ->
    nmbrTy ty2	    `thenNmbr` \ new_ty2 ->
    returnNmbr (SynTy new_ty1 new_ty2)

nmbrTy (ForAllTy tv ty)
  = addTyVar tv		$ \ new_tv ->
    nmbrTy ty		`thenNmbr` \ new_ty ->
    returnNmbr (ForAllTy new_tv new_ty)

nmbrTy (FunTy t1 t2)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (FunTy new_t1 new_t2)


nmbrTys tys = mapNmbr nmbrTy tys

lookupTyVar tyvar (NmbrEnv tv_fn tv_env) uniq
  = (uniq, tyvar')
  where
    tyvar' = case lookupUFM tv_env tyvar of
		Just tyvar' -> tyvar'
		Nothing     -> tv_fn tyvar

addTyVar tv m (NmbrEnv f_tv tv_ufm) u
  = m tv' nenv u'
  where
    nenv    = NmbrEnv f_tv tv_ufm'
    tv_ufm' = addToUFM tv_ufm tv tv'
    tv'	    = cloneTyVar tv u
    u'      = incrUnique u
\end{code}

Monad stuff

\begin{code}
data NmbrEnv
  = NmbrEnv (TyVar -> TyVar) (UniqFM TyVar)		-- Global and local map for tyvars

type NmbrM a = NmbrEnv -> Unique -> (Unique, a)		-- Unique is name supply

initNmbr :: (TyVar -> TyVar) -> Unique -> NmbrM a -> a
initNmbr tyvar_env uniq m
  = let
	init_nmbr_env = NmbrEnv tyvar_env emptyUFM
    in
    snd (m init_nmbr_env uniq)

returnNmbr x nenv u = (u, x)

thenNmbr m k nenv u
  = let
	(u', res) = m nenv u
    in
    k res nenv u'


mapNmbr f []     = returnNmbr []
mapNmbr f (x:xs)
  = f x		    `thenNmbr` \ r  ->
    mapNmbr f xs    `thenNmbr` \ rs ->
    returnNmbr (r:rs)
\end{code}
