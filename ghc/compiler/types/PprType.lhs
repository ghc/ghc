%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprKind, pprParendKind,
	pprType, pprParendType,
	pprConstraint, pprTheta,
	pprTyVarBndr, pprTyVarBndrs,

	getTyDescription,

	nmbrType, nmbrGlobalType
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import Type		( GenType(..), TyNote(..), Kind, Type, ThetaType, 
			  splitFunTys, splitDictTy_maybe,
			  splitForAllTys, splitSigmaTy, splitRhoTy,
			  boxedTypeKind
			)
import Var		( GenTyVar, TyVar, tyVarKind,
			  tyVarName, setTyVarName
			)
import VarEnv
import TyCon		( TyCon, isTupleTyCon, isUnboxedTupleTyCon, tyConArity )
import Class		( Class )

-- others:
import Maybes		( maybeToBool )
import Name		( getOccString, setNameVisibility, NamedThing(..) )
import Outputable
import PprEnv
import Unique		( Unique, Uniquable(..),
			  incrUnique, listTyConKey, initTyVarUnique 
			)
import Util
\end{code}

%************************************************************************
%*									*
\subsection{The external interface}
%*									*
%************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

\begin{code}
pprType, pprParendType :: GenType flexi -> SDoc
pprType       ty = ppr_ty pprTyEnv tOP_PREC   ty
pprParendType ty = ppr_ty pprTyEnv tYCON_PREC ty

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

pprConstraint :: Class -> [GenType flexi] -> SDoc
pprConstraint clas tys = ppr clas <+> hsep (map (pprParendType) tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (hsep (punctuate comma (map ppr_dict theta)))
	       where
		 ppr_dict (c,tys) = pprConstraint c tys

instance Outputable (GenType flexi) where
    ppr ty = pprType ty
\end{code}


%************************************************************************
%*									*
\subsection{Pretty printing}
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

\begin{code}
ppr_ty :: PprEnv (GenTyVar flexi) flexi -> Int
       -> GenType flexi
       -> SDoc

ppr_ty env ctxt_prec (TyVarTy tyvar)
  = pTyVarO env tyvar

	-- TUPLE CASE (boxed and unboxed)
ppr_ty env ctxt_prec (TyConApp tycon tys)
  |  isTupleTyCon tycon
  && length tys == tyConArity tycon	-- no magic if partially applied
  = parens tys_w_commas

  |  isUnboxedTupleTyCon tycon
  && length tys == tyConArity tycon	-- no magic if partially applied
  = parens (char '#' <+> tys_w_commas <+> char '#')
  where
    tys_w_commas = sep (punctuate comma (map (ppr_ty env tOP_PREC) tys))

	-- LIST CASE
ppr_ty env ctxt_prec (TyConApp tycon [ty])
  |  getUnique tycon == listTyConKey
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
  = ppr tycon

	-- GENERAL CASE
ppr_ty env ctxt_prec (TyConApp tycon tys)
  = maybeParen ctxt_prec tYCON_PREC (hsep [ppr tycon, tys_w_spaces])
  where
    tys_w_spaces = hsep (map (ppr_ty env tYCON_PREC) tys)


ppr_ty env ctxt_prec ty@(ForAllTy _ _)
  = getPprStyle $ \ sty -> 
    maybeParen ctxt_prec fUN_PREC $
    if userStyle sty then
       sep [ ptext SLIT("forall"), pp_tyvars, ptext SLIT("."), pp_maybe_ctxt, pp_body ]
    else
       sep [ ptext SLIT("__forall"), brackets pp_tyvars, pp_ctxt, pp_body ]
  where		
    (tyvars, rho_ty) = splitForAllTys ty
    (theta, body_ty) = splitRhoTy rho_ty
    
    pp_tyvars = hsep (map (pBndr env LambdaBind) tyvars)
    pp_body   = ppr_ty env tOP_PREC body_ty
    
    pp_maybe_ctxt | null theta = empty
	          | otherwise  = pp_ctxt

    pp_ctxt = ppr_theta env theta <+> ptext SLIT("=>") 


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

ppr_ty env ctxt_prec (NoteTy (SynNote ty) expansion)
  = ppr_ty env ctxt_prec ty

ppr_ty env ctxt_prec (NoteTy (FTVNote _) ty) = ppr_ty env ctxt_prec ty

ppr_theta env []    = empty
ppr_theta env theta = braces (hsep (punctuate comma (map (ppr_dict env tOP_PREC) theta)))

ppr_dict env ctxt (clas, tys) = ppr clas <+> 
				hsep (map (ppr_ty env tYCON_PREC) tys)
\end{code}

\begin{code}
pprTyEnv = initPprEnv b b (Just ppr) b (Just (\site -> pprTyVarBndr)) b
  where
    b = panic "PprType:init_ppr_env"
\end{code}

%************************************************************************
%*									*
\subsection[TyVar]{@TyVar@}
%*									*
%************************************************************************

We print type-variable binders with their kinds in interface files,
and when in debug mode.

\begin{code}
pprTyVarBndr tyvar
  = getPprStyle $ \ sty ->
    if (ifaceStyle sty || debugStyle sty) && kind /= boxedTypeKind then
        hcat [ppr tyvar, text " :: ", pprParendKind kind]
		-- See comments with ppDcolon in PprCore.lhs
    else
        ppr tyvar
  where
    kind = tyVarKind tyvar

pprTyVarBndrs tyvars = hsep (map pprTyVarBndr tyvars)
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
      NoteTy (FTVNote _) ty  -> getTyDescription ty
      NoteTy (SynNote ty1) _ -> getTyDescription ty1
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
nmbrGlobalType ty = nmbrType emptyVarEnv initTyVarUnique ty

nmbrType :: TyVarEnv Type 	-- Substitution
	 -> Unique		-- This unique and its successors are not 
				-- free in the range of the substitution
	 -> Type
	 -> Type

nmbrType tyvar_env uniq ty
  = initNmbr tyvar_env uniq (nmbrTy ty)

nmbrTy :: Type -> NmbrM Type

nmbrTy (TyVarTy tv)
  = lookupTyVar tv

nmbrTy (AppTy t1 t2)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (AppTy new_t1 new_t2)

nmbrTy (TyConApp tc tys)
  = mapNmbr nmbrTy tys		`thenNmbr` \ new_tys ->
    returnNmbr (TyConApp tc new_tys)

nmbrTy (NoteTy (SynNote ty1) ty2)
  = nmbrTy ty1	    `thenNmbr` \ new_ty1 ->
    nmbrTy ty2	    `thenNmbr` \ new_ty2 ->
    returnNmbr (NoteTy (SynNote new_ty1) new_ty2)

nmbrTy (NoteTy (FTVNote _) ty2) = nmbrTy ty2

nmbrTy (ForAllTy tv ty)
  = addTyVar tv		$ \ new_tv ->
    nmbrTy ty		`thenNmbr` \ new_ty ->
    returnNmbr (ForAllTy new_tv new_ty)

nmbrTy (FunTy t1 t2)
  = nmbrTy t1	    `thenNmbr` \ new_t1 ->
    nmbrTy t2	    `thenNmbr` \ new_t2 ->
    returnNmbr (FunTy new_t1 new_t2)


lookupTyVar tyvar env uniq
  = (uniq, ty)
  where
    ty = case lookupVarEnv env tyvar of
		Just ty -> ty
		Nothing -> TyVarTy tyvar

addTyVar tv m env u
  = m tv' env' u'
  where
    env' = extendVarEnv env tv (TyVarTy tv')
    tv'	 = setTyVarName tv (setNameVisibility Nothing u (tyVarName tv))
    u'   = incrUnique u
\end{code}

Monad stuff

\begin{code}
type NmbrM a = TyVarEnv Type -> Unique -> (Unique, a)		-- Unique is name supply

initNmbr :: TyVarEnv Type -> Unique -> NmbrM a -> a
initNmbr env uniq m
  = snd (m env uniq)

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
