%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprKind, pprParendKind,
	pprType, pprParendType,
	pprConstraint, pprPred, pprTheta,
	pprTyVarBndr, pprTyVarBndrs,

	-- Junk
	getTyDescription, showTypeCategory
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import TypeRep		( Type(..), TyNote(..), Kind, UsageAnn(..),
			  boxedTypeKind,
			)  -- friend
import Type		( PredType(..), ThetaType,
			  splitPredTy_maybe,
			  splitForAllTys, splitSigmaTy, splitRhoTy,
			  isDictTy, splitTyConApp_maybe, splitFunTy_maybe,
                          splitUsForAllTys
			)
import Var		( TyVar, tyVarKind,
			  tyVarName, setTyVarName
			)
import VarEnv
import TyCon		( TyCon, isPrimTyCon, isTupleTyCon, isUnboxedTupleTyCon, 
			  maybeTyConSingleCon, isEnumerationTyCon, 
			  tyConArity, tyConUnique
			)
import Class		( Class, className )

-- others:
import Maybes		( maybeToBool )
import Name		( getOccString, NamedThing(..) )
import Outputable
import PprEnv
import Unique		( Uniquable(..) )
import Unique		-- quite a few *Keys
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
pprType, pprParendType :: Type -> SDoc
pprType       ty = ppr_ty pprTyEnv tOP_PREC   ty
pprParendType ty = ppr_ty pprTyEnv tYCON_PREC ty

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

pprPred :: PredType -> SDoc
pprPred (Class clas tys) = pprConstraint clas tys
pprPred (IParam n ty)    = hsep [ppr n, ptext SLIT("::"), ppr ty]

pprConstraint :: Class -> [Type] -> SDoc
pprConstraint clas tys = ppr clas <+> hsep (map (pprParendType) tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (hsep (punctuate comma (map pprPred theta)))

instance Outputable Type where
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
ppr_ty :: PprEnv TyVar -> Int -> Type -> SDoc
ppr_ty env ctxt_prec (TyVarTy tyvar)
  = pTyVarO env tyvar

ppr_ty env ctxt_prec ty@(TyConApp tycon tys)
  	-- KIND CASE; it's of the form (Type x)
  | tycon `hasKey` typeConKey && n_tys == 1
  = 	-- For kinds, print (Type x) as just x if x is a 
	-- 	type constructor (must be Boxed, Unboxed, AnyBox)
	-- Otherwise print as (Type x)
    case ty1 of
	TyConApp bx [] -> ppr bx
	other	       -> maybeParen ctxt_prec tYCON_PREC 
				     (sep [ppr tycon, nest 4 tys_w_spaces])
		       
	
	-- TUPLE CASE (boxed and unboxed)
  |  isTupleTyCon tycon
  && length tys == tyConArity tycon	-- no magic if partially applied
  = parens tys_w_commas

  |  isUnboxedTupleTyCon tycon
  && length tys == tyConArity tycon	-- no magic if partially applied
  = parens (char '#' <+> tys_w_commas <+> char '#')

	-- LIST CASE
  | tycon `hasKey` listTyConKey && n_tys == 1
  = brackets (ppr_ty env tOP_PREC ty1)

	-- DICTIONARY CASE, prints {C a}
	-- This means that instance decls come out looking right in interfaces
	-- and that in turn means they get "gated" correctly when being slurped in
  | maybeToBool maybe_pred
  = braces (ppr_pred env pred)

	-- NO-ARGUMENT CASE (=> no parens)
  | null tys
  = ppr tycon

	-- GENERAL CASE
  | otherwise
  = maybeParen ctxt_prec tYCON_PREC (sep [ppr tycon, nest 4 tys_w_spaces])

  where
    n_tys      = length tys
    (ty1:_)    = tys
    Just pred  = maybe_pred
    maybe_pred = splitPredTy_maybe ty	-- Checks class and arity
    tys_w_commas = sep (punctuate comma (map (ppr_ty env tOP_PREC) tys))
    tys_w_spaces = sep (map (ppr_ty env tYCON_PREC) tys)
  


ppr_ty env ctxt_prec ty@(ForAllTy _ _)
  = getPprStyle $ \ sty -> 
    maybeParen ctxt_prec fUN_PREC $
    sep [ ptext SLIT("forall") <+> pp_tyvars <> ptext SLIT("."), 
	  ppr_theta theta,
	  ppr_ty env tOP_PREC tau
    ]
 where		
    (tyvars, rho) = splitForAllTys ty  -- don't treat theta specially any more (KSW 1999-04)
    (theta, tau)  = splitRhoTy rho
    
    pp_tyvars = hsep (map (pBndr env LambdaBind) tyvars)
    
    ppr_theta []	= empty
    ppr_theta theta     = parens (hsep (punctuate comma (map ppr_pred theta))) 
			  <+> ptext SLIT("=>")

    ppr_pred (Class clas tys) = ppr clas <+> hsep (map (ppr_ty env tYCON_PREC) tys)
    ppr_pred (IParam n ty)    = hsep [{- char '?' <> -} ppr n, text "::",
				      ppr_ty env tYCON_PREC ty]

ppr_ty env ctxt_prec (FunTy ty1 ty2)
  = maybeParen ctxt_prec fUN_PREC (sep (ppr_ty env fUN_PREC ty1 : pp_rest ty2))
  -- we don't want to lose usage annotations or synonyms,
  -- so we mustn't use splitFunTys here.
  where
    pp_rest (FunTy ty1 ty2) = pp_codom ty1 : pp_rest ty2
    pp_rest ty              = [pp_codom ty]
    pp_codom ty             = ptext SLIT("->") <+> ppr_ty env fUN_PREC ty

ppr_ty env ctxt_prec (AppTy ty1 ty2)
  = maybeParen ctxt_prec tYCON_PREC $
    ppr_ty env tOP_PREC ty1 <+> ppr_ty env tYCON_PREC ty2

ppr_ty env ctxt_prec (NoteTy (SynNote ty) expansion)
  = ppr_ty env ctxt_prec ty
--  = ppr_ty env ctxt_prec expansion -- if we don't want to see syntys

ppr_ty env ctxt_prec (NoteTy (FTVNote _) ty) = ppr_ty env ctxt_prec ty

ppr_ty env ctxt_prec ty@(NoteTy (UsgForAll _) _)
  = maybeParen ctxt_prec fUN_PREC $
    sep [ ptext SLIT("__fuall") <+> brackets pp_uvars <+> ptext SLIT("=>"),
          ppr_ty env tOP_PREC sigma
        ]
  where
    (uvars,sigma) = splitUsForAllTys ty
    pp_uvars      = hsep (map ppr uvars)

ppr_ty env ctxt_prec (NoteTy (UsgNote u) ty)
  = maybeParen ctxt_prec tYCON_PREC $
    ptext SLIT("__u") <+> ppr u <+> ppr_ty env tYCON_PREC ty

ppr_ty env ctxt_prec (NoteTy (IPNote nm) ty)
  = braces (ppr_pred env (IParam nm ty))

ppr_theta env []    = empty
ppr_theta env theta = braces (hsep (punctuate comma (map (ppr_pred env) theta)))

ppr_pred env (Class clas tys) = ppr clas <+>
				hsep (map (ppr_ty env tYCON_PREC) tys)
ppr_pred env (IParam n ty)    = hsep [char '?' <> ppr n, text "::",
				      ppr_ty env tYCON_PREC ty]

{-
ppr_dict env ctxt (clas, tys) = ppr clas <+> 
				hsep (map (ppr_ty env tYCON_PREC) tys)
-}
\end{code}

\begin{code}
pprTyEnv = initPprEnv b (Just ppr) b (Just (\site -> pprTyVarBndr)) b
  where
    b = panic "PprType:init_ppr_env"
\end{code}

\begin{code}
instance Outputable UsageAnn where
  ppr UsOnce     = ptext SLIT("-")
  ppr UsMany     = ptext SLIT("!")
  ppr (UsVar uv) = ppr uv
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
    if (ifaceStyle sty  && kind /= boxedTypeKind) || debugStyle sty then
        hsep [ppr tyvar, dcolon, pprParendKind kind]
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
      NoteTy (UsgNote _) ty  -> getTyDescription ty
      ForAllTy _ ty    -> getTyDescription ty
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other
\end{code}


\begin{code}
showTypeCategory :: Type -> Char
  {-
	{C,I,F,D}   char, int, float, double
	T	    tuple
	S	    other single-constructor type
	{c,i,f,d}   unboxed ditto
	t	    *unpacked* tuple
	s	    *unpacked" single-cons...

	v	    void#
	a	    primitive array

	E	    enumeration type
	+	    dictionary, unless it's a ...
	L	    List
	>	    function
	M	    other (multi-constructor) data-con type
	.	    other type
	-	    reserved for others to mark as "uninteresting"
    -}
showTypeCategory ty
  = if isDictTy ty
    then '+'
    else
      case splitTyConApp_maybe ty of
	Nothing -> if maybeToBool (splitFunTy_maybe ty)
		   then '>'
		   else '.'

	Just (tycon, _) ->
          let utc = getUnique tycon in
	  if	  utc == charDataConKey    then 'C'
	  else if utc == intDataConKey     then 'I'
	  else if utc == floatDataConKey   then 'F'
	  else if utc == doubleDataConKey  then 'D'
	  else if utc == smallIntegerDataConKey ||
		  utc == largeIntegerDataConKey   then 'J'
	  else if utc == charPrimTyConKey  then 'c'
	  else if (utc == intPrimTyConKey || utc == wordPrimTyConKey
		|| utc == addrPrimTyConKey)		   then 'i'
	  else if utc  == floatPrimTyConKey		   then 'f'
	  else if utc  == doublePrimTyConKey		   then 'd'
	  else if isPrimTyCon tycon {- array, we hope -}   then 'A'
	  else if isEnumerationTyCon tycon		   then 'E'
	  else if isTupleTyCon tycon			   then 'T'
	  else if maybeToBool (maybeTyConSingleCon tycon)  then 'S'
	  else if utc == listTyConKey			   then 'L'
	  else 'M' -- oh, well...
\end{code}
