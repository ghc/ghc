%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprKind, pprParendKind,
	pprType, pprParendType,
	pprPred, pprTheta, pprClassPred,
	pprTyVarBndr, pprTyVarBndrs,

	-- Junk
	getTyDescription, showTypeCategory
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import TypeRep		( Type(..), TyNote(..), Kind, liftedTypeKind ) -- friend
import Type		( PredType(..), ThetaType,
			  splitPredTy_maybe,
			  splitForAllTys, splitSigmaTy, splitRhoTy,
			  isPredTy, isDictTy, splitTyConApp_maybe, splitFunTy_maybe,
                          predRepTy, isUTyVar
			)
import Var		( TyVar, tyVarKind )
import Class		( Class )
import TyCon		( TyCon, isPrimTyCon, isTupleTyCon, tupleTyConBoxity,
			  maybeTyConSingleCon, isEnumerationTyCon, 
			  tyConArity, tyConName
			)

-- others:
import CmdLineOpts	( opt_PprStyle_RawTypes )
import Maybes		( maybeToBool )
import Name		( getOccString, getOccName )
import Outputable
import Unique		( Uniquable(..) )
import BasicTypes	( tupleParens )
import PrelNames		-- quite a few *Keys
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
pprType       ty = ppr_ty tOP_PREC   ty
pprParendType ty = ppr_ty tYCON_PREC ty

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

pprPred :: PredType -> SDoc
pprPred (ClassP clas tys) = pprClassPred clas tys
pprPred (IParam n ty)     = hsep [ptext SLIT("?") <> ppr n,
				  ptext SLIT("::"), ppr ty]

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = ppr clas <+> hsep (map pprParendType tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (hsep (punctuate comma (map pprPred theta)))

instance Outputable Type where
    ppr ty = pprType ty

instance Outputable PredType where
    ppr = pprPred
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
tOP_PREC    = (0 :: Int)  -- type   in ParseIface.y
fUN_PREC    = (1 :: Int)  -- btype  in ParseIface.y
tYCON_PREC  = (2 :: Int)  -- atype  in ParseIface.y

maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty
\end{code}

\begin{code}
ppr_ty :: Int -> Type -> SDoc
ppr_ty ctxt_prec (TyVarTy tyvar)
  = ppr tyvar

ppr_ty ctxt_prec ty@(TyConApp tycon tys)
  	-- KIND CASE; it's of the form (Type x)
  | tycon `hasKey` typeConKey && n_tys == 1
  = 	-- For kinds, print (Type x) as just x if x is a 
	-- 	type constructor (must be Boxed, Unboxed, AnyBox)
	-- Otherwise print as (Type x)
    case ty1 of
	TyConApp bx [] -> ppr (getOccName bx)	-- Always unqualified
	other	       -> maybeParen ctxt_prec tYCON_PREC 
				     (sep [ppr tycon, nest 4 tys_w_spaces])

	-- USAGE CASE
  | (tycon `hasKey` usOnceTyConKey || tycon `hasKey` usManyTyConKey) && n_tys == 0
  =	-- For usages (! and .), always print bare OccName, without pkg/mod/uniq
    ppr (getOccName (tyConName tycon))
	
	-- TUPLE CASE (boxed and unboxed)
  |  isTupleTyCon tycon
  && length tys == tyConArity tycon	-- no magic if partially applied
  = tupleParens (tupleTyConBoxity tycon) tys_w_commas

	-- LIST CASE
  | tycon `hasKey` listTyConKey && n_tys == 1
  = brackets (ppr_ty tOP_PREC ty1)

	-- DICTIONARY CASE, prints {C a}
	-- This means that instance decls come out looking right in interfaces
	-- and that in turn means they get "gated" correctly when being slurped in
  | maybeToBool maybe_pred
  = braces (pprPred pred)

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
    tys_w_commas = sep (punctuate comma (map (ppr_ty tOP_PREC) tys))
    tys_w_spaces = sep (map (ppr_ty tYCON_PREC) tys)
  


ppr_ty ctxt_prec ty@(ForAllTy _ _)
  = getPprStyle $ \ sty -> 
    maybeParen ctxt_prec fUN_PREC $
    sep [ ptext SLIT("forall") <+> pp_tyvars sty <> ptext SLIT("."), 
	  ppr_theta theta,
	  ppr_ty tOP_PREC tau
    ]
 where		
    (tyvars, rho) = splitForAllTys ty
    (theta, tau)  = splitRhoTy rho
    
    pp_tyvars sty = hsep (map pprTyVarBndr some_tyvars)
      where
        some_tyvars | userStyle sty && not opt_PprStyle_RawTypes
                    = filter (not . isUTyVar) tyvars  -- hide uvars from user
                    | otherwise
                    = tyvars
    
    ppr_theta []     = empty
    ppr_theta theta  = pprTheta theta <+> ptext SLIT("=>")


ppr_ty ctxt_prec (FunTy ty1 ty2)
  -- we don't want to lose usage annotations or synonyms,
  -- so we mustn't use splitFunTys here.
  = maybeParen ctxt_prec fUN_PREC $
    sep [ ppr_ty fUN_PREC ty1
        , ptext arrow <+> ppr_ty tOP_PREC ty2
        ]
  where arrow | isPredTy ty1 = SLIT("=>")
	      | otherwise    = SLIT("->")

ppr_ty ctxt_prec (AppTy ty1 ty2)
  = maybeParen ctxt_prec tYCON_PREC $
    ppr_ty fUN_PREC ty1 <+> ppr_ty tYCON_PREC ty2

ppr_ty ctxt_prec (UsageTy u ty)
  = maybeParen ctxt_prec tYCON_PREC $
    ptext SLIT("__u") <+> ppr_ty tYCON_PREC u
                      <+> ppr_ty tYCON_PREC ty
    -- fUN_PREC would be logical for u, but it yields a reduce/reduce conflict with AppTy

ppr_ty ctxt_prec (NoteTy (SynNote ty) expansion)
  = ppr_ty ctxt_prec ty
--  = ppr_ty ctxt_prec expansion -- if we don't want to see syntys

ppr_ty ctxt_prec (NoteTy (FTVNote _) ty) = ppr_ty ctxt_prec ty

ppr_ty ctxt_prec (PredTy p) = braces (pprPred p)
\end{code}


%************************************************************************
%*									*
\subsection[TyVar]{@TyVar@}
%*									*
%************************************************************************

We print type-variable binders with their kinds in interface files,
and when in debug mode.

\begin{code}
pprTyVarBndr :: TyVar -> SDoc
pprTyVarBndr tyvar
  = getPprStyle $ \ sty ->
    if (ifaceStyle sty  && kind /= liftedTypeKind) || debugStyle sty then
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
      PredTy p		     -> getTyDescription (predRepTy p)
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
	  else if isPrimTyCon tycon {- array, we hope -}   then 'A'	-- Bogus
	  else if isEnumerationTyCon tycon		   then 'E'
	  else if isTupleTyCon tycon			   then 'T'
	  else if maybeToBool (maybeTyConSingleCon tycon)  then 'S'
	  else if utc == listTyConKey			   then 'L'
	  else 'M' -- oh, well...
\end{code}
