%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprKind, pprParendKind,
	pprType, pprParendType,
	pprSourceType, pprPred, pprTheta, pprClassPred,
	pprTyVarBndr, pprTyVarBndrs,

	-- Junk
	getTyDescription, showTypeCategory
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import TypeRep		( Type(..), TyNote(..), Kind  ) -- friend
import Type		( SourceType(..) ) 
import TcType		( ThetaType, PredType, TyThing(..),
			  tcSplitSigmaTy, isPredTy, isDictTy,
			  tcSplitTyConApp_maybe, tcSplitFunTy_maybe
			) 
import Var		( TyVar, tyVarKind )
import Class		( Class )
import TyCon		( TyCon, isPrimTyCon, isTupleTyCon, tupleTyConBoxity,
			  maybeTyConSingleCon, isEnumerationTyCon, tyConArity
			)

-- others:
import Maybes		( maybeToBool )
import Name		( getOccString, getOccName )
import OccName		( occNameUserString )
import Outputable
import Unique		( Uniquable(..) )
import Util             ( lengthIs )
import BasicTypes	( IPName(..), tupleParens, ipNameName )
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
pprPred = pprSourceType

pprSourceType :: SourceType -> SDoc
pprSourceType (ClassP clas tys) = pprClassPred clas tys
pprSourceType (IParam n ty)     = hsep [ppr n, dcolon, ppr ty]
pprSourceType (NType tc tys)    = ppr tc <+> sep (map pprParendType tys)

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = ppr clas <+> sep (map pprParendType tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (sep (punctuate comma (map pprPred theta)))

instance Outputable Type where
    ppr ty = pprType ty

instance Outputable SourceType where
    ppr = pprPred

instance Outputable name => Outputable (IPName name) where
    ppr (Dupable n) = char '?' <> ppr n -- Ordinary implicit parameters
    ppr (Linear  n) = char '%' <> ppr n -- Splittable implicit parameters

instance Outputable name => OutputableBndr (IPName name) where
    pprBndr _ n = ppr n	-- Simple for now

instance Outputable TyThing where
  ppr (AnId   id)   = ptext SLIT("AnId")     <+> ppr id
  ppr (ATyCon tc)   = ptext SLIT("ATyCon")   <+> ppr tc
  ppr (AClass cl)   = ptext SLIT("AClass")   <+> ppr cl
  ppr (ADataCon dc) = ptext SLIT("ADataCon") <+> ppr dc
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
  | tycon `hasKey` typeConKey,
    [ty] <- tys
  = 	-- For kinds, print (Type x) as just x if x is a 
	-- 	type constructor (must be Boxed, Unboxed, AnyBox)
	-- Otherwise print as (Type x)
    case ty of
	TyConApp bx [] -> ppr (getOccName bx)	-- Always unqualified
	other	       -> maybeParen ctxt_prec tYCON_PREC 
				     (ppr tycon <+> ppr_ty tYCON_PREC ty)

	-- TUPLE CASE (boxed and unboxed)
  |  isTupleTyCon tycon,
      tys `lengthIs` tyConArity tycon 	-- No magic if partially applied
  = tupleParens (tupleTyConBoxity tycon)
		(sep (punctuate comma (map (ppr_ty tOP_PREC) tys)))

	-- LIST CASE
  | tycon `hasKey` listTyConKey,
    [ty] <- tys
  = brackets (ppr_ty tOP_PREC ty)

	-- PARALLEL ARRAY CASE
  | tycon `hasKey` parrTyConKey,
    [ty] <- tys
  = pabrackets (ppr_ty tOP_PREC ty)

	-- GENERAL CASE
  | otherwise
  = ppr_tc_app ctxt_prec tycon tys

  where
    pabrackets p = ptext SLIT("[:") <> p <> ptext SLIT(":]")


ppr_ty ctxt_prec ty@(ForAllTy _ _)
  = getPprStyle $ \ sty -> 
    maybeParen ctxt_prec fUN_PREC $
    sep [ ptext SLIT("forall") <+> pp_tyvars sty <> ptext SLIT("."), 
	  ppr_theta theta,
	  ppr_ty tOP_PREC tau
    ]
 where		
    (tyvars, theta, tau) = tcSplitSigmaTy ty
    pp_tyvars sty	 = sep (map pprTyVarBndr tyvars)
    
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

ppr_ty ctxt_prec (NoteTy (SynNote ty) expansion)
  = ppr_ty ctxt_prec ty
--  = ppr_ty ctxt_prec expansion -- if we don't want to see syntys

ppr_ty ctxt_prec (NoteTy (FTVNote _) ty) = ppr_ty ctxt_prec ty

ppr_ty ctxt_prec (SourceTy (NType tc tys)) = ppr_tc_app ctxt_prec tc tys
ppr_ty ctxt_prec (SourceTy pred)	   = braces (pprPred pred)

ppr_tc_app ctxt_prec tc []  = ppr tc
ppr_tc_app ctxt_prec tc tys = maybeParen ctxt_prec tYCON_PREC 
			  		 (sep [ppr tc, nest 4 (sep (map (ppr_ty tYCON_PREC) tys))])
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
    if debugStyle sty then
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
  = case (tcSplitSigmaTy ty) of { (_, _, tau_ty) ->
    case tau_ty of
      TyVarTy _	       	     -> "*"
      AppTy fun _      	     -> getTyDescription fun
      FunTy _ res      	     -> '-' : '>' : fun_result res
      TyConApp tycon _ 	     -> occNameUserString (getOccName tycon)
      NoteTy (FTVNote _) ty  -> getTyDescription ty
      NoteTy (SynNote ty1) _ -> getTyDescription ty1
      SourceTy sty	     -> getSourceTyDescription sty
      ForAllTy _ ty          -> getTyDescription ty
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other

getSourceTyDescription (ClassP cl tys) = getOccString cl
getSourceTyDescription (NType  tc tys) = getOccString tc
getSourceTyDescription (IParam ip ty)  = getOccString (ipNameName ip)
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
      case tcSplitTyConApp_maybe ty of
	Nothing -> if maybeToBool (tcSplitFunTy_maybe ty)
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
