%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[PprType]{Printing Types, TyVars, Classes, TyCons}

\begin{code}
module PprType(
	pprKind, pprParendKind,
	pprType, pprParendType,
	pprPred, pprTheta, pprThetaArrow, pprClassPred,
	pprTyVarBndr, pprTyVarBndrs,

	-- Junk
	getTyDescription, showTypeCategory
 ) where

#include "HsVersions.h"

-- friends:
-- (PprType can see all the representations it's trying to print)
import TypeRep		( Type(..), TyNote(..), PredType(..), TyThing(..), Kind, superKind  ) -- friend
import Type		( typeKind, eqKind )
import IfaceType	( toIfaceType, toIfacePred, pprParendIfaceType,
		 	  toIfaceKind, pprParendIfaceKind,
			  getIfaceExt ) 

import TcType		( ThetaType, PredType, 
			  tcSplitSigmaTy, isDictTy,
			  tcSplitTyConApp_maybe, tcSplitFunTy_maybe
			) 
import Var		( TyVar, tyVarKind )
import Class		( Class )
import TyCon		( isPrimTyCon, isTupleTyCon, maybeTyConSingleCon, isEnumerationTyCon )

-- others:
import Maybes		( maybeToBool )
import Name		( NamedThing(..), getOccString )
import Outputable
import BasicTypes	( IPName(..), ipNameName )
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
-- To save duplicating type-printing machinery, 
-- we print a type by converting to an IfaceType and printing that
pprType       ty = getIfaceExt $ \ ext ->
		   ppr (toIfaceType ext ty)
pprParendType ty = getIfaceExt $ \ ext ->
		   pprParendIfaceType (toIfaceType ext ty)

pprKind, pprParendKind :: Kind -> SDoc
pprKind       k = ppr (toIfaceKind k)
pprParendKind k = pprParendIfaceKind (toIfaceKind k)

pprPred :: PredType -> SDoc
pprPred pred = getIfaceExt $ \ ext ->
	       ppr (toIfacePred ext pred)

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = ppr clas <+> sep (map pprParendType tys)

pprTheta :: ThetaType -> SDoc
pprTheta theta = parens (sep (punctuate comma (map pprPred theta)))

pprThetaArrow :: ThetaType -> SDoc
pprThetaArrow theta 
  | null theta = empty
  | otherwise  = parens (sep (punctuate comma (map pprPred theta))) <+> ptext SLIT("=>")

instance Outputable Type where
    ppr ty | typeKind ty `eqKind` superKind = pprKind ty
	   | otherwise		            = pprType ty

instance Outputable PredType where
    ppr = pprPred

instance Outputable name => OutputableBndr (IPName name) where
    pprBndr _ n = ppr n	-- Simple for now

instance Outputable TyThing where
  ppr (AnId   id)   = ptext SLIT("AnId")     <+> ppr id
  ppr (ATyCon tc)   = ptext SLIT("ATyCon")   <+> ppr tc
  ppr (AClass cl)   = ptext SLIT("AClass")   <+> ppr cl
  ppr (ADataCon dc) = ptext SLIT("ADataCon") <+> ppr dc

instance NamedThing TyThing where	-- Can't put this with the type
  getName (AnId id)     = getName id	-- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc	-- isn't visible there
  getName (AClass cl)   = getName cl
  getName (ADataCon dc) = getName dc
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
      NewTcApp tycon _ 	     -> getOccString tycon
      TyConApp tycon _ 	     -> getOccString tycon
      NoteTy (FTVNote _) ty  -> getTyDescription ty
      NoteTy (SynNote ty1) _ -> getTyDescription ty1
      PredTy sty	     -> getPredTyDescription sty
      ForAllTy _ ty          -> getTyDescription ty
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other

getPredTyDescription (ClassP cl tys) = getOccString cl
getPredTyDescription (IParam ip ty)  = getOccString (ipNameName ip)
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
