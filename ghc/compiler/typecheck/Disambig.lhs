%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
%************************************************************************
%*									*
\section[Disambig]{Disambiguation of overloading}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module Disambig (
	disambiguateDicts,

	-- and for self-sufficiency...
	Inst, Subst, UniqueSupply, Bag, Error(..), SrcLoc,
	TcResult, Pretty(..), PprStyle, PrettyRep
    ) where

import TcMonad
import AbsSyn

import AbsPrel		( intTyCon, intTy, {-ToDo:?voidTy,-} doubleTyCon )
import AbsUniType	( applyTyCon, getTyVar, cmpTyVar, getClassKey,
			  isNumericClass, isStandardClass
			)
import Errors		( ambigErr, defaultErr, Error(..), UnifyErrContext(..) )
import Id		( Id, DictVar(..) )
import Inst		--( Inst(..), InstOrigin(..), OverloadedLit  )
import InstEnv		( lookupClassInstAtSimpleType )
import Maybes		( Maybe(..), firstJust )
import SrcLoc		( mkUnknownSrcLoc )
import TcSimplify	( tcSimplifyCheckThetas )
import Unique		( cReturnableClassKey )
import Util
\end{code}

If a dictionary constrains a type variable which is
\begin{itemize}
\item
not mentioned in the environment
\item
and not mentioned in the type of the expression
\end{itemize}
then it is ambiguous. No further information will arise to instantiate
the type variable; nor will it be generalised and turned into an extra
parameter to a function.

It is an error for this to occur, except that Haskell provided for
certain rules to be applied in the special case of numeric types.

Specifically, if
\begin{itemize}
\item
at least one of its classes is a numeric class, and
\item
all of its classes are numeric or standard
\end{itemize}
then the type variable can be defaulted to the first type in the
default-type list which is an instance of all the offending classes.

So here is the function which does the work.  It takes the ambiguous
dictionaries and either resolves them (producing bindings) or
complains.  It works by splitting the dictionary list by type
variable, and using @disambigOne@ to do the real business.

IMPORTANT: @disambiguate@ assumes that its argument dictionaries
constrain only a simple type variable.

\begin{code}
type SimpleDictInfo = (Inst, Class, TyVar)

disambiguateDicts :: [Inst] -> TcM ()

disambiguateDicts insts
  = mapTc disambigOne inst_infos    `thenTc` \ binds_lists ->
    returnTc ()
  where
    inst_infos = equivClasses cmp_tyvars (map mk_inst_info insts)
    (_,_,tv1) `cmp_tyvars` (_,_,tv2) = tv1 `cmpTyVar` tv2
  
    mk_inst_info dict@(Dict _ clas ty _)
      = (dict, clas, getTyVar "disambiguateDicts" ty)
\end{code}

@disambigOne@ assumes that its arguments dictionaries constrain all
the same type variable.

ADR Comment 20/6/94: I've changed the @CReturnable@ case to default to
@()@ instead of @Int@.  I reckon this is the Right Thing to do since
the most common use of defaulting is code like:
\begin{verbatim}
	_ccall_ foo	`seqPrimIO` bar
\end{verbatim}
Since we're not using the result of @foo@, the result if (presumably)
@void@.
WDP Comment: no such thing as voidTy; so not quite in yet (94/07).

\begin{code}
disambigOne :: [SimpleDictInfo] -> TcM ()

disambigOne dict_infos
  | isCReturnable dict_infos
   	-- C-returnable; just default to Void
  =  extendSubstTc tyvar intTy{-ToDo:voidTy-} (AmbigDictCtxt dicts)

  | not (isStandardNumericDefaultable dict_infos)
  = failTc (ambigErr dicts) -- no default

  | otherwise -- isStandardNumericDefaultable dict_infos
  = 	-- THE DICTS OBEY THE DEFAULTABLE CONSTRAINT
	-- SO, TRY DEFAULT TYPES IN ORDER

	-- Failure here is caused by there being no type in the
	-- default list which can satisfy all the ambiguous classes.
	-- For example, if Real a is reqd, but the only type in the
	-- default list is Int.
    getDefaultingTys		    `thenNF_Tc` \ default_tys ->

    mapNF_Tc try_default default_tys `thenNF_Tc` \ maybe_tys ->

    checkMaybeTc (firstJust maybe_tys)
		 (defaultErr dicts default_tys)
				    `thenTc` \ chosen_default_ty ->

	-- SUCCESS; COMBINE TO A BINDS, AND EXTEND SUBSTITUTION
    extendSubstTc tyvar chosen_default_ty (AmbigDictCtxt dicts)

  where
    (_,_,tyvar) = head dict_infos		-- Should be non-empty
    dicts = [dict | (dict,_,_) <- dict_infos]

    try_default :: UniType -> NF_TcM (Maybe UniType)

    try_default default_ty
      = let
	    thetas = [(clas, default_ty) | (_,clas,_) <- dict_infos]
        in
	recoverQuietlyTc Nothing ( -- if tcSimplify hates us, we get the Nothing

	    tcSimplifyCheckThetas (DefaultDeclOrigin mkUnknownSrcLoc) thetas `thenTc` \ _ ->
	    returnTc (Just default_ty)
	)
\end{code}

@isStandardNumericDefaultable@ sees whether the dicts have the
property required for defaulting; namely at least one is numeric, and
all are standard.

\begin{code}
isCReturnable, isStandardNumericDefaultable :: [SimpleDictInfo] -> Bool

isStandardNumericDefaultable dict_infos
  =    (any (\ (_,c,_) -> isNumericClass c)  dict_infos)
    && (all (\ (_,c,_) -> isStandardClass c) dict_infos)

isCReturnable [(_,c,_)] = getClassKey c == cReturnableClassKey
isCReturnable _	        = False -- duplicates will have been removed,
				-- so we don't have to worry about
				-- multiple copies of cReturnableClassKey...
\end{code}
