%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
module TcDefaults ( tcDefaults, defaultDefaultTys ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), DefaultDecl(..) )
import RnHsSyn		( RenamedHsDecl )

import TcMonad
import TcEnv		( tcLookupGlobal_maybe )
import TcMonoType	( tcHsType )
import TcSimplify	( tcSimplifyDefault )

import TysWiredIn	( integerTy, doubleTy )
import TcType           ( Type, mkClassPred, isTauTy )
import PrelNames	( numClassName )
import Outputable
import HscTypes		( TyThing(..) )
\end{code}

\begin{code}
defaultDefaultTys = [integerTy, doubleTy]

tcDefaults :: [RenamedHsDecl]
	   -> TcM [Type] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.
tcDefaults decls = tc_defaults [default_decl | DefD default_decl <- decls]

tc_defaults [] = returnTc defaultDefaultTys

tc_defaults [DefaultDecl [] locn]
  = returnTc []		-- no defaults

tc_defaults [DefaultDecl mono_tys locn]
  = tcLookupGlobal_maybe numClassName	`thenNF_Tc` \ maybe_num ->
    case maybe_num of
	Just (AClass num_class) -> common_case num_class
	other	          	-> returnTc []
		-- In the Nothing case, Num has not been sucked in, so the 
		-- defaults will never be used; so simply discard the default decl.
		-- This slightly benefits modules that don't use any
		-- numeric stuff at all, by avoid the necessity of
		-- always sucking in Num
  where
    common_case num_class
      = tcAddSrcLoc locn 		$
    	tcAddErrCtxt defaultDeclCtxt	$
    	mapTc tc_default_ty mono_tys	`thenTc` \ tau_tys ->
    
 		-- Check that all the types are instances of Num
 		-- We only care about whether it worked or not
    	tcSimplifyDefault [mkClassPred num_class [ty] | ty <- tau_tys]	`thenTc_`
    
    	returnTc tau_tys

tc_defaults decls@(DefaultDecl _ loc : _) =
    tcAddSrcLoc loc $
    failWithTc (dupDefaultDeclErr decls)


tc_default_ty hs_ty 
 = tcHsType hs_ty				`thenTc` \ ty ->
   checkTc (isTauTy ty) (polyDefErr hs_ty)	`thenTc_`
   returnTc ty

defaultDeclCtxt =  ptext SLIT("when checking that each type in a default declaration")
		    $$ ptext SLIT("is an instance of class Num")


dupDefaultDeclErr (DefaultDecl _ locn1 : dup_things)
  = hang (ptext SLIT("Multiple default declarations"))
      4  (vcat (map pp dup_things))
  where
    pp (DefaultDecl _ locn) = ptext SLIT("here was another default declaration") <+> ppr locn

polyDefErr ty 
  = hang (ptext SLIT("Illegal polymorphic type in default declaration") <> colon) 4 (ppr ty) 
\end{code}

