%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
module TcDefaults ( tcDefaults ) where

#include "HsVersions.h"

import HsSyn		( DefaultDecl(..) )
import Name		( Name )
import TcRnMonad
import TcEnv		( tcLookupGlobal_maybe )
import TcMonoType	( tcHsType )
import TcSimplify	( tcSimplifyDefault )
import TcType           ( Type, mkClassPred, isTauTy )
import PrelNames	( numClassName )
import Outputable
import HscTypes		( TyThing(..) )
\end{code}

\begin{code}
tcDefaults :: [DefaultDecl Name]
	   -> TcM [Type] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.

tcDefaults [] = returnM defaultDefaultTys

tcDefaults [DefaultDecl [] locn]
  = returnM []		-- no defaults

tcDefaults [DefaultDecl mono_tys locn]
  = tcLookupGlobal_maybe numClassName	`thenM` \ maybe_num ->
    case maybe_num of
	Just (AClass num_class) -> common_case num_class
	other	          	-> returnM []
		-- In the Nothing case, Num has not been sucked in, so the 
		-- defaults will never be used; so simply discard the default decl.
		-- This slightly benefits modules that don't use any
		-- numeric stuff at all, by avoid the necessity of
		-- always sucking in Num
  where
    common_case num_class
      = addSrcLoc locn 		$
    	addErrCtxt defaultDeclCtxt	$
    	mappM tc_default_ty mono_tys	`thenM` \ tau_tys ->
    
 		-- Check that all the types are instances of Num
 		-- We only care about whether it worked or not
    	tcSimplifyDefault [mkClassPred num_class [ty] | ty <- tau_tys]	`thenM_`
    
    	returnM tau_tys

tc_defaults decls@(DefaultDecl _ loc : _) =
    addSrcLoc loc $
    failWithTc (dupDefaultDeclErr decls)


tc_default_ty hs_ty 
 = tcHsType hs_ty				`thenM` \ ty ->
   checkTc (isTauTy ty) (polyDefErr hs_ty)	`thenM_`
   returnM ty

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

