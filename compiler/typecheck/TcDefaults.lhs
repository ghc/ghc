%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
module TcDefaults ( tcDefaults ) where

#include "HsVersions.h"

import HsSyn
import Name
import TcRnMonad
import TcEnv
import TcHsType
import TcSimplify
import TcType
import PrelNames
import SrcLoc
import Outputable
\end{code}

\begin{code}
tcDefaults :: [LDefaultDecl Name]
	   -> TcM (Maybe [Type])    -- Defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.

tcDefaults [] 
  = getDefaultTys		-- No default declaration, so get the
				-- default types from the envt; 
				-- i.e. use the curent ones
				-- (the caller will put them back there)
	-- It's important not to return defaultDefaultTys here (which
	-- we used to do) because in a TH program, tcDefaults [] is called
	-- repeatedly, once for each group of declarations between top-level
	-- splices.  We don't want to carefully set the default types in
	-- one group, only for the next group to ignore them and install
	-- defaultDefaultTys

tcDefaults [L locn (DefaultDecl [])]
  = returnM (Just [])		-- Default declaration specifying no types

tcDefaults [L locn (DefaultDecl mono_tys)]
  = setSrcSpan locn 			$
    addErrCtxt defaultDeclCtxt		$
    tcLookupClass numClassName		`thenM` \ num_class ->
    tcLookupClass isStringClassName		`thenM` \ num_class ->
    mappM tc_default_ty mono_tys	`thenM` \ tau_tys ->
    
    	-- Check that all the types are instances of Num
    	-- We only care about whether it worked or not
    tcSimplifyDefault [mkClassPred num_class [ty] | ty <- tau_tys]	`thenM_`
    
    returnM (Just tau_tys)

tcDefaults decls@(L locn (DefaultDecl _) : _) =
    setSrcSpan locn $
    failWithTc (dupDefaultDeclErr decls)


tc_default_ty hs_ty 
 = tcHsSigType DefaultDeclCtxt hs_ty		`thenM` \ ty ->
   checkTc (isTauTy ty) (polyDefErr hs_ty)	`thenM_`
   returnM ty

defaultDeclCtxt =  ptext SLIT("when checking that each type in a default declaration")
		    $$ ptext SLIT("is an instance of class Num")


dupDefaultDeclErr (L _ (DefaultDecl _) : dup_things)
  = hang (ptext SLIT("Multiple default declarations"))
      4  (vcat (map pp dup_things))
  where
    pp (L locn (DefaultDecl _)) = ptext SLIT("here was another default declaration") <+> ppr locn

polyDefErr ty 
  = hang (ptext SLIT("Illegal polymorphic type in default declaration") <> colon) 4 (ppr ty) 
\end{code}

