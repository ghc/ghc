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
import Class
import TcRnMonad
import TcEnv
import TcHsType
import TcSimplify
import TcType
import PrelNames
import DynFlags
import SrcLoc
import Maybe
import Outputable
\end{code}

\begin{code}
tcDefaults :: [LDefaultDecl Name]
	   -> TcM (Maybe [Type])    -- Defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.

tcDefaults [] 
  = getDeclaredDefaultTys	-- No default declaration, so get the
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
    do	{ ovl_str <- doptM Opt_OverloadedStrings
	; num_class    <- tcLookupClass numClassName
	; is_str_class <- tcLookupClass isStringClassName
	; let deflt_clss | ovl_str   = [num_class, is_str_class]
			 | otherwise = [num_class]

	; tau_tys <- mappM (tc_default_ty deflt_clss) mono_tys
    
	; return (Just tau_tys) }

tcDefaults decls@(L locn (DefaultDecl _) : _)
  = setSrcSpan locn $
    failWithTc (dupDefaultDeclErr decls)


tc_default_ty deflt_clss hs_ty 
 = do	{ ty <- tcHsSigType DefaultDeclCtxt hs_ty
	; checkTc (isTauTy ty) (polyDefErr hs_ty)

	-- Check that the type is an instance of at least one of the deflt_clss
	; oks <- mapM (check_instance ty) deflt_clss
	; checkTc (or oks) (badDefaultTy ty deflt_clss)
	; return ty }

check_instance :: Type -> Class -> TcM Bool
  -- Check that ty is an instance of cls
  -- We only care about whether it worked or not; return a boolean
check_instance ty cls
  = do	{ (_, mb_res) <- tryTc (tcSimplifyDefault [mkClassPred cls [ty]])
	; return (isJust mb_res) }
    
defaultDeclCtxt = ptext SLIT("When checking the types in a default declaration")

dupDefaultDeclErr (L _ (DefaultDecl _) : dup_things)
  = hang (ptext SLIT("Multiple default declarations"))
      4  (vcat (map pp dup_things))
  where
    pp (L locn (DefaultDecl _)) = ptext SLIT("here was another default declaration") <+> ppr locn

polyDefErr ty 
  = hang (ptext SLIT("Illegal polymorphic type in default declaration") <> colon) 4 (ppr ty) 

badDefaultTy ty deflt_clss
  = hang (ptext SLIT("The default type") <+> quotes (ppr ty) <+> ptext SLIT("is not an instance of"))
       2 (foldr1 (\a b -> a <+> ptext SLIT("or") <+> b) (map (quotes. ppr) deflt_clss))
\end{code}

