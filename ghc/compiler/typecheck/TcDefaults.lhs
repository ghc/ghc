%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
module TcDefaults ( tcDefaults ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), DefaultDecl(..) )
import RnHsSyn		( RenamedHsDecl )

import TcMonad
import TcEnv		( tcLookupClassByKey_maybe )
import TcMonoType	( tcHsType )
import TcSimplify	( tcSimplifyCheckThetas )

import TysWiredIn	( integerTy, doubleTy )
import Type             ( Type )
import Unique		( numClassKey )
import ErrUtils		( addShortErrLocLine )
import Outputable
import Util
\end{code}

\begin{code}
default_default = [integerTy, doubleTy]

tcDefaults :: [RenamedHsDecl]
	   -> TcM s [Type] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.
tcDefaults decls = tc_defaults [default_decl | DefD default_decl <- decls]

tc_defaults [] = returnTc default_default

tc_defaults [DefaultDecl [] locn]
  = returnTc []		-- no defaults

tc_defaults [DefaultDecl mono_tys locn]
  = tcLookupClassByKey_maybe numClassKey	`thenNF_Tc` \ maybe_num ->
    case maybe_num of {
	Nothing -> 	-- Num has not been sucked in, so the defaults will
			-- never be used; so simply discard the default decl.
			-- This slightly benefits modules that don't use any
			-- numeric stuff at all, by avoid the necessity of
			-- always sucking in Num
		returnTc [] ;

	Just num ->	-- The common case

    tcAddSrcLoc locn $
    mapTc tcHsType mono_tys	`thenTc` \ tau_tys ->

	    -- Check that all the types are instances of Num
	    -- We only care about whether it worked or not
    tcAddErrCtxt defaultDeclCtxt		$
    tcSimplifyCheckThetas
		[{- Nothing given -}]
		[ (num, [ty]) | ty <- tau_tys ]	`thenTc_`

    returnTc tau_tys
    }

tc_defaults decls@(DefaultDecl _ loc : _) =
    tcAddSrcLoc loc $
    failWithTc (dupDefaultDeclErr decls)


defaultDeclCtxt =  ptext SLIT("when checking that each type in a default declaration")
		    $$ ptext SLIT("is an instance of class Num")


dupDefaultDeclErr (DefaultDecl _ locn1 : dup_things)
  = hang (ptext SLIT("Multiple default declarations"))
      4  (vcat (map pp dup_things))
  where
    pp (DefaultDecl _ locn) = ptext SLIT("here was another default declaration") <+> ppr locn
\end{code}

