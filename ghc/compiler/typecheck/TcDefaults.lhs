%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[TcDefaults]{Typechecking \tr{default} declarations}

\begin{code}
#include "HsVersions.h"

module TcDefaults ( tcDefaults ) where

IMP_Ubiq()

import HsSyn		( HsDecl(..), TyDecl, ClassDecl, InstDecl, HsBinds,
			  DefaultDecl(..), HsType, IfaceSig,
			  HsExpr, HsLit, ArithSeqInfo, Fake, InPat)
import RnHsSyn		( RenamedHsDecl(..), RenamedDefaultDecl(..) )
import TcHsSyn		( TcIdOcc )

import TcMonad
import Inst		( InstOrigin(..) )
import TcEnv		( tcLookupClassByKey )
import SpecEnv		( SpecEnv )
import TcMonoType	( tcHsType )
import TcSimplify	( tcSimplifyCheckThetas )

import TysWiredIn	( intTy, doubleTy, unitTy )
import Type             ( SYN_IE(Type) )
import Unique		( numClassKey )
import Pretty		( ptext, vcat )
import ErrUtils		( addShortErrLocLine )
import Util
\end{code}

\begin{code}
default_default = [intTy, doubleTy] 	    -- language-specified default `default'

tcDefaults :: [RenamedHsDecl]
	   -> TcM s [Type] 	    -- defaulting types to heave
				    -- into Tc monad for later use
				    -- in Disambig.
tcDefaults decls = tc_defaults [default_decl | DefD default_decl <- decls]

tc_defaults [] = returnTc default_default

tc_defaults [DefaultDecl mono_tys locn]
  = tcAddSrcLoc locn $
    mapTc tcHsType mono_tys	`thenTc` \ tau_tys ->

    case tau_tys of
      [] -> returnTc []		-- no defaults

      _  ->
	    -- Check that all the types are instances of Num
	    -- We only care about whether it worked or not

	tcLookupClassByKey numClassKey			`thenNF_Tc` \ num ->
	tcSimplifyCheckThetas
		[ (num, ty) | ty <- tau_tys ]		`thenTc_`

	returnTc tau_tys

tc_defaults decls
  = failTc (dupDefaultDeclErr decls)


dupDefaultDeclErr (DefaultDecl _ locn1 : dup_things) sty
  = vcat (item1 : map dup_item dup_things)
  where
    item1
      = addShortErrLocLine locn1 (\ sty ->
	ptext SLIT("multiple default declarations")) sty

    dup_item (DefaultDecl _ locn)
      = addShortErrLocLine locn (\ sty ->
	ptext SLIT("here was another default declaration")) sty

\end{code}
