%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSplice]{Template Haskell splices}

\begin{code}
module TcSplice( tcSpliceExpr, tcSpliceDecls, tcBracket ) where

#include "HsVersions.h"

import HscMain		( compileExpr )
import TcRnDriver	( importSupportingDecls, tcTopSrcDecls )
	-- These imports are the reason that TcSplice 
	-- is very high up the module hierarchy

import qualified Language.Haskell.THSyntax as Meta

import HscTypes		( HscEnv(..), GhciMode(..), PersistentCompilerState(..), unQualInScope )
import HsSyn		( HsBracket(..) )
import Convert		( convertToHsExpr, convertToHsDecls )
import RnExpr		( rnExpr )
import RdrHsSyn		( RdrNameHsExpr, RdrNameHsDecl )
import RnHsSyn		( RenamedHsExpr )
import TcExpr		( tcMonoExpr )
import TcHsSyn		( TcExpr, TypecheckedHsExpr, mkHsLet, zonkTopExpr )
import TcSimplify	( tcSimplifyTop )
import TcType		( TcType, openTypeKind )
import TcEnv		( spliceOK, tcMetaTy )
import TcRnTypes	( TopEnv(..) )
import TcMType		( newTyVarTy )
import Name		( Name )
import TcRnMonad

import TysWiredIn	( mkListTy )
import DsMeta		( exprTyConName, declTyConName )
import Outputable
import GHC.Base		( unsafeCoerce# )	-- Should have a better home in the module hierarchy
\end{code}


%************************************************************************
%*									*
\subsection{Main interface + stubs for the non-GHCI case
%*									*
%************************************************************************

\begin{code}
tcSpliceDecls :: RenamedHsExpr -> TcM [RdrNameHsDecl]

tcSpliceExpr :: Name 
	     -> RenamedHsExpr
	     -> TcType
	     -> TcM TcExpr

#ifndef GHCI
tcSpliceExpr n e ty = pprPanic "Cant do tcSpliceExpr without GHCi" (ppr e)
tcSpliceDecls e     = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr e)
#else
\end{code}

%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
tcBracket :: HsBracket Name -> TcM TcType
tcBracket (ExpBr expr) 
  = newTyVarTy openTypeKind		`thenM` \ any_ty ->
    tcMonoExpr expr any_ty		`thenM_`
    tcMetaTy exprTyConName

tcBracket (DecBr decls)
  = tcTopSrcDecls decls			`thenM_`
    tcMetaTy declTyConName		`thenM` \ decl_ty ->
    returnM (mkListTy decl_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
tcSpliceExpr name expr res_ty
  = getStage		`thenM` \ level ->
    case spliceOK level of {
	Nothing 	-> failWithTc (illegalSplice level) ;
	Just next_level -> 

    case level of {
	Comp 		       -> tcTopSplice expr res_ty ;
	Brack _ ps_var lie_var ->  

	-- A splice inside brackets
  	-- NB: ignore res_ty
	-- e.g.   [| reverse $(h 4) |]
	-- Here (h 4) :: Q Exp
	-- but $(h 4) :: forall a.a 	i.e. anything!

    tcMetaTy exprTyConName			`thenM` \ meta_exp_ty ->
    setStage (Splice next_level) (
	setLIEVar lie_var	   $
	tcMonoExpr expr meta_exp_ty
    )						`thenM` \ expr' ->

	-- Write the pending splice into the bucket
    readMutVar ps_var				`thenM` \ ps ->
    writeMutVar ps_var ((name,expr') : ps) 	`thenM_`

    returnM (panic "tcSpliceExpr")	-- The returned expression is ignored
    }} 

-- tcTopSplice used to have this:
-- Note that we do not decrement the level (to -1) before 
-- typechecking the expression.  For example:
--	f x = $( ...$(g 3) ... )
-- The recursive call to tcMonoExpr will simply expand the 
-- inner escape before dealing with the outer one

tcTopSplice expr res_ty
  = tcMetaTy exprTyConName		`thenM` \ meta_exp_ty ->
    setStage topSpliceStage (
	getLIE (tcMonoExpr expr meta_exp_ty)
    )					`thenM` \ (expr', lie) ->

	-- Solve the constraints
    tcSimplifyTop lie			`thenM` \ const_binds ->
    let 
	q_expr = mkHsLet const_binds expr'
    in
    zonkTopExpr q_expr			`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaE zonked_q_expr		`thenM` \ simple_expr ->
  
    let 
	-- simple_expr :: Meta.Exp

	expr2 :: RdrNameHsExpr
	expr2 = convertToHsExpr simple_expr 
    in
    traceTc (text "Got result" <+> ppr expr2) 	`thenM_`
    initRn SourceMode (rnExpr expr2)		`thenM` \ (exp3, fvs) ->
    importSupportingDecls fvs			`thenM` \ env ->

    setGblEnv env (tcMonoExpr exp3 res_ty)
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
-- Always at top level
tcSpliceDecls expr
  = tcMetaTy declTyConName		`thenM` \ meta_dec_ty ->
    setStage topSpliceStage (
	getLIE (tcMonoExpr expr (mkListTy meta_dec_ty))
    )					`thenM` \ (expr', lie) ->
	-- Solve the constraints
    tcSimplifyTop lie			`thenM` \ const_binds ->
    let 
	q_expr = mkHsLet const_binds expr'
    in
    zonkTopExpr q_expr			`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaD zonked_q_expr		`thenM` \ simple_expr ->
    let 
	-- simple_expr :: [Meta.Dec]
	decls :: [RdrNameHsDecl]
	decls = convertToHsDecls simple_expr 
    in
    traceTc (text "Got result" <+> vcat (map ppr decls))	`thenM_`
    returnM decls
\end{code}


%************************************************************************
%*									*
\subsection{Running an expression}
%*									*
%************************************************************************

\begin{code}
runMetaE :: TypecheckedHsExpr 	-- Of type (Q Exp)
	 -> TcM Meta.Exp	-- Of type Exp
runMetaE e = runMeta tcRunQ e

runMetaD :: TypecheckedHsExpr 	-- Of type [Q Dec]
	 -> TcM [Meta.Dec]	-- Of type [Dec]
runMetaD e = runMeta run_decl e
	   where
	     run_decl :: [Meta.Decl] -> TcM [Meta.Dec]
	     run_decl ds = mappM tcRunQ ds

-- Warning: if Q is anything other than IO, we need to change this
tcRunQ :: Meta.Q a -> TcM a
tcRunQ thing = ioToTcRn thing


runMeta :: (x -> TcM t)		-- :: X -> IO t
	-> TypecheckedHsExpr 	-- Of type X
	-> TcM t		-- Of type t
runMeta run_it expr :: TcM t
  = getTopEnv		`thenM` \ top_env ->
    getEps		`thenM` \ eps ->
    getNameCache	`thenM` \ name_cache -> 
    getModule		`thenM` \ this_mod ->
    getGlobalRdrEnv	`thenM` \ rdr_env -> 
    let
	ghci_mode = top_mode top_env

	hsc_env = HscEnv { hsc_mode = ghci_mode, hsc_HPT = top_hpt top_env,
			   hsc_dflags = top_dflags top_env }

	pcs = PCS { pcs_nc = name_cache, pcs_EPS = eps }

	print_unqual = unQualInScope rdr_env
    in
    if (ghci_mode == OneShot) then
	failWithTc (ptext SLIT("You must use --make or --interactive to run splice expressions"))
	-- The reason for this is that the demand-linker doesn't have
	-- enough information available to link all the things that
	-- are needed when you try to run a splice
    else

    ioToTcRn (HscMain.compileExpr hsc_env pcs this_mod 
				  print_unqual expr) `thenM` \ hval ->

    tryM (run_it (unsafeCoerce# hval))	`thenM` \ either_tval ->

    case either_tval of
	  Left exn -> failWithTc (vcat [text "Exception when running compile-time code:", 
				        nest 4 (vcat [text "Code:" <+> ppr expr,
						      text ("Exn: " ++ show exn)])])
	  Right v  -> returnM v
\end{code}



-----------------------------------
	Random comments


      module Foo where
	import Lib( g :: Int -> M Exp )
	h x = not x	
	f x y = [| \z -> (x, $(g y), z, map, h) |]

	h p = $( (\q r -> if q then [| \s -> (p,r,s) |] 
			       else ... ) True 3)   )

==> core

	f :: Liftable a => a -> Int -> M Exp
	f = /\a -> \d::Liftable a ->
	    \ x y -> genSym "z"		`bindM` \ z::String ->
		     g y		`bindM` \ vv::Exp ->
		     Lam z (Tup [lift d x, v, Var z, 
				 Glob "Prelude" "map",
				 Glob "Foo" "h"])


	h :: Tree Int -> M Exp
	h = \p -> \s' -> (p,3,s')


		Bound	Used

	map:	C0	C1	(top-level/imp)
	x: 	C0	C1	(lam/case)
	y:	C0	C0
	z:	C1	C1

	p:	C0	S1
	r:	S0	S1
	q:	S0	S0
	s:	S1	S1

-------

	f x y = lam "z" (tup [lift x, g y, var "z", 
			      [| map |], [| h |] ])
==> core
	
	f = \x y -> lam "z" (tup [lift d x, g y, var "z",
				  return (Glob "Prelude" "map"),
				  return (Glob "Foo" "h")])







	h :: M Exp -> M Exp
	h v = [| \x -> map $v x |]

	g :: Tree Int -> M Exp
	g x = $(h [| x |])
==>
	g x = \x' -> map x x'

*** Simon claims x does not have to be liftable! **
	
Level 0	compile time
Level 1 run time
Level 2 code returned by run time (generation time)

Non-top-level variables
	x occurs at level 1
	  inside brackets
	    bound at level 0 	--> x
	    bound at level 1    --> var "x"

	  not inside brackets 	--> x

	x at level 2
	  inside brackets
	    bound at level 0 	--> x
	    bound at level 1    --> var "x"

	f x = x

Two successive brackets aren't allowed


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

\begin{code}
illegalSplice level
  = ptext SLIT("Illegal splice at level") <+> ppr level

#endif 	/* GHCI */
\end{code}
