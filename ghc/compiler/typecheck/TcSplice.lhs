2%
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

import HscTypes		( HscEnv(..), PersistentCompilerState(..) )
import HsSyn		( HsBracket(..), HsExpr(..) )
import Convert		( convertToHsExpr, convertToHsDecls )
import RnExpr		( rnExpr )
import RdrHsSyn		( RdrNameHsExpr, RdrNameHsDecl )
import RnHsSyn		( RenamedHsExpr )
import TcExpr		( tcCheckRho, tcMonoExpr )
import TcHsSyn		( TcExpr, TypecheckedHsExpr, mkHsLet, zonkTopExpr )
import TcSimplify	( tcSimplifyTop, tcSimplifyBracket )
import TcUnify		( Expected, zapExpectedTo, zapExpectedType )
import TcType		( TcType, openTypeKind, mkAppTy )
import TcEnv		( spliceOK, tcMetaTy, tcWithTempInstEnv, bracketOK )
import TcRnTypes	( TopEnv(..) )
import TcMType		( newTyVarTy, UserTypeCtxt(ExprSigCtxt) )
import TcMonoType	( tcHsSigType )
import Name		( Name )
import TcRnMonad

import TysWiredIn	( mkListTy )
import DsMeta		( expQTyConName, typeQTyConName, decTyConName, qTyConName )
import ErrUtils (Message)
import Outputable
import Panic		( showException )
import GHC.Base		( unsafeCoerce# )	-- Should have a better home in the module hierarchy
import Monad (liftM)
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
	     -> Expected TcType
	     -> TcM TcExpr

#ifndef GHCI
tcSpliceExpr n e ty = pprPanic "Cant do tcSpliceExpr without GHCi" (ppr e)
tcSpliceDecls e     = pprPanic "Cant do tcSpliceDecls without GHCi" (ppr e)
#else
\end{code}

%************************************************************************
%*									*
\subsection{Quoting an expression}
%*									*
%************************************************************************

\begin{code}
tcBracket :: HsBracket Name -> Expected TcType -> TcM TcExpr
tcBracket brack res_ty
  = getStage 				`thenM` \ level ->
    case bracketOK level of {
	Nothing         -> failWithTc (illegalBracket level) ;
	Just next_level ->

   	-- Typecheck expr to make sure it is valid,
	-- but throw away the results.  We'll type check
	-- it again when we actually use it.
    newMutVar []	 		`thenM` \ pending_splices ->
    getLIEVar				`thenM` \ lie_var ->

    setStage (Brack next_level pending_splices lie_var) (
	getLIE (tc_bracket brack)
    )					`thenM` \ (meta_ty, lie) ->
    tcSimplifyBracket lie 		`thenM_`  

	-- Make the expected type have the right shape
    zapExpectedTo res_ty meta_ty	`thenM_`

	-- Return the original expression, not the type-decorated one
    readMutVar pending_splices		`thenM` \ pendings ->
    returnM (HsBracketOut brack pendings)
    }

tc_bracket :: HsBracket Name -> TcM TcType
tc_bracket (ExpBr expr) 
  = newTyVarTy openTypeKind	`thenM` \ any_ty ->
    tcCheckRho expr any_ty	`thenM_`
    tcMetaTy expQTyConName
	-- Result type is Expr (= Q Exp)

tc_bracket (TypBr typ) 
  = tcHsSigType ExprSigCtxt typ		`thenM_`
    tcMetaTy typeQTyConName
	-- Result type is Type (= Q Typ)

tc_bracket (DecBr decls)
  = tcWithTempInstEnv (tcTopSrcDecls decls)	`thenM_`
	-- Typecheck the declarations, dicarding any side effects
	-- on the instance environment (which is in a mutable variable)
	-- and the extended environment.  We'll get all that stuff
	-- later, when we splice it in

    tcMetaTy decTyConName		`thenM` \ decl_ty ->
    tcMetaTy qTyConName			`thenM` \ q_ty ->
    returnM (mkAppTy q_ty (mkListTy decl_ty))
	-- Result type is Q [Dec]
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
  	-- NB: ignore res_ty, apart from zapping it to a mono-type
	-- e.g.   [| reverse $(h 4) |]
	-- Here (h 4) :: Q Exp
	-- but $(h 4) :: forall a.a 	i.e. anything!

    zapExpectedType res_ty			`thenM_`
    tcMetaTy expQTyConName			`thenM` \ meta_exp_ty ->
    setStage (Splice next_level) (
	setLIEVar lie_var	   $
	tcCheckRho expr meta_exp_ty
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
  = tcMetaTy expQTyConName		`thenM` \ meta_exp_ty ->

	-- Typecheck the expression
    tcTopSpliceExpr expr meta_exp_ty	`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaE zonked_q_expr		`thenM` \ simple_expr ->
  
    let 
	-- simple_expr :: Meta.Exp

	expr2 :: RdrNameHsExpr
	expr2 = convertToHsExpr simple_expr 
    in
    traceTc (text "Got result" <+> ppr expr2) 	`thenM_`

    showSplice "expression" 
	       zonked_q_expr (ppr expr2)	`thenM_`
    initRn SourceMode (rnExpr expr2)		`thenM` \ (exp3, fvs) ->
    importSupportingDecls fvs			`thenM` \ env ->

    setGblEnv env (tcMonoExpr exp3 res_ty)


tcTopSpliceExpr :: RenamedHsExpr -> TcType -> TcM TypecheckedHsExpr
-- Type check an expression that is the body of a top-level splice
--   (the caller will compile and run it)
tcTopSpliceExpr expr meta_ty
  = checkNoErrs $	-- checkNoErrs: must not try to run the thing
			--	        if the type checker fails!

    setStage topSpliceStage $

	-- Typecheck the expression
    getLIE (tcCheckRho expr meta_ty)	`thenM` \ (expr', lie) ->

	-- Solve the constraints
    tcSimplifyTop lie			`thenM` \ const_binds ->
	
	-- And zonk it
    zonkTopExpr (mkHsLet const_binds expr')
\end{code}


%************************************************************************
%*									*
\subsection{Splicing an expression}
%*									*
%************************************************************************

\begin{code}
-- Always at top level
tcSpliceDecls expr
  = tcMetaTy decTyConName		`thenM` \ meta_dec_ty ->
    tcMetaTy qTyConName    		`thenM` \ meta_q_ty ->
    let
	list_q = mkAppTy meta_q_ty (mkListTy meta_dec_ty)
    in
    tcTopSpliceExpr expr list_q		`thenM` \ zonked_q_expr ->

	-- Run the expression
    traceTc (text "About to run" <+> ppr zonked_q_expr) 	`thenM_`
    runMetaD zonked_q_expr		`thenM` \ simple_expr ->
    -- simple_expr :: [Meta.Dec]
    -- decls :: [RdrNameHsDecl]
    handleErrors (convertToHsDecls simple_expr) `thenM` \ decls ->
    traceTc (text "Got result" <+> vcat (map ppr decls))	`thenM_`
    showSplice "declarations"
	       zonked_q_expr (vcat (map ppr decls))		`thenM_`
    returnM decls

  where handleErrors :: [Either a Message] -> TcM [a]
        handleErrors [] = return []
        handleErrors (Left x:xs) = liftM (x:) (handleErrors xs)
        handleErrors (Right m:xs) = do addErrTc m
                                       handleErrors xs
\end{code}


%************************************************************************
%*									*
\subsection{Running an expression}
%*									*
%************************************************************************

\begin{code}
runMetaE :: TypecheckedHsExpr 	-- Of type (Q Exp)
	 -> TcM Meta.Exp	-- Of type Exp
runMetaE e = runMeta e

runMetaD :: TypecheckedHsExpr 	-- Of type Q [Dec]
	 -> TcM [Meta.Dec]	-- Of type [Dec]
runMetaD e = runMeta e

runMeta :: TypecheckedHsExpr 	-- Of type X
	-> TcM t		-- Of type t
runMeta expr
  = getTopEnv		`thenM` \ top_env ->
    getGblEnv		`thenM` \ tcg_env ->
    getEps		`thenM` \ eps ->
    getNameCache	`thenM` \ name_cache -> 
    getModule		`thenM` \ this_mod ->
    let
	ghci_mode = top_mode top_env

	hsc_env = HscEnv { hsc_mode = ghci_mode, hsc_HPT = top_hpt top_env,
			   hsc_dflags = top_dflags top_env }

	pcs = PCS { pcs_nc = name_cache, pcs_EPS = eps }

	type_env = tcg_type_env tcg_env
	rdr_env  = tcg_rdr_env tcg_env
    in
	-- Wrap the compile-and-run in an exception-catcher
	-- Compiling might fail if linking fails
	-- Running might fail if it throws an exception
    tryM (ioToTcRn (do
	hval <- HscMain.compileExpr 
		      hsc_env pcs this_mod 
	              rdr_env type_env expr
        Meta.runQ (unsafeCoerce# hval)		-- Coerce it to Q t, and run it
    ))					`thenM` \ either_tval ->

    case either_tval of
	  Left exn -> failWithTc (vcat [text "Exception when trying to run compile-time code:", 
				        nest 4 (vcat [text "Code:" <+> ppr expr,
						      text ("Exn: " ++ Panic.showException exn)])])
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
showSplice :: String -> TypecheckedHsExpr -> SDoc -> TcM ()
showSplice what before after
  = getSrcLocM		`thenM` \ loc ->
    traceSplice (vcat [ppr loc <> colon <+> text "Splicing" <+> text what, 
		       nest 2 (sep [nest 2 (ppr before),
				    text "======>",
				    nest 2 after])])

illegalBracket level
  = ptext SLIT("Illegal bracket at level") <+> ppr level

illegalSplice level
  = ptext SLIT("Illegal splice at level") <+> ppr level

#endif 	/* GHCI */
\end{code}
