%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsListComp]{Desugaring list comprehensions}

\begin{code}
#include "HsVersions.h"

module DsListComp ( dsListComp ) where

IMP_Ubiq()
IMPORT_DELOOPER(DsLoop)		-- break dsExpr-ish loop

import HsSyn		( Stmt(..), HsExpr, HsBinds )
import TcHsSyn		( SYN_IE(TypecheckedStmt), SYN_IE(TypecheckedHsExpr) , SYN_IE(TypecheckedHsBinds) )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import CmdLineOpts	( opt_FoldrBuildOn )
import CoreUtils	( coreExprType, mkCoreIfThenElse )
import PrelVals		( mkBuild, foldrId )
import Type		( mkTyVarTy, mkForAllTy, mkFunTys, mkFunTy )
import TysPrim		( alphaTy )
import TysWiredIn	( nilDataCon, consDataCon, listTyCon )
import TyVar		( alphaTyVar )
import Match		( matchSimply )
import Util		( panic )
\end{code}

List comprehensions may be desugared in one of two ways: ``ordinary''
(as you would expect if you read SLPJ's book) and ``with foldr/build
turned on'' (if you read Gill {\em et al.}'s paper on the subject).

There will be at least one ``qualifier'' in the input.

\begin{code}
dsListComp :: [TypecheckedStmt] 
	   -> Type		-- Type of list elements
	   -> DsM CoreExpr

dsListComp quals elt_ty
  | not opt_FoldrBuildOn 		 -- Be boring
  = deListComp quals nil_expr

  | otherwise				 -- foldr/build lives!
  = newTyVarsDs [alphaTyVar]    `thenDs` \ [n_tyvar] ->
    let
        alpha_to_alpha = alphaTy `mkFunTy` alphaTy

	n_ty = mkTyVarTy n_tyvar
        c_ty = mkFunTys [elt_ty, n_ty] n_ty
        g_ty = mkForAllTy alphaTyVar (
    		(elt_ty `mkFunTy` alpha_to_alpha)
    		`mkFunTy` 
    		alpha_to_alpha
    	   )
    in
    newSysLocalsDs [c_ty,n_ty,g_ty]  `thenDs` \ [c, n, g] ->

    dfListComp	c_ty c
    		n_ty n
    		quals	    `thenDs` \ result ->

    returnDs (mkBuild elt_ty n_tyvar c n g result)
  where
    nil_expr    = mkCon nilDataCon [] [elt_ty] []
\end{code}

%************************************************************************
%*									*
\subsection[DsListComp-ordinary]{Ordinary desugaring of list comprehensions}
%*									*
%************************************************************************

Just as in Phil's chapter~7 in SLPJ, using the rules for
optimally-compiled list comprehensions.  This is what Kevin followed
as well, and I quite happily do the same.  The TQ translation scheme
transforms a list of qualifiers (either boolean expressions or
generators) into a single expression which implements the list
comprehension.  Because we are generating 2nd-order polymorphic
lambda-calculus, calls to NIL and CONS must be applied to a type
argument, as well as their usual value arguments.
\begin{verbatim}
TE << [ e | qs ] >>  =  TQ << [ e | qs ] ++ Nil (typeOf e) >>

(Rule C)
TQ << [ e | ] ++ L >> = Cons (typeOf e) TE <<e>> TE <<L>>

(Rule B)
TQ << [ e | b , qs ] ++ L >> =
    if TE << b >> then TQ << [ e | qs ] ++ L >> else TE << L >>

(Rule A')
TQ << [ e | p <- L1, qs ]  ++  L2 >> =
  letrec
    h = \ u1 ->
    	  case u1 of
	    []        ->  TE << L2 >>
	    (u2 : u3) ->
		  (( \ TE << p >> -> ( TQ << [e | qs]  ++  (h u3) >> )) u2)
		    [] (h u3)
  in
    h ( TE << L1 >> )

"h", "u1", "u2", and "u3" are new variables.
\end{verbatim}

@deListComp@ is the TQ translation scheme.  Roughly speaking, @dsExpr@
is the TE translation scheme.  Note that we carry around the @L@ list
already desugared.  @dsListComp@ does the top TE rule mentioned above.

\begin{code}
deListComp :: [TypecheckedStmt] -> CoreExpr -> DsM CoreExpr

deListComp [ReturnStmt expr] list		-- Figure 7.4, SLPJ, p 135, rule C above
  = dsExpr expr			`thenDs` \ core_expr ->
    mkConDs consDataCon [TyArg (coreExprType core_expr), VarArg core_expr, VarArg list]

deListComp (GuardStmt guard locn : quals) list	-- rule B above
  = dsExpr guard       		`thenDs` \ core_guard ->
    deListComp quals list	`thenDs` \ core_rest ->
    returnDs (mkCoreIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list
  = dsBinds binds		`thenDs` \ core_binds ->
    deListComp quals list	`thenDs` \ core_rest ->
    returnDs (mkCoLetsAny core_binds core_rest)

deListComp (BindStmt pat list1 locn : quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    let
	u3_ty@u1_ty = coreExprType core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = outPatType pat

	res_ty = coreExprType core_list2
	h_ty   = u1_ty `mkFunTy` res_ty
    in
    newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]	`thenDs` \ [h, u1, u2, u3] ->

    -- the "fail" value ...
    mkAppDs (Var h) [VarArg (Var u3)]		`thenDs` \ core_fail ->
    deListComp quals core_fail			`thenDs` \ rest_expr ->
    matchSimply (Var u2) pat res_ty 
		rest_expr core_fail		`thenDs` \ core_match ->
    mkAppDs (Var h) [VarArg core_list1]		`thenDs` \ letrec_body ->

    returnDs (
      mkCoLetrecAny [
      ( h,
	(Lam (ValBinder u1)
	 (Case (Var u1)
	    (AlgAlts
	      [(nilDataCon,  [], 	core_list2),
	       (consDataCon, [u2, u3],  core_match)]
	    NoDefault)))
      )] letrec_body
    )
\end{code}

%************************************************************************
%*									*
\subsection[DsListComp-foldr-build]{Foldr/Build desugaring of list comprehensions}
%*									*
%************************************************************************

@dfListComp@ are the rules used with foldr/build turned on:
\begin{verbatim}
TE < [ e | ] >>          c n = c e n
TE << [ e | b , q ] >>   c n = if b then TE << [ e | q ] >> c n else n
TE << [ e | p <- l , q ] c n =  foldr
			(\ TE << p >> b -> TE << [ e | q ] >> c b
			   _          b  -> b)	n l
\end{verbatim}
\begin{code}
dfListComp :: Type -> Id		-- 'c'; its type and id
	   -> Type -> Id		-- 'n'; its type and id
	   -> [TypecheckedStmt] 	-- the rest of the qual's
	   -> DsM CoreExpr

dfListComp c_ty c_id n_ty n_id [ReturnStmt expr]
  = dsExpr expr			`thenDs` \ core_expr ->
    mkAppDs (Var c_id) [VarArg core_expr, VarArg (Var n_id)]

dfListComp c_ty c_id n_ty n_id (GuardStmt guard locn  : quals)
  = dsExpr guard               			`thenDs` \ core_guard ->
    dfListComp c_ty c_id n_ty n_id quals	`thenDs` \ core_rest ->
    returnDs (mkCoreIfThenElse core_guard core_rest (Var n_id))

dfListComp c_ty c_id n_ty n_id (LetStmt binds : quals)
  -- new in 1.3, local bindings
  = dsBinds binds                         `thenDs` \ core_binds ->
    dfListComp c_ty c_id n_ty n_id quals  `thenDs` \ core_rest ->
    returnDs (mkCoLetsAny core_binds core_rest)

dfListComp c_ty c_id n_ty n_id (BindStmt pat list1 locn : quals)
    -- evaluate the two lists
  = dsExpr list1				`thenDs` \ core_list1 ->

    -- find the required type

    let p_ty   = outPatType pat
	b_ty   = n_ty		-- alias b_ty to n_ty
	fn_ty  = mkFunTys [p_ty, b_ty] b_ty
	lst_ty = coreExprType core_list1
    in

    -- create some new local id's

    newSysLocalsDs [b_ty,p_ty,fn_ty,lst_ty]		`thenDs` \ [b,p,fn,lst] ->

    -- build rest of the comprehesion

    dfListComp c_ty c_id b_ty b quals			`thenDs` \ core_rest ->
    -- build the pattern match

    matchSimply (Var p) pat b_ty core_rest (Var b)	`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return

    returnDs (
      mkCoLetsAny
	[NonRec fn (mkValLam [p, b] core_expr),
	 NonRec lst core_list1]
	(mkFoldr p_ty n_ty fn n_id lst)
    )

mkFoldr a b f z xs
  = mkValApp (mkTyApp (Var foldrId) [a,b]) [VarArg f, VarArg z, VarArg xs]
\end{code}
