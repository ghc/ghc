%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsListComp]{Desugaring list comprehensions}

\begin{code}
module DsListComp ( dsListComp ) where

import Ubiq
import DsLoop		-- break dsExpr-ish loop

import HsSyn		( Qual(..), HsExpr, HsBinds )
import TcHsSyn		( TypecheckedQual(..), TypecheckedHsExpr(..) , TypecheckedHsBinds(..) )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import CmdLineOpts	( opt_FoldrBuildOn )
import CoreUtils	( coreExprType, mkCoreIfThenElse )
import PrelVals		( mkBuild, foldrId )
import Type		( mkTyVarTy, mkForAllTy, mkFunTys )
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
dsListComp :: CoreExpr -> [TypecheckedQual] -> DsM CoreExpr

dsListComp expr quals
  = let
	expr_ty = coreExprType expr
    in
    if not opt_FoldrBuildOn then -- be boring
	deListComp expr quals (nIL_EXPR expr_ty)

    else -- foldr/build lives!
	new_alpha_tyvar		    `thenDs` \ (n_tyvar, n_ty) ->
	let
	    alpha_to_alpha = mkFunTys [alphaTy] alphaTy

	    c_ty = mkFunTys [expr_ty, n_ty] n_ty
	    g_ty = mkForAllTy alphaTyVar (
			(mkFunTys [expr_ty, alpha_to_alpha] alpha_to_alpha))
	in
	newSysLocalsDs [c_ty,n_ty,g_ty]  `thenDs` \ [c, n, g] ->

	dfListComp expr expr_ty
			c_ty c
			n_ty n
			quals	    `thenDs` \ result ->

	returnDs (mkBuild expr_ty n_tyvar c n g result)
  where
    nIL_EXPR ty = mkCon nilDataCon [] [ty] []

    new_alpha_tyvar :: DsM (TyVar, Type)
    new_alpha_tyvar
      = newTyVarsDs [alphaTyVar]    `thenDs` \ [new_ty] ->
	returnDs (new_ty, mkTyVarTy new_ty)
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
deListComp :: CoreExpr -> [TypecheckedQual] -> CoreExpr -> DsM CoreExpr

deListComp expr [] list		-- Figure 7.4, SLPJ, p 135, rule C above
  = mkConDs consDataCon [coreExprType expr] [expr, list]

deListComp expr (FilterQual filt : quals) list	-- rule B above
  = dsExpr filt                `thenDs` \ core_filt ->
    deListComp expr quals list `thenDs` \ core_rest ->
    returnDs ( mkCoreIfThenElse core_filt core_rest list )

deListComp expr (LetQual binds : quals) list
  = panic "deListComp:LetQual"

deListComp expr ((GeneratorQual pat list1):quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    let
	u3_ty@u1_ty = coreExprType core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = outPatType pat

	res_ty = coreExprType core_list2
	h_ty = mkFunTys [u1_ty] res_ty
    in
    newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]
				    `thenDs` \ [h', u1, u2, u3] ->
    {-
       Make the function h unfoldable by the deforester.
       Since it only occurs once in the body, we can't get
       an increase in code size by unfolding it.
    -}
    let
	h = if False -- LATER: sw_chkr DoDeforest???
	    then panic "deListComp:deforest"
		 -- replaceIdInfo h' (addInfo (getIdInfo h') DoDeforest)
	    else h'
    in
    -- the "fail" value ...
    mkAppDs (Var h) [] [Var u3]  `thenDs` \ core_fail ->

    deListComp expr quals core_fail `thenDs` \ rest_expr ->

    matchSimply (Var u2) pat res_ty rest_expr core_fail `thenDs` \ core_match ->

    mkAppDs (Var h) [] [core_list1]  `thenDs` \ letrec_body ->

    returnDs (
      mkCoLetrecAny [
      ( h,
	(Lam (ValBinder u1)
	 (Case (Var u1)
	    (AlgAlts
	      [(nilDataCon,  [], core_list2),
	       (consDataCon, [u2, u3], core_match)]
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
dfListComp :: CoreExpr 		-- the inside of the comp
	   -> Type			-- the type of the inside
	   -> Type -> Id		-- 'c'; its type and id
	   -> Type -> Id		-- 'n'; its type and id
	   -> [TypecheckedQual] 	-- the rest of the qual's
	   -> DsM CoreExpr

dfListComp expr expr_ty c_ty c_id n_ty n_id []
  = mkAppDs (Var c_id) [] [expr, Var n_id]

dfListComp expr expr_ty c_ty c_id n_ty n_id (FilterQual filt : quals)
  = dsExpr filt                			`thenDs` \ core_filt ->
    dfListComp expr expr_ty c_ty c_id n_ty n_id quals
						`thenDs` \ core_rest ->
    returnDs (mkCoreIfThenElse core_filt core_rest (Var n_id))

dfListComp expr expr_ty c_ty c_id n_ty n_id (LetQual binds : quals)
  = panic "dfListComp:LetQual"

dfListComp expr expr_ty c_ty c_id n_ty n_id (GeneratorQual pat list1 : quals)
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

    dfListComp expr expr_ty c_ty c_id b_ty b quals	`thenDs` \ core_rest ->
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
