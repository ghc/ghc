%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[DsListComp]{Desugaring list comprehensions}

\begin{code}
module DsListComp ( dsListComp ) where


import AbsSyn		-- the stuff being desugared
import PlainCore	-- the output of desugaring;
			-- importing this module also gets all the
			-- CoreSyn utility functions
import DsMonad		-- the monadery used in the desugarer

import AbsPrel		( mkFunTy, nilDataCon, consDataCon, listTyCon,
			  mkBuild, mkFoldr
			)
import AbsUniType	( alpha_tv, alpha, mkTyVarTy, mkForallTy )
import CmdLineOpts	( GlobalSwitch(..) )
import DsExpr		( dsExpr )
import DsUtils
import Id		( getIdInfo, replaceIdInfo )
import IdInfo
import Match		( matchSimply )
import Util
\end{code}

List comprehensions may be desugared in one of two ways: ``ordinary''
(as you would expect if you read SLPJ's book) and ``with foldr/build
turned on'' (if you read Gill {\em et al.}'s paper on the subject).

There will be at least one ``qualifier'' in the input.

\begin{code}
dsListComp :: PlainCoreExpr -> [TypecheckedQual] -> DsM PlainCoreExpr

dsListComp expr quals
  = let  expr_ty    = typeOfCoreExpr expr
    in
    ifSwitchSetDs FoldrBuildOn (
	new_alpha_tyvar		    `thenDs` \ (n_tyvar, n_ty) ->
	let
	    c_ty = expr_ty `mkFunTy` (n_ty `mkFunTy` n_ty)
	    g_ty = mkForallTy [alpha_tv] (
			(expr_ty `mkFunTy` (alpha `mkFunTy` alpha))
				 `mkFunTy` (alpha `mkFunTy` alpha))
	in
	newSysLocalsDs [c_ty,n_ty,g_ty]  `thenDs` \ [c, n, g] -> 

	dfListComp expr expr_ty
			c_ty c 
			n_ty n
			quals	    `thenDs` \ result ->

	returnDs (mkBuild expr_ty n_tyvar c n g result)

    ) {-else be boring-} (
	deListComp expr quals (nIL_EXPR expr_ty)
    )
  where
    nIL_EXPR ty = CoCon nilDataCon [ty] []

    new_alpha_tyvar :: DsM (TyVar, UniType)
    new_alpha_tyvar
      = newTyVarsDs [alpha_tv]	`thenDs` \ [new_ty] ->
	returnDs (new_ty,mkTyVarTy new_ty)
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
deListComp :: PlainCoreExpr -> [TypecheckedQual] -> PlainCoreExpr -> DsM PlainCoreExpr

deListComp expr [] list		-- Figure 7.4, SLPJ, p 135, rule C above
  = mkCoConDs consDataCon [typeOfCoreExpr expr] [expr, list]

deListComp expr ((FilterQual filt): quals) list	-- rule B above
  = dsExpr filt                `thenDs` \ core_filt ->
    deListComp expr quals list `thenDs` \ core_rest ->
    returnDs ( mkCoreIfThenElse core_filt core_rest list )

deListComp expr ((GeneratorQual pat list1):quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    let
	u3_ty@u1_ty = typeOfCoreExpr core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = typeOfPat pat
	
        res_ty = typeOfCoreExpr core_list2
	h_ty = mkFunTy u1_ty res_ty
    in
    newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]
				    `thenDs` \ [h', u1, u2, u3] ->
    {-
       Make the function h unfoldable by the deforester.
       Since it only occurs once in the body, we can't get
       an increase in code size by unfolding it.
    -}
--  getSwitchCheckerDs		    `thenDs` \ sw_chkr ->
    let
	h = if False -- LATER: sw_chkr DoDeforest???
	    then replaceIdInfo h' (addInfo (getIdInfo h') DoDeforest)
	    else h'
    in
    -- the "fail" value ...
    mkCoAppDs (CoVar h) (CoVar u3)  `thenDs` \ core_fail ->

    deListComp expr quals core_fail `thenDs` \ rest_expr ->

    matchSimply (CoVar u2) pat res_ty rest_expr core_fail `thenDs` \ core_match ->

    mkCoAppDs (CoVar h) core_list1  `thenDs` \ letrec_body ->

    returnDs (
      mkCoLetrecAny [
      ( h,
	(CoLam [ u1 ]
	 (CoCase (CoVar u1)
	    (CoAlgAlts
	      [(nilDataCon,  [], core_list2),
	       (consDataCon, [u2, u3], core_match)]
	    CoNoDefault)))
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
dfListComp :: PlainCoreExpr 		-- the inside of the comp 
	   -> UniType			-- the type of the inside
	   -> UniType -> Id		-- 'c'; its type and id
	   -> UniType -> Id		-- 'n'; its type and id
	   -> [TypecheckedQual] 	-- the rest of the qual's
	   -> DsM PlainCoreExpr

dfListComp expr expr_ty c_ty c_id n_ty n_id [] 
  = mkCoAppDs (CoVar c_id) expr   `thenDs` \ inner ->
    mkCoAppDs inner (CoVar n_id)

dfListComp expr expr_ty c_ty c_id n_ty n_id ((FilterQual filt) : quals)
  = dsExpr filt                			`thenDs` \ core_filt ->
    dfListComp expr expr_ty c_ty c_id n_ty n_id quals
						`thenDs` \ core_rest ->
    returnDs (mkCoreIfThenElse core_filt core_rest (CoVar n_id))

dfListComp expr expr_ty c_ty c_id n_ty n_id ((GeneratorQual pat list1):quals)
    -- evaluate the two lists
  = dsExpr list1				`thenDs` \ core_list1 ->

    -- find the required type

    let p_ty = typeOfPat pat
	b_ty = n_ty		-- alias b_ty to n_ty
	fn_ty = p_ty `mkFunTy` (b_ty `mkFunTy` b_ty)
	lst_ty = typeOfCoreExpr core_list1
    in

    -- create some new local id's

    newSysLocalsDs [b_ty,p_ty,fn_ty,lst_ty]		`thenDs` \ [b,p,fn,lst] ->

    -- build rest of the comprehesion

    dfListComp expr expr_ty c_ty c_id b_ty b quals	`thenDs` \ core_rest ->
    -- build the pattern match

    matchSimply (CoVar p) pat b_ty core_rest (CoVar b)	`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return

    returnDs (
      mkCoLetsAny
	[CoNonRec fn (CoLam [p,b] core_expr),
	 CoNonRec lst core_list1]
	(mkFoldr p_ty n_ty fn n_id lst)
    )
\end{code}

