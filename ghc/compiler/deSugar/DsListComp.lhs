%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsListComp]{Desugaring list comprehensions}

\begin{code}
module DsListComp ( dsListComp ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr ( dsExpr, dsLet )

import HsSyn		( Stmt(..), HsExpr )
import TcHsSyn		( TypecheckedStmt, TypecheckedHsExpr )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import CmdLineOpts	( opt_FoldrBuildOn )
import CoreUtils	( exprType, mkIfThenElse )
import Id		( idType )
import Var              ( Id, TyVar )
import PrelInfo		( foldrId, buildId )
import Type		( mkTyVarTy, mkForAllTy, mkFunTys, mkFunTy, Type )
import TysPrim		( alphaTyVar, alphaTy )
import TysWiredIn	( nilDataCon, consDataCon, listTyCon )
import Match		( matchSimply )
import Outputable
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
  = deListComp quals (mkNilExpr elt_ty)

  | otherwise				 -- foldr/build lives!
  = newTyVarsDs [alphaTyVar]    `thenDs` \ [n_tyvar] ->
    let
	n_ty = mkTyVarTy n_tyvar
        c_ty = mkFunTys [elt_ty, n_ty] n_ty
    in
    newSysLocalsDs [c_ty,n_ty]	`thenDs` \ [c, n] ->

    dfListComp c n quals	`thenDs` \ result ->

    returnDs (Var buildId `App` Type elt_ty 
			  `App` mkLams [n_tyvar, c, n] result)
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

deListComp [ReturnStmt expr] list	-- Figure 7.4, SLPJ, p 135, rule C above
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkConsExpr (exprType core_expr) core_expr list)

deListComp (GuardStmt guard locn : quals) list	-- rule B above
  = dsExpr guard       		`thenDs` \ core_guard ->
    deListComp quals list	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list
  = deListComp quals list	`thenDs` \ core_rest ->
    dsLet binds core_rest

deListComp (BindStmt pat list1 locn : quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    let
	u3_ty@u1_ty = exprType core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = outPatType pat

	res_ty = exprType core_list2
	h_ty   = u1_ty `mkFunTy` res_ty
    in
    newSysLocalsDs [h_ty, u1_ty, u2_ty, u3_ty]	`thenDs` \ [h, u1, u2, u3] ->

    -- the "fail" value ...
    let
	core_fail   = App (Var h) (Var u3)
	letrec_body = App (Var h) core_list1
    in
    deListComp quals core_fail			`thenDs` \ rest_expr ->
    matchSimply (Var u2) ListCompMatch pat
		rest_expr core_fail		`thenDs` \ core_match ->
    let
	rhs = Lam u1 $
	      Case (Var u1) u1 [(DataAlt nilDataCon,  [], 	core_list2),
			        (DataAlt consDataCon, [u2, u3], core_match)]
    in
    returnDs (Let (Rec [(h, rhs)]) letrec_body)
\end{code}


%************************************************************************
%*									*
\subsection[DsListComp-foldr-build]{Foldr/Build desugaring of list comprehensions}
%*									*
%************************************************************************

@dfListComp@ are the rules used with foldr/build turned on:

\begin{verbatim}
TE[ e | ]            c n = c e n
TE[ e | b , q ]      c n = if b then TE[ e | q ] c n else n
TE[ e | p <- l , q ] c n = let 
				f = \ x b -> case x of
						  p -> TE[ e | q ] c b
						  _ -> b
			   in
			   foldr f n l
\end{verbatim}

\begin{code}
dfListComp :: Id -> Id			-- 'c' and 'n'
	   -> [TypecheckedStmt] 	-- the rest of the qual's
	   -> DsM CoreExpr

dfListComp c_id n_id [ReturnStmt expr]
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkApps (Var c_id) [core_expr, Var n_id])

dfListComp c_id n_id (GuardStmt guard locn  : quals)
  = dsExpr guard               			`thenDs` \ core_guard ->
    dfListComp c_id n_id quals	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest (Var n_id))

dfListComp c_id n_id (LetStmt binds : quals)
  -- new in 1.3, local bindings
  = dfListComp c_id n_id quals	`thenDs` \ core_rest ->
    dsLet binds core_rest

dfListComp c_id n_id (BindStmt pat list1 locn : quals)
    -- evaluate the two lists
  = dsExpr list1				`thenDs` \ core_list1 ->

    -- find the required type
    let x_ty   = outPatType pat
	b_ty   = idType n_id
    in

    -- create some new local id's
    newSysLocalsDs [b_ty,x_ty]			`thenDs` \ [b,x] ->

    -- build rest of the comprehesion
    dfListComp c_id b quals			`thenDs` \ core_rest ->

    -- build the pattern match
    matchSimply (Var x) ListCompMatch pat core_rest (Var b)	`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return
    returnDs (
      Var foldrId `App` Type x_ty 
		  `App` Type b_ty
		  `App` mkLams [x, b] core_expr
		  `App` Var n_id
		  `App` core_list1
    )
\end{code}


