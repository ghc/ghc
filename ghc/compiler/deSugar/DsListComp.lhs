%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsListComp]{Desugaring list comprehensions}

\begin{code}
module DsListComp ( dsListComp ) where

#include "HsVersions.h"

import {-# SOURCE #-} DsExpr ( dsExpr, dsLet )

import BasicTypes	( Boxity(..) )
import HsSyn		( OutPat(..), HsExpr(..), Stmt(..), HsMatchContext(..), HsDoContext(..) )
import TcHsSyn		( TypecheckedStmt, TypecheckedPat, TypecheckedHsExpr )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad		-- the monadery used in the desugarer
import DsUtils

import CmdLineOpts	( opt_FoldrBuildOn )
import CoreUtils	( exprType, mkIfThenElse )
import Id		( idType )
import Var              ( Id )
import TcType		( mkTyVarTy, mkFunTys, mkFunTy, Type )
import TysPrim		( alphaTyVar )
import TysWiredIn	( nilDataCon, consDataCon, unitDataConId, mkListTy, mkTupleTy )
import Match		( matchSimply )
import PrelNames	( foldrName, buildName )
import SrcLoc		( noSrcLoc )
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
  |  not opt_FoldrBuildOn 		 -- Be boring
  || isParallelComp quals
  = deListComp quals (mkNilExpr elt_ty)

  | otherwise				 -- foldr/build lives!
  = newTyVarsDs [alphaTyVar]    `thenDs` \ [n_tyvar] ->
    let
	n_ty = mkTyVarTy n_tyvar
        c_ty = mkFunTys [elt_ty, n_ty] n_ty
    in
    newSysLocalsDs [c_ty,n_ty]		`thenDs` \ [c, n] ->
    dfListComp c n quals		`thenDs` \ result ->
    dsLookupGlobalValue buildName	`thenDs` \ build_id ->
    returnDs (Var build_id `App` Type elt_ty 
			   `App` mkLams [n_tyvar, c, n] result)

  where isParallelComp (ParStmtOut bndrstmtss : _) = True
	isParallelComp _                           = False
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

To the above, we add an additional rule to deal with parallel list
comprehensions.  The translation goes roughly as follows:
     [ e | p1 <- e11, let v1 = e12, p2 <- e13
         | q1 <- e21, let v2 = e22, q2 <- e23]
     =>
     [ e | ((x1, .., xn), (y1, ..., ym)) <-
               zip [(x1,..,xn) | p1 <- e11, let v1 = e12, p2 <- e13]
                   [(y1,..,ym) | q1 <- e21, let v2 = e22, q2 <- e23]]
where (x1, .., xn) are the variables bound in p1, v1, p2
      (y1, .., ym) are the variables bound in q1, v2, q2

In the translation below, the ParStmtOut branch translates each parallel branch
into a sub-comprehension, and desugars each independently.  The resulting lists
are fed to a zip function, we create a binding for all the variables bound in all
the comprehensions, and then we hand things off the the desugarer for bindings.
The zip function is generated here a) because it's small, and b) because then we
don't have to deal with arbitrary limits on the number of zip functions in the
prelude, nor which library the zip function came from.
The introduced tuples are Boxed, but only because I couldn't get it to work
with the Unboxed variety.

\begin{code}

deListComp :: [TypecheckedStmt] -> CoreExpr -> DsM CoreExpr

deListComp (ParStmtOut bndrstmtss : quals) list
  = mapDs do_list_comp bndrstmtss	`thenDs` \ exps ->
    mkZipBind qual_tys			`thenDs` \ (zip_fn, zip_rhs) ->

	-- Deal with [e | pat <- zip l1 .. ln] in example above
    deBindComp pat (Let (Rec [(zip_fn, zip_rhs)]) (mkApps (Var zip_fn) exps)) 
		   quals list

  where -- pat is the pattern ((x1,..,xn), (y1,..,ym)) in the example above
	pat	       = TuplePat pats Boxed
	pats	       = map (\(bs,_) -> mk_hs_tuple_pat bs) bndrstmtss

	-- Types of (x1,..,xn), (y1,..,yn) etc
	qual_tys = [ mk_bndrs_tys bndrs | (bndrs,_) <- bndrstmtss ]

	do_list_comp (bndrs, stmts)
	  = dsListComp (stmts ++ [ResultStmt (mk_hs_tuple_expr bndrs) noSrcLoc])
		       (mk_bndrs_tys bndrs)

	mk_bndrs_tys bndrs = mk_tuple_ty (map idType bndrs)

	-- Last: the one to return
deListComp [ResultStmt expr locn] list	-- Figure 7.4, SLPJ, p 135, rule C above
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkConsExpr (exprType core_expr) core_expr list)

	-- Non-last: must be a guard
deListComp (ExprStmt guard locn : quals) list	-- rule B above
  = dsExpr guard       		`thenDs` \ core_guard ->
    deListComp quals list	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest list)

-- [e | let B, qs] = let B in [e | qs]
deListComp (LetStmt binds : quals) list
  = deListComp quals list	`thenDs` \ core_rest ->
    dsLet binds core_rest

deListComp (BindStmt pat list1 locn : quals) core_list2 -- rule A' above
  = dsExpr list1		    `thenDs` \ core_list1 ->
    deBindComp pat core_list1 quals core_list2
\end{code}


\begin{code}
deBindComp pat core_list1 quals core_list2
  = let
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
    matchSimply (Var u2) (DoCtxt ListComp) pat
		rest_expr core_fail		`thenDs` \ core_match ->
    let
	rhs = Lam u1 $
	      Case (Var u1) u1 [(DataAlt nilDataCon,  [], 	core_list2),
			        (DataAlt consDataCon, [u2, u3], core_match)]
    in
    returnDs (Let (Rec [(h, rhs)]) letrec_body)
\end{code}


\begin{code}
mkZipBind :: [Type] -> DsM (Id, CoreExpr)
-- mkZipBind [t1, t2] 
-- = (zip, \as1:[t1] as2:[t2] 
--	   -> case as1 of 
--		[] -> []
--		(a1:as'1) -> case as2 of
--				[] -> []
--				(a2:as'2) -> (a2,a2) : zip as'1 as'2)]

mkZipBind elt_tys 
  = mapDs newSysLocalDs  list_tys	`thenDs` \ ass ->
    mapDs newSysLocalDs  elt_tys	`thenDs` \ as' ->
    mapDs newSysLocalDs  list_tys	`thenDs` \ as's ->
    newSysLocalDs zip_fn_ty		`thenDs` \ zip_fn ->
    let 
	inner_rhs = mkConsExpr ret_elt_ty (mkTupleExpr as') (mkVarApps (Var zip_fn) as's)
	zip_body  = foldr mk_case inner_rhs (zip3 ass as' as's)
    in
    returnDs (zip_fn, mkLams ass zip_body)
  where
    list_tys   = map mkListTy elt_tys
    ret_elt_ty = mk_tuple_ty elt_tys
    zip_fn_ty  = mkFunTys list_tys (mkListTy ret_elt_ty)

    mk_case (as, a', as') rest
	  = Case (Var as) as [(DataAlt nilDataCon,  [],        mkNilExpr ret_elt_ty),
			      (DataAlt consDataCon, [a', as'], rest)]

-- Helper function 
mk_tuple_ty :: [Type] -> Type
mk_tuple_ty [ty] = ty
mk_tuple_ty tys  = mkTupleTy Boxed (length tys) tys

-- Helper functions that makes an HsTuple only for non-1-sized tuples
mk_hs_tuple_expr :: [Id] -> TypecheckedHsExpr
mk_hs_tuple_expr []   = HsVar unitDataConId
mk_hs_tuple_expr [id] = HsVar id
mk_hs_tuple_expr ids  = ExplicitTuple [ HsVar i | i <- ids ] Boxed

mk_hs_tuple_pat :: [Id] -> TypecheckedPat
mk_hs_tuple_pat [b] = VarPat b
mk_hs_tuple_pat bs  = TuplePat (map VarPat bs) Boxed
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

	-- Last: the one to return
dfListComp c_id n_id [ResultStmt expr locn]
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkApps (Var c_id) [core_expr, Var n_id])

	-- Non-last: must be a guard
dfListComp c_id n_id (ExprStmt guard locn  : quals)
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
    matchSimply (Var x) (DoCtxt ListComp) 
		pat core_rest (Var b)		`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return
    dsLookupGlobalValue foldrName		`thenDs` \ foldr_id ->
    returnDs (
      Var foldr_id `App` Type x_ty 
		   `App` Type b_ty
		   `App` mkLams [x, b] core_expr
		   `App` Var n_id
		   `App` core_list1
    )
\end{code}


