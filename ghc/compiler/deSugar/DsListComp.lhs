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
import CoreUtils	( coreExprType )
import Var              ( Id, TyVar )
import Const		( Con(..) )
import PrelInfo		( foldrId )
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
    nil_expr = mkNilExpr elt_ty
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

deListComp :: [TypecheckedStmt]
	   -> CoreExpr -> CoreExpr	-- Cons and nil resp; can be copied freely
	   -> DsM CoreExpr

deListComp [ReturnStmt expr] cons nil
  = dsExpr expr			`thenDs` \ expr' ->
    returnDs (mkApps cons [expr', nil])

deListComp (GuardStmt guard locn : quals) cons nil
  = dsExpr guard       		`thenDs` \ guard' ->
    deListComp quals cons nil 	`thenDs` \ rest' ->
    returnDs (mkIfThenElse guard' rest' nil)

deListComp (LetStmt binds : quals) cons nil
  = deListComp quals cons nil	        `thenDs` \ rest' ->
    dsLet binds	rest'

deListComp (BindStmt pat list locn : quals) cons nil
  = dsExpr list		    `thenDs` \ list' ->
    let
	pat_ty 	    = outPatType pat
	nil_ty	    = coreExprType nil
    in
    newSysLocalsDs [pat_ty, nil_ty]			`thenDs` \ [x,ys] ->
 
    dsListComp quals cons (Var ys)			`thenDs` \ rest ->
    matchSimply (Var x) ListCompMatch pat
		rest (Var ys)				`thenDs` \ core_match ->
    bindNonRecDs (mkLams [x,ys] fn_body)		$ \ fn ->
    dsListExpr list (Var fn) nil


data FExpr = FEOther CoreExpr			-- Default case
	   | FECons				-- cons
	   | FEConsComposedWith CoreExpr	-- (cons . e)
	   | FENil				-- nil

feComposeWith FECons g
  = returnDs (FEConsComposedWith g)

feComposeWith (FEOther f) g
  = composeWith f f	`thenDs` \ h ->
    returnDs (FEOther h)

feComposeWith (FEConsComposedWith f) g
  = composeWith f f	`thenDs` \ h ->
    returnDs (FEConsComposedWith h)


composeWith f g
  = newSysLocalDs arg_ty	`thenDs` \ x ->
    returnDs (Lam x (App e (App f (Var x))))
  where
    arg_ty = case splitFunTy_maybe (coreExprType g) of
		Just (arg_ty,_) -> arg_ty
		other		-> panic "feComposeWith"

deListExpr :: TypecheckedHsExpr
	   -> FExpr -> FExpr	-- Cons and nil expressions
	   -> DsM CoreExpr

deListExpr cons nil (HsDoOut ListComp stmts _ _ _ result_ty src_loc)
  = deListComp stmts cons nil

deListExpr cons nil (HsVar map, _, [f,xs])
 | goodInst var mapIdKey = dsExpr f			`thenDs` \ f' ->
			   feComposeWith cons f'	`thenDs` \ cons' ->
			   in
			   deListExpr xs cons' nil


data HsExprForm = GoodForm What [Type] [TypecheckedHsExpr]
		| BadForm

data What = HsMap | HsConcat | HsFilter |  HsZip | HsFoldr

analyseListProducer (HsVar v) ty_args val_args
  | good_inst mapIdKey    2 = GoodForm HsMap ty_args val_args
  | good_inst concatIdKey 1 = GoodForm HsConcat ty_args val_args
  | good_inst filterIdKey 2 = GoodForm HsFilter ty_args val_args
  | good_id   zipIdKey    2 = GoodForm HsZip    ty_args val_args
  | otherwise		    = 
  where
    good_inst key arity = isInstIdOf key v   && result_is_list && n_args == arity
    good_id   key arity = getUnique v == key && result_is_list && n_args == arity

    n_args :: Int
    n_args = length val_args

    result_is_list = resultTyIsList (idType v) ty_args val_args

resultTyIsList ty ty_args val_args
  = go ty ty_args
  where
    go1 ty (_:tys) = case splitForAllTy_maybe ty of
			Just (_,ty) -> go1 ty tys
			Nothing     -> False
    go1 ty [] = go2 ty val_args

    go2 ty (_:args) = case splitFunTy_maybe of
			Just (_,ty) -> go2 ty args
			Nothing     -> False

    go2 ty [] = case splitTyConApp_maybe of
		  Just (tycon, [_]) | tycon == listTyCon -> True
		  other					 -> False


\begin{code}
deListComp :: [TypecheckedStmt] -> CoreExpr -> DsM CoreExpr

deListComp [ReturnStmt expr] list		-- Figure 7.4, SLPJ, p 135, rule C above
  = dsExpr expr			`thenDs` \ core_expr ->
    returnDs (mkConApp consDataCon [Type (coreExprType core_expr), core_expr, list])

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
	u3_ty@u1_ty = coreExprType core_list1	-- two names, same thing

	-- u1_ty is a [alpha] type, and u2_ty = alpha
	u2_ty = outPatType pat

	res_ty = coreExprType core_list2
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
	      Case (Var u1) u1 [(DataCon nilDataCon,  [], 	core_list2),
			        (DataCon consDataCon, [u2, u3], core_match)]
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
    returnDs (mkApps (Var c_id) [core_expr, Var n_id])

dfListComp c_ty c_id n_ty n_id (GuardStmt guard locn  : quals)
  = dsExpr guard               			`thenDs` \ core_guard ->
    dfListComp c_ty c_id n_ty n_id quals	`thenDs` \ core_rest ->
    returnDs (mkIfThenElse core_guard core_rest (Var n_id))

dfListComp c_ty c_id n_ty n_id (LetStmt binds : quals)
  -- new in 1.3, local bindings
  = dfListComp c_ty c_id n_ty n_id quals	`thenDs` \ core_rest ->
    dsLet binds core_rest

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

    matchSimply (Var p) ListCompMatch pat core_rest (Var b)	`thenDs` \ core_expr ->

    -- now build the outermost foldr, and return

    returnDs (
      mkLets
	[NonRec fn (mkLams [p, b] core_expr),
	 NonRec lst core_list1]
	(mkFoldr p_ty n_ty fn n_id lst)
    )
\end{code}


@mkBuild@ is sugar for building a build!

@mkbuild ty tv c n e@ $Rightarrow$ @build ty (/\ tv -> \ c n -> e)@
@ty@ is the type of the list.
@tv@ is always a new type variable.
@c,n@ are Id's for the abstract cons and nil, @g@ for let binding the argument argument.
	c :: a -> b -> b
	n :: b
	v :: (\/ b . (a -> b -> b) -> b -> b) -> [a]
--  \/ a .  (\/ b . (a -> b -> b) -> b -> b) -> [a]
@e@ is the object right inside the @build@

\begin{code}
mkBuild :: Type
	-> TyVar
	-> Id
	-> Id
	-> Id
	-> CoreExpr -- template
	-> CoreExpr -- template

mkBuild ty tv c n g expr
  = Let (NonRec g (mkLams [tv, c,n] expr))
	(mkApps (Var buildId) [Type ty, Var g])

buildId = error "DsListComp: buildId"

mkFoldr a b f z xs
  = mkApps (mkTyApps (Var foldrId) [a,b]) [Var f, Var z, Var xs]
\end{code}

