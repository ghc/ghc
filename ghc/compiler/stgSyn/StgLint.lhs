%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[StgLint]{A ``lint'' pass to check for Stg correctness}

\begin{code}
module StgLint ( lintStgBindings ) where

#include "HsVersions.h"

import StgSyn

import Bag		( Bag, emptyBag, isEmptyBag, snocBag, bagToList )
import Id		( Id, idType, isLocalId )
import VarSet
import DataCon		( DataCon, dataConArgTys, dataConRepType )
import CoreSyn		( AltCon(..) )
import PrimOp		( primOpType )
import Literal		( literalType )
import Maybes		( catMaybes )
import Name		( getSrcLoc )
import ErrUtils		( Message, mkLocMessage )
import Type		( mkFunTys, splitFunTys, splitTyConApp_maybe,
			  isUnLiftedType, isTyVarTy, dropForAlls, Type
			)
import TyCon		( isAlgTyCon, isNewTyCon, tyConDataCons )
import Util		( zipEqual, equalLength )
import SrcLoc		( srcLocSpan )
import Outputable

infixr 9 `thenL`, `thenL_`, `thenMaybeL`
\end{code}

Checks for
	(a) *some* type errors
	(b) locally-defined variables used but not defined


Note: unless -dverbose-stg is on, display of lint errors will result
in "panic: bOGUS_LVs".

WARNING: 
~~~~~~~~

This module has suffered bit-rot; it is likely to yield lint errors
for Stg code that is currently perfectly acceptable for code
generation.  Solution: don't use it!  (KSW 2000-05).


%************************************************************************
%*									*
\subsection{``lint'' for various constructs}
%*									*
%************************************************************************

@lintStgBindings@ is the top-level interface function.

\begin{code}
lintStgBindings :: String -> [StgBinding] -> [StgBinding]

lintStgBindings whodunnit binds
  = {-# SCC "StgLint" #-}
    case (initL (lint_binds binds)) of
      Nothing  -> binds
      Just msg -> pprPanic "" (vcat [
			ptext SLIT("*** Stg Lint ErrMsgs: in") <+> text whodunnit <+> ptext SLIT("***"),
			msg,
			ptext SLIT("*** Offending Program ***"),
			pprStgBindings binds,
			ptext SLIT("*** End of Offense ***")])
  where
    lint_binds :: [StgBinding] -> LintM ()

    lint_binds [] = returnL ()
    lint_binds (bind:binds)
      = lintStgBinds bind 		`thenL` \ binders ->
	addInScopeVars binders (
	    lint_binds binds
	)
\end{code}


\begin{code}
lintStgArg :: StgArg -> LintM (Maybe Type)
lintStgArg (StgLitArg lit) = returnL (Just (literalType lit))
lintStgArg (StgVarArg v)   = lintStgVar v

lintStgVar v  = checkInScope v	`thenL_`
		returnL (Just (idType v))
\end{code}

\begin{code}
lintStgBinds :: StgBinding -> LintM [Id]		-- Returns the binders
lintStgBinds (StgNonRec binder rhs)
  = lint_binds_help (binder,rhs) 	`thenL_`
    returnL [binder]

lintStgBinds (StgRec pairs)
  = addInScopeVars binders (
	mapL lint_binds_help pairs `thenL_`
	returnL binders
    )
  where
    binders = [b | (b,_) <- pairs]

lint_binds_help (binder, rhs)
  = addLoc (RhsOf binder) (
	-- Check the rhs
	lintStgRhs rhs    `thenL` \ maybe_rhs_ty ->

	-- Check binder doesn't have unlifted type
	checkL (not (isUnLiftedType binder_ty))
	       (mkUnLiftedTyMsg binder rhs)		`thenL_`

	-- Check match to RHS type
	(case maybe_rhs_ty of
	  Nothing     -> returnL ()
	  Just rhs_ty -> checkTys  binder_ty
				   rhs_ty
				   (mkRhsMsg binder rhs_ty)
	)			`thenL_`

	returnL ()
    )
  where
    binder_ty = idType binder
\end{code}

\begin{code}
lintStgRhs :: StgRhs -> LintM (Maybe Type)

lintStgRhs (StgRhsClosure _ _ _ _ _ [] expr)
  = lintStgExpr expr

lintStgRhs (StgRhsClosure _ _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) (
    addInScopeVars binders (
	lintStgExpr expr   `thenMaybeL` \ body_ty ->
	returnL (Just (mkFunTys (map idType binders) body_ty))
    ))

lintStgRhs (StgRhsCon _ con args)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkRhsConMsg con_ty arg_tys)
  where
    con_ty = dataConRepType con
\end{code}

\begin{code}
lintStgExpr :: StgExpr -> LintM (Maybe Type)	-- Nothing if error found

lintStgExpr (StgLit l) = returnL (Just (literalType l))

lintStgExpr e@(StgApp fun args)
  = lintStgVar fun		`thenMaybeL` \ fun_ty  ->
    mapMaybeL lintStgArg args	`thenL`      \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintStgExpr e@(StgConApp con args)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkFunAppMsg con_ty arg_tys e)
  where
    con_ty = dataConRepType con

lintStgExpr e@(StgOpApp (StgFCallOp _ _) args res_ty)
  = 	-- We don't have enough type information to check
	-- the application; ToDo
    mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    returnL (Just res_ty)

lintStgExpr e@(StgOpApp (StgPrimOp op) args _)
  = mapMaybeL lintStgArg args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp op_ty arg_tys (mkFunAppMsg op_ty arg_tys e)
  where
    op_ty = primOpType op

lintStgExpr (StgLam _ bndrs _)
  = addErrL (ptext SLIT("Unexpected StgLam") <+> ppr bndrs)	`thenL_`
    returnL Nothing

lintStgExpr (StgLet binds body)
  = lintStgBinds binds		`thenL` \ binders ->
    addLoc (BodyOfLetRec binders) (
    addInScopeVars binders (
	lintStgExpr body
    ))

lintStgExpr (StgLetNoEscape _ _ binds body)
  = lintStgBinds binds		`thenL` \ binders ->
    addLoc (BodyOfLetRec binders) (
    addInScopeVars binders (
	lintStgExpr body
    ))

lintStgExpr (StgSCC _ expr)	= lintStgExpr expr

lintStgExpr e@(StgCase scrut _ _ bndr _ alts_type alts)
  = lintStgExpr scrut		`thenMaybeL` \ _ ->

    (case alts_type of
	AlgAlt tc    -> check_bndr tc
	PrimAlt tc   -> check_bndr tc
	UbxTupAlt tc -> check_bndr tc
	PolyAlt      -> returnL ()
    )							`thenL_`
	
    (trace (showSDoc (ppr e)) $ 
	-- we only allow case of tail-call or primop.
    (case scrut of
	StgApp _ _    -> returnL ()
	StgConApp _ _ -> returnL ()
	StgOpApp _ _ _ -> returnL ()
	other -> addErrL (mkCaseOfCaseMsg e))   `thenL_`

    addInScopeVars [bndr] (lintStgAlts alts scrut_ty)
    )
  where
    scrut_ty	  = idType bndr
    bad_bndr      = mkDefltMsg bndr
    check_bndr tc = case splitTyConApp_maybe scrut_ty of
			Just (bndr_tc, _) -> checkL (tc == bndr_tc) bad_bndr
			Nothing		  -> addErrL bad_bndr


lintStgAlts :: [StgAlt]
	    -> Type 		-- Type of scrutinee
	    -> LintM (Maybe Type)	-- Type of alternatives

lintStgAlts alts scrut_ty
  = mapL (lintAlt scrut_ty) alts 	`thenL` \ maybe_result_tys ->

	 -- Check the result types
    case catMaybes (maybe_result_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`thenL_`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg alts)

lintAlt scrut_ty (DEFAULT, _, _, rhs)
 = lintStgExpr rhs

lintAlt scrut_ty (LitAlt lit, _, _, rhs)
 = checkTys (literalType lit) scrut_ty (mkAltMsg1 scrut_ty)	`thenL_`
   lintStgExpr rhs

lintAlt scrut_ty (DataAlt con, args, _, rhs)
  = (case splitTyConApp_maybe scrut_ty of
      Just (tycon, tys_applied) | isAlgTyCon tycon && 
				  not (isNewTyCon tycon) ->
	 let
	   cons    = tyConDataCons tycon
	   arg_tys = dataConArgTys con tys_applied
		-- This almost certainly does not work for existential constructors
	 in
	 checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con) `thenL_`
	 checkL (equalLength arg_tys args) (mkAlgAltMsg3 con args)
								 `thenL_`
	 mapL check (zipEqual "lintAlgAlt:stg" arg_tys args)	 `thenL_`
	 returnL ()
      other ->
	 addErrL (mkAltMsg1 scrut_ty)
    )								 `thenL_`
    addInScopeVars args 	(
	 lintStgExpr rhs
    )
  where
    check (ty, arg) = checkTys ty (idType arg) (mkAlgAltMsg4 ty arg)

    -- elem: yes, the elem-list here can sometimes be long-ish,
    -- but as it's use-once, probably not worth doing anything different
    -- We give it its own copy, so it isn't overloaded.
    elem _ []	    = False
    elem x (y:ys)   = x==y || elem x ys
\end{code}


%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = [LintLocInfo] 	-- Locations
	    -> IdSet		-- Local vars in scope
	    -> Bag Message	-- Error messages so far
	    -> (a, Bag Message)	-- Result and error messages (if any)

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf [Id]	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders

dumpLoc (RhsOf v) =
  (srcLocSpan (getSrcLoc v), ptext SLIT(" [RHS of ") <> pp_binders [v] <> char ']' )
dumpLoc (LambdaBodyOf bs) =
  (srcLocSpan (getSrcLoc (head bs)), ptext SLIT(" [in body of lambda with binders ") <> pp_binders bs <> char ']' )

dumpLoc (BodyOfLetRec bs) =
  (srcLocSpan (getSrcLoc (head bs)), ptext SLIT(" [in body of letrec with binders ") <> pp_binders bs <> char ']' )


pp_binders :: [Id] -> SDoc
pp_binders bs
  = sep (punctuate comma (map pp_binder bs))
  where
    pp_binder b
      = hsep [ppr b, dcolon, ppr (idType b)]
\end{code}

\begin{code}
initL :: LintM a -> Maybe Message
initL m
  = case (m [] emptyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just (vcat (punctuate (text "") (bagToList errs)))
    }

returnL :: a -> LintM a
returnL r loc scope errs = (r, errs)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k loc scope errs
  = case m loc scope errs of
      (r, errs') -> k r loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k loc scope errs
  = case m loc scope errs of
      (_, errs') -> k loc scope errs'

thenMaybeL :: LintM (Maybe a) -> (a -> LintM (Maybe b)) -> LintM (Maybe b)
thenMaybeL m k loc scope errs
  = case m loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just r,  errs2) -> k r loc scope errs2

mapL :: (a -> LintM b) -> [a] -> LintM [b]
mapL f [] = returnL []
mapL f (x:xs)
  = f x 	`thenL` \ r ->
    mapL f xs	`thenL` \ rs ->
    returnL (r:rs)

mapMaybeL :: (a -> LintM (Maybe b)) -> [a] -> LintM (Maybe [b])
	-- Returns Nothing if anything fails
mapMaybeL f [] = returnL (Just [])
mapMaybeL f (x:xs)
  = f x	    	    `thenMaybeL` \ r ->
    mapMaybeL f xs  `thenMaybeL` \ rs ->
    returnL (Just (r:rs))
\end{code}

\begin{code}
checkL :: Bool -> Message -> LintM ()
checkL True  msg loc scope errs = ((), errs)
checkL False msg loc scope errs = ((), addErr errs msg loc)

addErrL :: Message -> LintM ()
addErrL msg loc scope errs = ((), addErr errs msg loc)

addErr :: Bag Message -> Message -> [LintLocInfo] -> Bag Message

addErr errs_so_far msg locs
  = errs_so_far `snocBag` mk_msg locs
  where
    mk_msg (loc:_) = let (l,hdr) = dumpLoc loc 
		     in  mkLocMessage l (hdr $$ msg)
    mk_msg []      = msg

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m loc scope errs
  = m (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m loc scope errs
  = -- We check if these "new" ids are already
    -- in scope, i.e., we have *shadowing* going on.
    -- For now, it's just a "trace"; we may make
    -- a real error out of it...
    let
	new_set = mkVarSet ids
    in
--  After adding -fliberate-case, Simon decided he likes shadowed
--  names after all.  WDP 94/07
--  (if isEmptyVarSet shadowed
--  then id
--  else pprTrace "Shadowed vars:" (ppr (varSetElems shadowed))) $
    m loc (scope `unionVarSet` new_set) errs
\end{code}

Checking function applications: we only check that the type has the
right *number* of arrows, we don't actually compare the types.  This
is because we can't expect the types to be equal - the type
applications and type lambdas that we use to calculate accurate types
have long since disappeared.

\begin{code}
checkFunApp :: Type 		    -- The function type
	    -> [Type]		    -- The arg type(s)
	    -> Message		    -- Error messgae
	    -> LintM (Maybe Type)   -- The result type

checkFunApp fun_ty arg_tys msg loc scope errs
  = cfa res_ty expected_arg_tys arg_tys
  where
    (expected_arg_tys, res_ty) = splitFunTys (dropForAlls fun_ty)

    cfa res_ty expected []	-- Args have run out; that's fine
      = (Just (mkFunTys expected res_ty), errs)

    cfa res_ty [] arg_tys	-- Expected arg tys ran out first;
				-- first see if res_ty is a tyvar template;
				-- otherwise, maybe res_ty is a
				-- dictionary type which is actually a function?
      | isTyVarTy res_ty
      = (Just res_ty, errs)
      | otherwise
      = case splitFunTys res_ty of
	  ([], _) 		  -> (Nothing, addErr errs msg loc)	-- Too many args
	  (new_expected, new_res) -> cfa new_res new_expected arg_tys

    cfa res_ty (expected_arg_ty:expected_arg_tys) (arg_ty:arg_tys)
      = cfa res_ty expected_arg_tys arg_tys
\end{code}

\begin{code}
checkInScope :: Id -> LintM ()
checkInScope id loc scope errs
  = if isLocalId id && not (id `elemVarSet` scope) then
	((), addErr errs (hsep [ppr id, ptext SLIT("is out of scope")]) loc)
    else
	((), errs)

checkTys :: Type -> Type -> Message -> LintM ()
checkTys ty1 ty2 msg loc scope errs
  = -- if (ty1 == ty2) then
    ((), errs)
    -- else ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: [StgAlt] -> Message
mkCaseAltMsg alts
  = ($$) (text "In some case alternatives, type of alternatives not all same:")
	    (empty) -- LATER: ppr alts

mkDefltMsg :: Id -> Message
mkDefltMsg bndr
  = ($$) (ptext SLIT("Binder of a case expression doesn't match type of scrutinee:"))
	    (panic "mkDefltMsg")

mkFunAppMsg :: Type -> [Type] -> StgExpr -> Message
mkFunAppMsg fun_ty arg_tys expr
  = vcat [text "In a function application, function type doesn't match arg types:",
	      hang (ptext SLIT("Function type:")) 4 (ppr fun_ty),
	      hang (ptext SLIT("Arg types:")) 4 (vcat (map (ppr) arg_tys)),
	      hang (ptext SLIT("Expression:")) 4 (ppr expr)]

mkRhsConMsg :: Type -> [Type] -> Message
mkRhsConMsg fun_ty arg_tys
  = vcat [text "In a RHS constructor application, con type doesn't match arg types:",
	      hang (ptext SLIT("Constructor type:")) 4 (ppr fun_ty),
	      hang (ptext SLIT("Arg types:")) 4 (vcat (map (ppr) arg_tys))]

mkAltMsg1 :: Type -> Message
mkAltMsg1 ty
  = ($$) (text "In a case expression, type of scrutinee does not match patterns")
	 (ppr ty)

mkAlgAltMsg2 :: Type -> DataCon -> Message
mkAlgAltMsg2 ty con
  = vcat [
	text "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
	ppr ty,
	ppr con
    ]

mkAlgAltMsg3 :: DataCon -> [Id] -> Message
mkAlgAltMsg3 con alts
  = vcat [
	text "In some algebraic case alternative, number of arguments doesn't match constructor:",
	ppr con,
	ppr alts
    ]

mkAlgAltMsg4 :: Type -> Id -> Message
mkAlgAltMsg4 ty arg
  = vcat [
	text "In some algebraic case alternative, type of argument doesn't match data constructor:",
	ppr ty,
	ppr arg
    ]

mkCaseOfCaseMsg :: StgExpr -> Message
mkCaseOfCaseMsg e
  = text "Case of non-tail-call:" $$ ppr e

mkRhsMsg :: Id -> Type -> Message
mkRhsMsg binder ty
  = vcat [hsep [ptext SLIT("The type of this binder doesn't match the type of its RHS:"),
		     ppr binder],
	      hsep [ptext SLIT("Binder's type:"), ppr (idType binder)],
	      hsep [ptext SLIT("Rhs type:"), ppr ty]
	     ]

mkUnLiftedTyMsg binder rhs
  = (ptext SLIT("Let(rec) binder") <+> quotes (ppr binder) <+> 
     ptext SLIT("has unlifted type") <+> quotes (ppr (idType binder)))
    $$
    (ptext SLIT("RHS:") <+> ppr rhs)
\end{code}
