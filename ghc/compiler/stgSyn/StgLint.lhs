%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[StgLint]{A ``lint'' pass to check for Stg correctness}

\begin{code}
#include "HsVersions.h"

module StgLint (
	lintStgBindings,
	
	PprStyle, StgBinding, PlainStgBinding(..), Id
    ) where

IMPORT_Trace

import AbsPrel		( typeOfPrimOp, mkFunTy, PrimOp(..), PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType
import Bag
import BasicLit		( typeOfBasicLit, BasicLit )
import Id		( getIdUniType, isNullaryDataCon, isDataCon,
			  isBottomingId,
			  getInstantiatedDataConSig, Id
			  IF_ATTACK_PRAGMAS(COMMA bottomIsGuaranteed)
			)
import Maybes
import Outputable
import Pretty
import SrcLoc		( SrcLoc )
import StgSyn
import UniqSet
import Util

infixr 9 `thenL`, `thenL_`, `thenMaybeL`, `thenMaybeL_`
\end{code}

Checks for 
	(a) *some* type errors
	(b) locally-defined variables used but not defined

%************************************************************************
%*									*
\subsection{``lint'' for various constructs}
%*									*
%************************************************************************

@lintStgBindings@ is the top-level interface function.

\begin{code}
lintStgBindings :: PprStyle -> String -> [PlainStgBinding] -> [PlainStgBinding]

lintStgBindings sty whodunnit binds
  = BSCC("StgLint")
    case (initL (lint_binds binds)) of
      Nothing  -> binds
      Just msg -> pprPanic "" (ppAboves [
			ppStr ("*** Stg Lint Errors: in "++whodunnit++" ***"),
			msg sty,
			ppStr "*** Offending Program ***",
			ppAboves (map (pprPlainStgBinding sty) binds),
			ppStr "*** End of Offense ***"])
    ESCC
  where
    lint_binds :: [PlainStgBinding] -> LintM ()

    lint_binds [] = returnL ()
    lint_binds (bind:binds) 
      = lintStgBinds bind 		`thenL` \ binders ->
	addInScopeVars binders (
	    lint_binds binds
	)
\end{code}


\begin{code}
lintStgAtom :: PlainStgAtom -> LintM (Maybe UniType)

lintStgAtom (StgLitAtom lit)       = returnL (Just (typeOfBasicLit lit))
lintStgAtom a@(StgVarAtom v)
  = checkInScope v	`thenL_`
    returnL (Just (getIdUniType v))
\end{code}

\begin{code}
lintStgBinds :: PlainStgBinding -> LintM [Id]		-- Returns the binders
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

	-- Check match to RHS type
	(case maybe_rhs_ty of
	  Nothing     -> returnL ()
	  Just rhs_ty -> checkTys (getIdUniType binder) 
				   rhs_ty 
				   (mkRhsMsg binder rhs_ty)
	)			`thenL_` 

	returnL ()
    )
\end{code}

\begin{code}
lintStgRhs :: PlainStgRhs -> LintM (Maybe UniType)

lintStgRhs (StgRhsClosure _ _ _ _ binders expr)
  = addLoc (LambdaBodyOf binders) (
    addInScopeVars binders (
	lintStgExpr expr   `thenMaybeL` \ body_ty ->
	returnL (Just (foldr (mkFunTy . getIdUniType) body_ty binders))
    ))

lintStgRhs (StgRhsCon _ con args)
  = mapMaybeL lintStgAtom args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkRhsConMsg con_ty arg_tys)
  where
    con_ty = getIdUniType con
\end{code}

\begin{code}
lintStgExpr :: PlainStgExpr -> LintM (Maybe UniType)	-- Nothing if error found

lintStgExpr e@(StgApp fun args _)
  = lintStgAtom fun		`thenMaybeL` \ fun_ty  ->
    mapMaybeL lintStgAtom args	`thenL`      \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintStgExpr e@(StgConApp con args _)
  = mapMaybeL lintStgAtom args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_ty arg_tys (mkFunAppMsg con_ty arg_tys e)
  where
    con_ty = getIdUniType con

lintStgExpr e@(StgPrimApp op args _)
  = mapMaybeL lintStgAtom args	`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing      -> returnL Nothing
      Just arg_tys -> checkFunApp op_ty arg_tys (mkFunAppMsg op_ty arg_tys e)
  where
    op_ty = typeOfPrimOp op

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

lintStgExpr (StgSCC _ _ expr)	= lintStgExpr expr

lintStgExpr e@(StgCase scrut _ _ _ alts)
  = lintStgExpr scrut		`thenMaybeL` \ _ ->

	-- Check that it is a data type
    case getUniDataTyCon_maybe scrut_ty of
      Nothing -> addErrL (mkCaseDataConMsg e)	`thenL_`
		 returnL Nothing
      Just (tycon, _, _)
	      -> lintStgAlts alts scrut_ty tycon
  where
    scrut_ty = get_ty alts

    get_ty (StgAlgAlts  ty _ _) = ty
    get_ty (StgPrimAlts ty _ _) = ty
\end{code}

\begin{code}
lintStgAlts :: PlainStgCaseAlternatives
	     -> UniType 		-- Type of scrutinee
	     -> TyCon			-- TyCon pinned on the case
	     -> LintM (Maybe UniType)	-- Type of alternatives

lintStgAlts alts scrut_ty case_tycon
  = (case alts of
	 StgAlgAlts _ alg_alts deflt ->  
	   chk_non_abstract_type case_tycon	`thenL_`
	   mapL (lintAlgAlt scrut_ty) alg_alts 	`thenL` \ maybe_alt_tys ->
	   lintDeflt deflt scrut_ty		`thenL` \ maybe_deflt_ty ->
	   returnL (maybe_deflt_ty : maybe_alt_tys)

	 StgPrimAlts _ prim_alts deflt -> 
	   mapL (lintPrimAlt scrut_ty) prim_alts `thenL` \ maybe_alt_tys ->
	   lintDeflt deflt scrut_ty		 `thenL` \ maybe_deflt_ty ->
	   returnL (maybe_deflt_ty : maybe_alt_tys)
    )						 `thenL` \ maybe_result_tys ->
	 -- Check the result types
    case catMaybes (maybe_result_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`thenL_`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg alts)
  where
    chk_non_abstract_type tycon
      = case (getTyConFamilySize tycon) of
	  Nothing -> addErrL (mkCaseAbstractMsg tycon)
	  Just  _ -> returnL () -- that's cool

lintAlgAlt scrut_ty (con, args, _, rhs)
  = (case getUniDataTyCon_maybe scrut_ty of
      Nothing -> 
	 addErrL (mkAlgAltMsg1 scrut_ty)
      Just (tycon, tys_applied, cons) ->
	 let
	   (_, arg_tys, _) = getInstantiatedDataConSig con tys_applied
	 in
	 checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con) `thenL_`
	 checkL (length arg_tys == length args) (mkAlgAltMsg3 con args)	
								 `thenL_`
	 mapL check (arg_tys `zipEqual` args)			 `thenL_`
	 returnL ()
    )								 `thenL_`
    addInScopeVars args 	(
	 lintStgExpr rhs
    )
  where
    check (ty, arg) = checkTys ty (getIdUniType arg) (mkAlgAltMsg4 ty arg)

    -- elem: yes, the elem-list here can sometimes be long-ish,
    -- but as it's use-once, probably not worth doing anything different
    -- We give it its own copy, so it isn't overloaded.
    elem _ []	    = False
    elem x (y:ys)   = x==y || elem x ys

lintPrimAlt scrut_ty alt@(lit,rhs)
 = checkTys (typeOfBasicLit lit) scrut_ty (mkPrimAltMsg alt)	`thenL_`
   lintStgExpr rhs
   
lintDeflt StgNoDefault scrut_ty = returnL Nothing
lintDeflt deflt@(StgBindDefault binder _ rhs) scrut_ty 
  = checkTys (getIdUniType binder) scrut_ty (mkDefltMsg deflt)	`thenL_`
    addInScopeVars [binder] (
	lintStgExpr rhs
    )
\end{code}


%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = [LintLocInfo] 	-- Locations
	    -> UniqSet Id	-- Local vars in scope
	    -> Bag ErrMsg	-- Error messages so far
	    -> (a, Bag ErrMsg)	-- Result and error messages (if any)

type ErrMsg = PprStyle -> Pretty

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf [Id]	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders

instance Outputable LintLocInfo where
    ppr sty (RhsOf v)
      = ppBesides [ppr sty (getSrcLoc v), ppStr ": [RHS of ", pp_binders sty [v], ppStr "]"]

    ppr sty (LambdaBodyOf bs)
      = ppBesides [ppr sty (getSrcLoc (head bs)),
		ppStr ": [in body of lambda with binders ", pp_binders sty bs, ppStr "]"]

    ppr sty (BodyOfLetRec bs)
      = ppBesides [ppr sty (getSrcLoc (head bs)),
		ppStr ": [in body of letrec with binders ", pp_binders sty bs, ppStr "]"]

pp_binders :: PprStyle -> [Id] -> Pretty
pp_binders sty bs
  = ppInterleave ppComma (map pp_binder bs)
  where
    pp_binder b
      = ppCat [ppr sty b, ppStr "::", ppr sty (getIdUniType b)]
\end{code}

\begin{code}
initL :: LintM a -> Maybe ErrMsg
initL m
  = case (m [] emptyUniqSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just ( \ sty ->
	  ppAboves [ msg sty | msg <- bagToList errs ]
	)
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

thenMaybeL_ :: LintM (Maybe a) -> LintM (Maybe b) -> LintM (Maybe b)
thenMaybeL_ m k loc scope errs
  = case m loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just _,  errs2) -> k loc scope errs2

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
checkL :: Bool -> ErrMsg -> LintM ()
checkL True  msg loc scope errs = ((), errs)
checkL False msg loc scope errs = ((), addErr errs msg loc)

addErrL :: ErrMsg -> LintM ()
addErrL msg loc scope errs = ((), addErr errs msg loc)

addErr :: Bag ErrMsg -> ErrMsg -> [LintLocInfo] -> Bag ErrMsg

addErr errs_so_far msg locs
  = errs_so_far `snocBag` ( \ sty ->
    ppHang (ppr sty (head locs)) 4 (msg sty)
    )

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
	new_set = mkUniqSet ids

	shadowed = scope `intersectUniqSets` new_set
    in
--  After adding -fliberate-case, Simon decided he likes shadowed
--  names after all.  WDP 94/07
--  (if isEmptyUniqSet shadowed
--  then id
--  else pprTrace "Shadowed vars:" (ppr PprDebug (uniqSetToList shadowed))) (
    m loc (scope `unionUniqSets` new_set) errs
--  )
\end{code}

\begin{code}
checkFunApp :: UniType 		-- The function type
	    -> [UniType] 	-- The arg type(s)
	    -> ErrMsg 		-- Error messgae
	    -> LintM (Maybe UniType)	-- The result type

checkFunApp fun_ty arg_tys msg loc scope errs
  = cfa res_ty expected_arg_tys arg_tys
  where
    (_, expected_arg_tys, res_ty) = splitTypeWithDictsAsArgs fun_ty

    cfa res_ty expected []	-- Args have run out; that's fine
      = (Just (glueTyArgs expected res_ty), errs)

    cfa res_ty [] arg_tys	-- Expected arg tys ran out first;
				-- first see if res_ty is a tyvar template;
				-- otherwise, maybe res_ty is a 
				-- dictionary type which is actually a function?
      | isTyVarTemplateTy res_ty
      = (Just res_ty, errs)
      | otherwise
      = case splitTyArgs (unDictifyTy res_ty) of
	  ([], _) 		  -> (Nothing, addErr errs msg loc)	-- Too many args
	  (new_expected, new_res) -> cfa new_res new_expected arg_tys

    cfa res_ty (expected_arg_ty:expected_arg_tys) (arg_ty:arg_tys)
      = case (sleazy_cmp_ty expected_arg_ty arg_ty) of
	  EQ_ -> cfa res_ty expected_arg_tys arg_tys
	  _   -> (Nothing, addErr errs msg loc) -- Arg mis-match
\end{code}

\begin{code}
checkInScope :: Id -> LintM ()
checkInScope id loc scope errs
  = if isLocallyDefined id && not (isDataCon id) && not (id `elementOfUniqSet` scope) then
	((), addErr errs (\ sty -> ppCat [ppr sty id, ppStr "is out of scope"]) loc)
    else
	((), errs)

checkTys :: UniType -> UniType -> ErrMsg -> LintM ()
checkTys ty1 ty2 msg loc scope errs
  = case (sleazy_cmp_ty ty1 ty2) of
      EQ_   -> ((), errs)
      other -> ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: PlainStgCaseAlternatives -> ErrMsg
mkCaseAltMsg alts sty
  = ppAbove (ppStr "In some case alternatives, type of alternatives not all same:")
	    -- LATER: (ppr sty alts)
	    (panic "mkCaseAltMsg")

mkCaseDataConMsg :: PlainStgExpr -> ErrMsg
mkCaseDataConMsg expr sty
  = ppAbove (ppStr "A case scrutinee not a type-constructor type:")
	    (pp_expr sty expr)

mkCaseAbstractMsg :: TyCon -> ErrMsg
mkCaseAbstractMsg tycon sty
  = ppAbove (ppStr "An algebraic case on an abstract type:")
	    (ppr sty tycon)

mkDefltMsg :: PlainStgCaseDefault -> ErrMsg
mkDefltMsg deflt sty
  = ppAbove (ppStr "Binder in default case of a case expression doesn't match type of scrutinee:")
	    --LATER: (ppr sty deflt)
	    (panic "mkDefltMsg")

mkFunAppMsg :: UniType -> [UniType] -> PlainStgExpr -> ErrMsg
mkFunAppMsg fun_ty arg_tys expr sty
  = ppAboves [ppStr "In a function application, function type doesn't match arg types:",
	      ppHang (ppStr "Function type:") 4 (ppr sty fun_ty),
	      ppHang (ppStr "Arg types:") 4 (ppAboves (map (ppr sty) arg_tys)),
	      ppHang (ppStr "Expression:") 4 (pp_expr sty expr)]

mkRhsConMsg :: UniType -> [UniType] -> ErrMsg
mkRhsConMsg fun_ty arg_tys sty
  = ppAboves [ppStr "In a RHS constructor application, con type doesn't match arg types:",
	      ppHang (ppStr "Constructor type:") 4 (ppr sty fun_ty),
	      ppHang (ppStr "Arg types:") 4 (ppAboves (map (ppr sty) arg_tys))]

mkUnappTyMsg :: Id -> UniType -> ErrMsg
mkUnappTyMsg var ty sty
  = ppAboves [ppStr "Variable has a for-all type, but isn't applied to any types.",
	      ppBeside (ppStr "Var:      ") (ppr sty var),
	      ppBeside (ppStr "Its type: ") (ppr sty ty)]

mkAlgAltMsg1 :: UniType -> ErrMsg
mkAlgAltMsg1 ty sty
  = ppAbove (ppStr "In some case statement, type of scrutinee is not a data type:")
	    (ppr sty ty)

mkAlgAltMsg2 :: UniType -> Id -> ErrMsg
mkAlgAltMsg2 ty con sty
  = ppAboves [
	ppStr "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
	ppr sty ty,
	ppr sty con
    ]

mkAlgAltMsg3 :: Id -> [Id] -> ErrMsg
mkAlgAltMsg3 con alts sty
  = ppAboves [
	ppStr "In some algebraic case alternative, number of arguments doesn't match constructor:",
	ppr sty con,
	ppr sty alts
    ]

mkAlgAltMsg4 :: UniType -> Id -> ErrMsg
mkAlgAltMsg4 ty arg sty
  = ppAboves [
	ppStr "In some algebraic case alternative, type of argument doesn't match data constructor:",
	ppr sty ty,
	ppr sty arg
    ]

mkPrimAltMsg :: (BasicLit, PlainStgExpr) -> ErrMsg
mkPrimAltMsg alt sty
  = ppAbove (ppStr "In a primitive case alternative, type of literal doesn't match type of scrutinee:")
	    (ppr sty alt)

mkRhsMsg :: Id -> UniType -> ErrMsg
mkRhsMsg binder ty sty
  = ppAboves [ppCat [ppStr "The type of this binder doesn't match the type of its RHS:", 
		     ppr sty binder],
	      ppCat [ppStr "Binder's type:", ppr sty (getIdUniType binder)],
	      ppCat [ppStr "Rhs type:", ppr sty ty]
	     ]

pp_expr :: PprStyle -> PlainStgExpr -> Pretty
pp_expr sty expr = ppr sty expr

sleazy_cmp_ty ty1 ty2
	-- NB: probably severe overkill (WDP 95/04)
  = case (splitTypeWithDictsAsArgs ty1) of { (_,tyargs1,tyres1) ->
    case (splitTypeWithDictsAsArgs ty2) of { (_,tyargs2,tyres2) ->
    let
	ty11 = glueTyArgs tyargs1 tyres1
	ty22 = glueTyArgs tyargs2 tyres2
    in
    cmpUniType False{-!!!NOT PROPERLY!!!-} ty11 ty22
    }}
\end{code}
