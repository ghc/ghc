%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
#include "HsVersions.h"

module CoreLint (
	lintCoreBindings,
	lintUnfolding,
	
	PprStyle, CoreBinding, PlainCoreBinding(..), Id
    ) where

IMPORT_Trace

import AbsPrel		( typeOfPrimOp, mkFunTy, PrimOp(..), PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType
import Bag
import BasicLit		( typeOfBasicLit, BasicLit )
import CoreSyn		( pprCoreBinding ) -- ToDo: correctly
import Id		( getIdUniType, isNullaryDataCon, isBottomingId,
			  getInstantiatedDataConSig, Id
			  IF_ATTACK_PRAGMAS(COMMA bottomIsGuaranteed)
			)
import Maybes
import Outputable
import PlainCore
import Pretty
import SrcLoc		( SrcLoc )
import UniqSet
import Util

infixr 9 `thenL`, `thenL_`, `thenMaybeL`, `thenMaybeL_`
\end{code}

Checks for 
	(a) type errors
	(b) locally-defined variables used but not defined

Doesn't check for out-of-scope type variables, because they can
legitimately arise.  Eg
\begin{verbatim}
	k = /\a b -> \x::a y::b -> x
	f = /\c -> \z::c -> k c w z (error w "foo")
\end{verbatim}
Here \tr{w} is just a free type variable.

%************************************************************************
%*									*
\subsection{``lint'' for various constructs}
%*									*
%************************************************************************

@lintCoreBindings@ is the top-level interface function.

\begin{code}
lintCoreBindings :: PprStyle -> String -> Bool -> [PlainCoreBinding] -> [PlainCoreBinding]

lintCoreBindings sty whodunnit spec_done binds
  = BSCC("CoreLint")
    case (initL (lint_binds binds) spec_done) of
      Nothing  -> binds
      Just msg -> pprPanic "" (ppAboves [
			ppStr ("*** Core Lint Errors: in "++whodunnit++" ***"),
			msg sty,
			ppStr "*** Offending Program ***",
			ppAboves (map (pprCoreBinding sty pprBigCoreBinder pprTypedCoreBinder ppr) binds),
			ppStr "*** End of Offense ***"])
    ESCC
  where
    lint_binds :: [PlainCoreBinding] -> LintM ()

    lint_binds [] = returnL ()
    lint_binds (bind:binds) 
      = lintCoreBinds bind 		`thenL` \ binders ->
	addInScopeVars binders (
	    lint_binds binds
	)
\end{code}

We use this to check all unfoldings that come in from interfaces
(it is very painful to catch errors otherwise):
\begin{code}
lintUnfolding :: SrcLoc -> PlainCoreExpr -> Maybe PlainCoreExpr

lintUnfolding locn expr
  = case (initL (addLoc (ImportedUnfolding locn) (lintCoreExpr expr)) True{-pretend spec done-}) of
      Nothing  -> Just expr
      Just msg -> pprTrace "WARNING: Discarded bad unfolding from interface:\n"
			   (ppAboves [msg PprForUser,
				      ppStr "*** Bad unfolding ***",
				      ppr PprDebug expr,
				      ppStr "*** End unfolding ***"])
		  Nothing
\end{code}

\begin{code}
lintCoreAtom :: PlainCoreAtom -> LintM (Maybe UniType)

lintCoreAtom (CoLitAtom lit)       = returnL (Just (typeOfBasicLit lit))
lintCoreAtom a@(CoVarAtom v)
  = checkInScope v	`thenL_`
    returnL (Just (getIdUniType v))
\end{code}

\begin{code}
lintCoreBinds :: PlainCoreBinding -> LintM [Id]		-- Returns the binders
lintCoreBinds (CoNonRec binder rhs)
  = lint_binds_help (binder,rhs) 	`thenL_`
    returnL [binder]

lintCoreBinds (CoRec pairs) 
  = addInScopeVars binders (
	mapL lint_binds_help pairs `thenL_`
	returnL binders
    )
  where
    binders = [b | (b,_) <- pairs]

lint_binds_help (binder,rhs)
  = addLoc (RhsOf binder) (
	-- Check the rhs
	lintCoreExpr rhs    	`thenL` \ maybe_rhs_ty ->

	-- Check match to RHS type
	(case maybe_rhs_ty of
	  Nothing     -> returnL ()
	  Just rhs_ty -> checkTys (getIdUniType binder) 
				   rhs_ty 
				   (mkRhsMsg binder rhs_ty)
	)			`thenL_` 

	-- Check not isPrimType
	checkIfSpecDoneL (not (isPrimType (getIdUniType binder)))
	       		 (mkRhsPrimMsg binder rhs)
				`thenL_`

	-- Check unfolding, if any
	-- Blegh. This is tricky, because the unfolding is a SimplifiableCoreExpr
	-- Give up for now

	returnL ()
    )
\end{code}

\begin{code}
lintCoreExpr :: PlainCoreExpr -> LintM (Maybe UniType)	-- Nothing if error found

lintCoreExpr (CoVar var)
  = checkInScope var	`thenL_`
    returnL (Just ty)
{-
    case (splitForalls ty) of { (tyvars, _) ->
    if null tyvars then
	returnL (Just ty)
    else
	addErrL (mkUnappTyMsg var ty)	`thenL_`
	returnL Nothing
    }
-}
  where
    ty = getIdUniType var

lintCoreExpr (CoLit lit)	= returnL (Just (typeOfBasicLit lit))
lintCoreExpr (CoSCC label expr)	= lintCoreExpr expr

lintCoreExpr (CoLet binds body)	
  = lintCoreBinds binds		`thenL` \ binders ->
    ASSERT(not (null binders))
    addLoc (BodyOfLetRec binders) (
    addInScopeVars binders (
	lintCoreExpr body
    ))

lintCoreExpr e@(CoCon con tys args)
  = checkTyApp con_ty tys (mkTyAppMsg e)    	`thenMaybeL` \ con_tau_ty ->
    -- Note: no call to checkSpecTyApp for constructor type args
    mapMaybeL lintCoreAtom args			`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing 	    -> returnL Nothing
      Just arg_tys  -> checkFunApp con_tau_ty arg_tys (mkFunAppMsg con_tau_ty arg_tys e)
  where
    con_ty = getIdUniType con

lintCoreExpr e@(CoPrim op tys args)
  = checkTyApp op_ty tys (mkTyAppMsg e)		`thenMaybeL` \ op_tau_ty ->
    -- ToDo: checkSpecTyApp e tys (mkSpecTyAppMsg e)	`thenMaybeL_`
    mapMaybeL lintCoreAtom args			`thenL` \ maybe_arg_tys ->
    case maybe_arg_tys of
      Nothing -> returnL Nothing
      Just arg_tys -> checkFunApp op_tau_ty arg_tys (mkFunAppMsg op_tau_ty arg_tys e)
  where
    op_ty = typeOfPrimOp op

lintCoreExpr e@(CoApp fun arg)
  = lce e []
  where
    lce (CoApp fun arg) arg_tys = lintCoreAtom arg	`thenMaybeL` \ arg_ty ->
			          lce fun (arg_ty:arg_tys)

    lce other_fun arg_tys = lintCoreExpr other_fun	`thenMaybeL` \ fun_ty ->
			    checkFunApp fun_ty arg_tys (mkFunAppMsg fun_ty arg_tys e)

lintCoreExpr e@(CoTyApp fun ty_arg)
  = lce e []
  where
    lce (CoTyApp fun ty_arg) ty_args = lce fun (ty_arg:ty_args)

    lce other_fun ty_args = lintCoreExpr other_fun	`thenMaybeL` \ fun_ty ->
			    checkTyApp fun_ty ty_args (mkTyAppMsg e)
							`thenMaybeL` \ res_ty ->
			    checkSpecTyApp other_fun ty_args (mkSpecTyAppMsg e)
							`thenMaybeL_`
			    returnL (Just res_ty)

lintCoreExpr (CoLam binders expr)
  = ASSERT (not (null binders))
    addLoc (LambdaBodyOf binders) (
    addInScopeVars binders (
	lintCoreExpr expr   `thenMaybeL` \ body_ty ->
	returnL (Just (foldr (mkFunTy . getIdUniType) body_ty binders))
    ))

lintCoreExpr (CoTyLam tyvar expr)
  = lintCoreExpr expr		`thenMaybeL` \ body_ty ->
    case quantifyTy [tyvar] body_ty of
      (_, ty) -> returnL (Just ty) -- not worried about the TyVarTemplates that come back

lintCoreExpr e@(CoCase scrut alts)
 = lintCoreExpr scrut		`thenMaybeL` \ scrut_ty ->

	-- Check that it is a data type
   case getUniDataTyCon_maybe scrut_ty of
	Nothing -> addErrL (mkCaseDataConMsg e)	`thenL_`
		   returnL Nothing
	Just (tycon, _, _)
		-> lintCoreAlts alts scrut_ty tycon

lintCoreAlts :: PlainCoreCaseAlternatives
	     -> UniType 		-- Type of scrutinee
	     -> TyCon			-- TyCon pinned on the case
	     -> LintM (Maybe UniType)	-- Type of alternatives

lintCoreAlts alts scrut_ty case_tycon
  = (case alts of
	 CoAlgAlts alg_alts deflt ->  
	   chk_prim_type False case_tycon	`thenL_`
	   chk_non_abstract_type case_tycon	`thenL_`
	   mapL (lintAlgAlt scrut_ty) alg_alts 	`thenL` \ maybe_alt_tys ->
	   lintDeflt deflt scrut_ty		`thenL` \ maybe_deflt_ty ->
	   returnL (maybe_deflt_ty : maybe_alt_tys)

	 CoPrimAlts prim_alts deflt -> 
	   chk_prim_type True case_tycon	 `thenL_`
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
    chk_prim_type prim_required tycon
      = if (isPrimTyCon tycon == prim_required) then
	    returnL ()
	else
	    addErrL (mkCasePrimMsg prim_required tycon)

    chk_non_abstract_type tycon
      = case (getTyConFamilySize tycon) of
	  Nothing -> addErrL (mkCaseAbstractMsg tycon)
	  Just  _ -> returnL ()


lintAlgAlt scrut_ty (con,args,rhs)
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
	 lintCoreExpr rhs
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
   lintCoreExpr rhs
   
lintDeflt CoNoDefault scrut_ty = returnL Nothing
lintDeflt deflt@(CoBindDefault binder rhs) scrut_ty 
  = checkTys (getIdUniType binder) scrut_ty (mkDefltMsg deflt)	`thenL_`
    addInScopeVars [binder] (
	lintCoreExpr rhs
    )
\end{code}


%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = Bool		-- True <=> specialisation has been done
	    -> [LintLocInfo] 	-- Locations
	    -> UniqSet Id	-- Local vars in scope
	    -> Bag ErrMsg	-- Error messages so far
	    -> (a, Bag ErrMsg)	-- Result and error messages (if any)

type ErrMsg = PprStyle -> Pretty

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf [Id]	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)

instance Outputable LintLocInfo where
    ppr sty (RhsOf v)
      = ppBesides [ppr sty (getSrcLoc v), ppStr ": [RHS of ", pp_binders sty [v], ppStr "]"]

    ppr sty (LambdaBodyOf bs)
      = ppBesides [ppr sty (getSrcLoc (head bs)),
		ppStr ": [in body of lambda with binders ", pp_binders sty bs, ppStr "]"]

    ppr sty (BodyOfLetRec bs)
      = ppBesides [ppr sty (getSrcLoc (head bs)),
		ppStr ": [in body of letrec with binders ", pp_binders sty bs, ppStr "]"]

    ppr sty (ImportedUnfolding locn)
      = ppBeside (ppr sty locn) (ppStr ": [in an imported unfolding]")

pp_binders :: PprStyle -> [Id] -> Pretty
pp_binders sty bs
  = ppInterleave ppComma (map pp_binder bs)
  where
    pp_binder b
      = ppCat [ppr sty b, ppStr "::", ppr sty (getIdUniType b)]
\end{code}

\begin{code}
initL :: LintM a -> Bool -> Maybe ErrMsg
initL m spec_done
  = case (m spec_done [] emptyUniqSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just ( \ sty ->
	  ppAboves [ msg sty | msg <- bagToList errs ]
	)
    }

returnL :: a -> LintM a
returnL r spec loc scope errs = (r, errs)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k spec loc scope errs
  = case m spec loc scope errs of 
      (r, errs') -> k r spec loc scope errs'

thenL_ :: LintM a -> LintM b -> LintM b
thenL_ m k spec loc scope errs
  = case m spec loc scope errs of 
      (_, errs') -> k spec loc scope errs'

thenMaybeL :: LintM (Maybe a) -> (a -> LintM (Maybe b)) -> LintM (Maybe b)
thenMaybeL m k spec loc scope errs
  = case m spec loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just r,  errs2) -> k r spec loc scope errs2

thenMaybeL_ :: LintM (Maybe a) -> LintM (Maybe b) -> LintM (Maybe b)
thenMaybeL_ m k spec loc scope errs
  = case m spec loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just _,  errs2) -> k spec loc scope errs2

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
checkL True  msg spec loc scope errs = ((), errs)
checkL False msg spec loc scope errs = ((), addErr errs msg loc)

checkIfSpecDoneL :: Bool -> ErrMsg -> LintM ()
checkIfSpecDoneL True  msg spec  loc scope errs = ((), errs)
checkIfSpecDoneL False msg True  loc scope errs = ((), addErr errs msg loc)
checkIfSpecDoneL False msg False loc scope errs = ((), errs)

addErrL :: ErrMsg -> LintM ()
addErrL msg spec loc scope errs = ((), addErr errs msg loc)

addErr :: Bag ErrMsg -> ErrMsg -> [LintLocInfo] -> Bag ErrMsg

addErr errs_so_far msg locs
  = ASSERT (not (null locs))
    errs_so_far `snocBag` ( \ sty ->
    ppHang (ppr sty (head locs)) 4 (msg sty)
    )

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m spec loc scope errs
  = m spec (extra_loc:loc) scope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m spec loc scope errs
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
    m spec loc (scope `unionUniqSets` new_set) errs
--  )
\end{code}

\begin{code}
checkTyApp :: UniType
	   -> [UniType]
	   -> ErrMsg
	   -> LintM (Maybe UniType)

checkTyApp forall_ty ty_args msg spec_done loc scope errs
  = if (not spec_done && n_ty_args /= n_tyvars)
    || (spec_done     && n_ty_args  > n_tyvars)
    --
    -- Things are *not* OK if:
    --
    -- * Unsaturated type app before specialisation has been done;
    --
    -- * Oversaturated type app after specialisation (eta reduction
    --   may well be happening...);
    --
    -- Note: checkTyApp is usually followed by a call to checkSpecTyApp.
    --
    then (Nothing,     addErr errs msg loc)
    else (Just res_ty, errs)
  where
    (tyvars, rho_ty) = splitForalls forall_ty
    n_tyvars = length tyvars
    n_ty_args = length ty_args
    leftover_tyvars = drop n_ty_args tyvars
    inst_env = tyvars `zip` ty_args
    res_ty = mkForallTy leftover_tyvars (instantiateTy inst_env rho_ty)
\end{code}

\begin{code}
checkSpecTyApp :: PlainCoreExpr -> [UniType] -> ErrMsg -> LintM (Maybe ())

checkSpecTyApp expr ty_args msg spec_done loc scope errs
  = if spec_done
    && any isUnboxedDataType ty_args
    && not (an_application_of_error expr)
    then (Nothing, addErr errs msg loc)
    else (Just (), errs)
  where
     -- always safe (but maybe unfriendly) to say "False"
    an_application_of_error (CoVar id) | isBottomingId id = True
    an_application_of_error _ = False
\end{code}

\begin{code}
checkFunApp :: UniType 		-- The function type
	    -> [UniType] 	-- The arg type(s)
	    -> ErrMsg 		-- Error messgae
	    -> LintM (Maybe UniType)	-- The result type

checkFunApp fun_ty arg_tys msg spec loc scope errs
  = cfa res_ty expected_arg_tys arg_tys
  where
    (expected_arg_tys, res_ty) = splitTyArgs fun_ty

    cfa res_ty expected []	-- Args have run out; that's fine
	= (Just (glueTyArgs expected res_ty), errs)

    cfa res_ty [] arg_tys	-- Expected arg tys ran out first; maybe res_ty is a 
				-- dictionary type which is actually a function?
	= case splitTyArgs (unDictifyTy res_ty) of
	    ([], _) 		    -> (Nothing, addErr errs msg loc)	-- Too many args
	    (new_expected, new_res) -> cfa new_res new_expected arg_tys

    cfa res_ty (expected_arg_ty:expected_arg_tys) (arg_ty:arg_tys)
	= case (cmpUniType True{-properly-} expected_arg_ty arg_ty) of
		EQ_ -> cfa res_ty expected_arg_tys arg_tys
		other -> (Nothing, addErr errs msg loc)			-- Arg mis-match
\end{code}

\begin{code}
checkInScope :: Id -> LintM ()
checkInScope id spec loc scope errs
  = if isLocallyDefined id && not (id `elementOfUniqSet` scope) then
	((), addErr errs (\ sty -> ppCat [ppr sty id, ppStr "is out of scope"]) loc)
    else
	((), errs)

checkTys :: UniType -> UniType -> ErrMsg -> LintM ()
checkTys ty1 ty2 msg spec loc scope errs
  = case (cmpUniType True{-properly-} ty1 ty2) of
	EQ_   -> ((), errs)
	other -> ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: PlainCoreCaseAlternatives -> ErrMsg
mkCaseAltMsg alts sty
  = ppAbove (ppStr "In some case alternatives, type of alternatives not all same:")
	    (ppr sty alts)

mkCaseDataConMsg :: PlainCoreExpr -> ErrMsg
mkCaseDataConMsg expr sty
  = ppAbove (ppStr "A case scrutinee not a type-constructor type:")
	    (pp_expr sty expr)

mkCasePrimMsg :: Bool -> TyCon -> ErrMsg
mkCasePrimMsg True tycon sty
  = ppAbove (ppStr "A primitive case on a non-primitive type:")
	    (ppr sty tycon)
mkCasePrimMsg False tycon sty
  = ppAbove (ppStr "An algebraic case on a primitive type:")
	    (ppr sty tycon)

mkCaseAbstractMsg :: TyCon -> ErrMsg
mkCaseAbstractMsg tycon sty
  = ppAbove (ppStr "An algebraic case on an abstract type:")
	    (ppr sty tycon)

mkDefltMsg :: PlainCoreCaseDefault -> ErrMsg
mkDefltMsg deflt sty
  = ppAbove (ppStr "Binder in default case of a case expression doesn't match type of scrutinee:")
	    (ppr sty deflt)

mkFunAppMsg :: UniType -> [UniType] -> PlainCoreExpr -> ErrMsg
mkFunAppMsg fun_ty arg_tys expr sty
  = ppAboves [ppStr "In a function application, function type doesn't match arg types:",
	      ppHang (ppStr "Function type:") 4 (ppr sty fun_ty),
	      ppHang (ppStr "Arg types:") 4 (ppAboves (map (ppr sty) arg_tys)),
	      ppHang (ppStr "Expression:") 4 (pp_expr sty expr)]

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

mkPrimAltMsg :: (BasicLit, PlainCoreExpr) -> ErrMsg
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

mkRhsPrimMsg :: Id -> PlainCoreExpr -> ErrMsg
mkRhsPrimMsg binder rhs sty
  = ppAboves [ppCat [ppStr "The type of this binder is primitive:", 
		     ppr sty binder],
	      ppCat [ppStr "Binder's type:", ppr sty (getIdUniType binder)]
	     ]

mkTyAppMsg :: PlainCoreExpr -> ErrMsg
mkTyAppMsg expr sty
  = ppAboves [ppStr "In a type application, either the function's type doesn't match",
	      ppStr "the argument types, or an argument type is primitive:",
	      pp_expr sty expr]

mkSpecTyAppMsg :: PlainCoreExpr -> ErrMsg
mkSpecTyAppMsg expr sty
  = ppAbove (ppStr "Unboxed types in a type application (after specialisation):")
	    (pp_expr sty expr)

pp_expr sty expr
  = pprCoreExpr sty pprBigCoreBinder pprTypedCoreBinder pprTypedCoreBinder expr
\end{code}
