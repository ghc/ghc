%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
#include "HsVersions.h"

module CoreLint (
	lintCoreBindings,
	lintUnfolding
    ) where

import Ubiq

import CoreSyn

import Bag
import Kind		( Kind{-instance-} )
import Literal		( literalType, Literal{-instance-} )
import Id		( idType, isBottomingId,
			  dataConArgTys, GenId{-instances-}
			)
import Maybes		( catMaybes )
import Name		( isLocallyDefined, getSrcLoc )
import Outputable	( Outputable(..){-instance * []-} )
import PprCore
import PprStyle		( PprStyle(..) )
import PprType		( GenType, GenTyVar, TyCon )
import Pretty
import PrimOp		( primOpType, PrimOp(..) )
import PrimRep		( PrimRep(..) )
import SrcLoc		( SrcLoc )
import Type		( mkFunTy,getFunTy_maybe,mkForAllTy,getForAllTy_maybe,
			  isPrimType,getTypeKind,instantiateTy,
			  mkForAllUsageTy,getForAllUsageTy,instantiateUsage,
			  maybeAppDataTyCon, eqTy
			)
import TyCon		( isPrimTyCon, tyConFamilySize )
import TyVar		( getTyVarKind, GenTyVar{-instances-} )
import UniqSet		( emptyUniqSet, mkUniqSet, intersectUniqSets,
			  unionUniqSets, elementOfUniqSet, UniqSet(..)
			)
import Unique		( Unique )
import Usage		( GenUsage )
import Util		( zipEqual, pprTrace, pprPanic, assertPanic, panic )

infixr 9 `thenL`, `seqL`, `thenMaybeL`, `seqMaybeL`
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreBindings]{@lintCoreBindings@: Top-level interface}
%*									*
%************************************************************************

Checks that a set of core bindings is well-formed.  The PprStyle and String
just control what we print in the event of an error.  The Bool value
indicates whether we have done any specialisation yet (in which case we do
some extra checks).

We check for
	(a) type errors
	(b) Out-of-scope type variables
	(c) Out-of-scope local variables
	(d) Ill-kinded types

If we have done specialisation the we check that there are
	(a) No top-level bindings of primitive (unboxed type)

Outstanding issues:

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

\begin{code}
lintCoreBindings
	:: PprStyle -> String -> Bool -> [CoreBinding] -> [CoreBinding]

lintCoreBindings sty whoDunnit spec_done binds
  = case (initL (lint_binds binds) spec_done) of
      Nothing  -> binds
      Just msg ->
	pprPanic "" (ppAboves [
	  ppStr ("*** Core Lint Errors: in " ++ whoDunnit ++ " ***"),
	  msg sty,
	  ppStr "*** Offending Program ***",
	  ppAboves (map (pprCoreBinding sty) binds),
	  ppStr "*** End of Offense ***"
	])
  where
    lint_binds [] = returnL ()
    lint_binds (bind:binds)
      = lintCoreBinding bind `thenL` \binders ->
	addInScopeVars binders (lint_binds binds)
\end{code}

%************************************************************************
%*									*
\subsection[lintUnfolding]{lintUnfolding}
%*									*
%************************************************************************

We use this to check all unfoldings that come in from interfaces
(it is very painful to catch errors otherwise):

\begin{code}
lintUnfolding :: SrcLoc -> CoreExpr -> Maybe CoreExpr

lintUnfolding locn expr
  = case
      (initL (addLoc (ImportedUnfolding locn) (lintCoreExpr expr))
       True{-pretend spec done-})
    of
      Nothing  -> Just expr
      Just msg ->
        pprTrace "WARNING: Discarded bad unfolding from interface:\n"
	(ppAboves [msg PprForUser,
		   ppStr "*** Bad unfolding ***",
		   ppr PprDebug expr,
		   ppStr "*** End unfolding ***"])
	Nothing
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreBinding]{lintCoreBinding}
%*									*
%************************************************************************

Check a core binding, returning the list of variables bound.

\begin{code}
lintCoreBinding :: CoreBinding -> LintM [Id]

lintCoreBinding (NonRec binder rhs)
  = lintSingleBinding (binder,rhs) `seqL` returnL [binder]

lintCoreBinding (Rec pairs)
  = addInScopeVars binders (
      mapL lintSingleBinding pairs `seqL` returnL binders
    )
  where
    binders = [b | (b,_) <- pairs]

lintSingleBinding (binder,rhs)
  = addLoc (RhsOf binder) (
	-- Check the rhs
	lintCoreExpr rhs

	`thenL` \maybe_ty ->
	-- Check match to RHS type
	(case maybe_ty of
	  Nothing -> returnL ()
	  Just ty -> checkTys (idType binder) ty (mkRhsMsg binder ty))

	`seqL`
	-- Check (not isPrimType)
	checkIfSpecDoneL (not (isPrimType (idType binder)))
	  (mkRhsPrimMsg binder rhs)

	-- We should check the unfolding, if any, but this is tricky because
	-- the unfolding is a SimplifiableCoreExpr. Give up for now.
    )
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreExpr]{lintCoreExpr}
%*									*
%************************************************************************

\begin{code}
lintCoreExpr :: CoreExpr -> LintM (Maybe Type)	-- Nothing if error found

lintCoreExpr (Var var) = checkInScope var `seqL` returnL (Just (idType var))
lintCoreExpr (Lit lit) = returnL (Just (literalType lit))
lintCoreExpr (SCC _ expr) = lintCoreExpr expr

lintCoreExpr (Let binds body)
  = lintCoreBinding binds `thenL` \binders ->
    if (null binders) then
	lintCoreExpr body  -- Can't add a new source location
    else
      addLoc (BodyOfLetRec binders)
	(addInScopeVars binders (lintCoreExpr body))

lintCoreExpr e@(Con con args)
  = lintCoreArgs False e (idType con) args
    -- Note: we don't check for primitive types in these arguments

lintCoreExpr e@(Prim op args)
  = lintCoreArgs True e (primOpType op) args
    -- Note: we do check for primitive types in these arguments

lintCoreExpr e@(App fun@(Var v) arg) | isBottomingId v
  = lintCoreExpr fun `thenMaybeL` \ ty -> lintCoreArg False e ty arg
    -- Note: we don't check for primitive types in argument to 'error'

lintCoreExpr e@(App fun arg)
  = lintCoreExpr fun `thenMaybeL` \ty -> lintCoreArg True e ty arg
    -- Note: we do check for primitive types in this argument

lintCoreExpr (Lam (ValBinder var) expr)
  = addLoc (LambdaBodyOf var)
      (addInScopeVars [var]
	(lintCoreExpr expr `thenMaybeL` \ty ->
	 returnL (Just (mkFunTy (idType var) ty))))

lintCoreExpr (Lam (TyBinder tyvar) expr)
  = lintCoreExpr expr `thenMaybeL` \ty ->
    returnL (Just(mkForAllTy tyvar ty))
    -- ToDo: Should add in-scope type variable at this point

lintCoreExpr e@(Case scrut alts)
 = lintCoreExpr scrut `thenMaybeL` \ty ->
   -- Check that it is a data type
   case maybeAppDataTyCon ty of
     Nothing -> addErrL (mkCaseDataConMsg e) `seqL` returnL Nothing
     Just(tycon, _, _) -> lintCoreAlts alts ty tycon
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArgs]{lintCoreArgs}
%*									*
%************************************************************************

The boolean argument indicates whether we should flag type
applications to primitive types as being errors.

\begin{code}
lintCoreArgs :: Bool -> CoreExpr -> Type -> [CoreArg] -> LintM (Maybe Type)

lintCoreArgs _          _ ty [] = returnL (Just ty)
lintCoreArgs checkTyApp e ty (a : args)
  = lintCoreArg  checkTyApp e ty  a `thenMaybeL` \ res ->
    lintCoreArgs checkTyApp e res args
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArg]{lintCoreArg}
%*									*
%************************************************************************

\begin{code}
lintCoreArg :: Bool -> CoreExpr -> Type -> CoreArg -> LintM (Maybe Type)

lintCoreArg _ e ty (LitArg lit)
  = -- Make sure function type matches argument
    case (getFunTy_maybe ty) of
      Just (arg,res) | (literalType lit `eqTy` arg) -> returnL(Just res)
      _ -> addErrL (mkAppMsg ty (literalType lit) e) `seqL` returnL Nothing

lintCoreArg _ e ty (VarArg v)
  = -- Make sure variable is bound
    checkInScope v `seqL`
    -- Make sure function type matches argument
    case (getFunTy_maybe ty) of
      Just (arg,res) | (idType v `eqTy` arg) -> returnL(Just res)
      _ -> addErrL (mkAppMsg ty (idType v) e) `seqL` returnL Nothing

lintCoreArg checkTyApp e ty a@(TyArg arg_ty)
  = -- ToDo: Check that ty is well-kinded and has no unbound tyvars
    checkIfSpecDoneL (not (isPrimType arg_ty)) (mkSpecTyAppMsg a)
    `seqL`
    case (getForAllTy_maybe ty) of
      Just (tyvar,body) | (getTyVarKind tyvar == getTypeKind arg_ty) ->
	returnL(Just(instantiateTy [(tyvar,arg_ty)] body))
	| pprTrace "lintCoreArg:kinds:" (ppCat [ppr PprDebug (getTyVarKind tyvar), ppr PprDebug (getTypeKind arg_ty)]) False -> panic "impossible"
      _ -> addErrL (mkTyAppMsg ty arg_ty e) `seqL` returnL Nothing
	
lintCoreArg _ e ty (UsageArg u)
  = -- ToDo: Check that usage has no unbound usage variables
    case (getForAllUsageTy ty) of
      Just (uvar,bounds,body) ->
        -- ToDo: Check argument satisfies bounds
        returnL(Just(panic "lintCoreArg:instantiateUsage uvar u body"))
      _ -> addErrL (mkUsageAppMsg ty u e) `seqL` returnL Nothing
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreAlts]{lintCoreAlts}
%*									*
%************************************************************************

\begin{code}
lintCoreAlts :: CoreCaseAlts
	     -> Type  			-- Type of scrutinee
	     -> TyCon			-- TyCon pinned on the case
	     -> LintM (Maybe Type)	-- Type of alternatives

lintCoreAlts whole_alts@(AlgAlts alts deflt) ty tycon
  = -- Check tycon is not a primitive tycon
    addErrIfL (isPrimTyCon tycon) (mkCasePrimMsg tycon)
    `seqL`
    -- Check we are scrutinising a proper datatype
    -- (ToDo: robustify)
    addErrIfL (not (tyConFamilySize tycon >= 1)) (mkCaseAbstractMsg tycon)
    `seqL`
    lintDeflt deflt ty
    `thenL` \maybe_deflt_ty ->
    mapL (lintAlgAlt ty tycon) alts
    `thenL` \maybe_alt_tys ->
    -- Check the result types
    case catMaybes (maybe_deflt_ty : maybe_alt_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`seqL`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg whole_alts)

lintCoreAlts whole_alts@(PrimAlts alts deflt) ty tycon
  = -- Check tycon is a primitive tycon
    addErrIfL (not (isPrimTyCon tycon)) (mkCaseNotPrimMsg tycon)
    `seqL`
    mapL (lintPrimAlt ty) alts
    `thenL` \maybe_alt_tys ->
    lintDeflt deflt ty
    `thenL` \maybe_deflt_ty ->
    -- Check the result types
    case catMaybes (maybe_deflt_ty : maybe_alt_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`seqL`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg whole_alts)

lintAlgAlt scrut_ty tycon{-ToDo: use it!-} (con,args,rhs)
  = (case maybeAppDataTyCon scrut_ty of
      Nothing ->
	 addErrL (mkAlgAltMsg1 scrut_ty)
      Just (tycon, tys_applied, cons) ->
	 let
	   arg_tys = dataConArgTys con tys_applied
	 in
	 checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con) `seqL`
	 checkL (length arg_tys == length args) (mkAlgAltMsg3 con args)
								 `seqL`
	 mapL check (arg_tys `zipEqual` args)			 `seqL`
	 returnL ()
    )								 `seqL`
    addInScopeVars args 	(
	 lintCoreExpr rhs
    )
  where
    check (ty, arg) = checkTys ty (idType arg) (mkAlgAltMsg4 ty arg)

    -- elem: yes, the elem-list here can sometimes be long-ish,
    -- but as it's use-once, probably not worth doing anything different
    -- We give it its own copy, so it isn't overloaded.
    elem _ []	    = False
    elem x (y:ys)   = x==y || elem x ys

lintPrimAlt ty alt@(lit,rhs)
 = checkTys (literalType lit) ty (mkPrimAltMsg alt) `seqL`
   lintCoreExpr rhs

lintDeflt NoDefault _ = returnL Nothing
lintDeflt deflt@(BindDefault binder rhs) ty
  = checkTys (idType binder) ty (mkDefltMsg deflt) `seqL`
    addInScopeVars [binder] (lintCoreExpr rhs)
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
  | LambdaBodyOf Id	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)

instance Outputable LintLocInfo where
    ppr sty (RhsOf v)
      = ppBesides [ppr sty (getSrcLoc v), ppStr ": [RHS of ", pp_binders sty [v], ppStr "]"]

    ppr sty (LambdaBodyOf b)
      = ppBesides [ppr sty (getSrcLoc b),
		ppStr ": [in body of lambda with binder ", pp_binder sty b, ppStr "]"]

    ppr sty (BodyOfLetRec bs)
      = ppBesides [ppr sty (getSrcLoc (head bs)),
		ppStr ": [in body of letrec with binders ", pp_binders sty bs, ppStr "]"]

    ppr sty (ImportedUnfolding locn)
      = ppBeside (ppr sty locn) (ppStr ": [in an imported unfolding]")

pp_binders :: PprStyle -> [Id] -> Pretty
pp_binders sty bs = ppInterleave ppComma (map (pp_binder sty) bs)

pp_binder :: PprStyle -> Id -> Pretty
pp_binder sty b = ppCat [ppr sty b, ppStr "::", ppr sty (idType b)]
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

seqL :: LintM a -> LintM b -> LintM b
seqL m k spec loc scope errs
  = case m spec loc scope errs of
      (_, errs') -> k spec loc scope errs'

thenMaybeL :: LintM (Maybe a) -> (a -> LintM (Maybe b)) -> LintM (Maybe b)
thenMaybeL m k spec loc scope errs
  = case m spec loc scope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just r,  errs2) -> k r spec loc scope errs2

seqMaybeL :: LintM (Maybe a) -> LintM (Maybe b) -> LintM (Maybe b)
seqMaybeL m k spec loc scope errs
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

addErrIfL pred spec
  = if pred then addErrL spec else returnL ()

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
checkInScope :: Id -> LintM ()
checkInScope id spec loc scope errs
  = if isLocallyDefined id && not (id `elementOfUniqSet` scope) then
      ((),addErr errs (\sty -> ppCat [ppr sty id,ppStr "is out of scope"]) loc)
    else
      ((),errs)

checkTys :: Type -> Type -> ErrMsg -> LintM ()
checkTys ty1 ty2 msg spec loc scope errs
  = if ty1 `eqTy` ty2 then ((), errs) else ((), addErr errs msg loc)
\end{code}

\begin{code}
mkCaseAltMsg :: CoreCaseAlts -> ErrMsg
mkCaseAltMsg alts sty
  = ppAbove (ppStr "Type of case alternatives not the same:")
	    (ppr sty alts)

mkCaseDataConMsg :: CoreExpr -> ErrMsg
mkCaseDataConMsg expr sty
  = ppAbove (ppStr "A case scrutinee not of data constructor type:")
	    (pp_expr sty expr)

mkCaseNotPrimMsg :: TyCon -> ErrMsg
mkCaseNotPrimMsg tycon sty
  = ppAbove (ppStr "A primitive case on a non-primitive type:")
	    (ppr sty tycon)

mkCasePrimMsg :: TyCon -> ErrMsg
mkCasePrimMsg tycon sty
  = ppAbove (ppStr "An algebraic case on a primitive type:")
	    (ppr sty tycon)

mkCaseAbstractMsg :: TyCon -> ErrMsg
mkCaseAbstractMsg tycon sty
  = ppAbove (ppStr "An algebraic case on some weird type:")
	    (ppr sty tycon)

mkDefltMsg :: CoreCaseDefault -> ErrMsg
mkDefltMsg deflt sty
  = ppAbove (ppStr "Binder in case default doesn't match type of scrutinee:")
	    (ppr sty deflt)

mkAppMsg :: Type -> Type -> CoreExpr -> ErrMsg
mkAppMsg fun arg expr sty
  = ppAboves [ppStr "Argument values doesn't match argument type:",
	      ppHang (ppStr "Fun type:") 4 (ppr sty fun),
	      ppHang (ppStr "Arg type:") 4 (ppr sty arg),
	      ppHang (ppStr "Expression:") 4 (pp_expr sty expr)]

mkTyAppMsg :: Type -> Type -> CoreExpr -> ErrMsg
mkTyAppMsg ty arg expr sty
  = ppAboves [ppStr "Illegal type application:",
	      ppHang (ppStr "Exp type:")   4 (ppr sty ty),
	      ppHang (ppStr "Arg type:")   4 (ppr sty arg),
	      ppHang (ppStr "Expression:") 4 (pp_expr sty expr)]

mkUsageAppMsg :: Type -> Usage -> CoreExpr -> ErrMsg
mkUsageAppMsg ty u expr sty
  = ppAboves [ppStr "Illegal usage application:",
	      ppHang (ppStr "Exp type:") 4 (ppr sty ty),
	      ppHang (ppStr "Usage exp:") 4 (ppr sty u),
	      ppHang (ppStr "Expression:") 4 (pp_expr sty expr)]

mkAlgAltMsg1 :: Type -> ErrMsg
mkAlgAltMsg1 ty sty
  = ppAbove (ppStr "In some case statement, type of scrutinee is not a data type:")
	    (ppr sty ty)

mkAlgAltMsg2 :: Type -> Id -> ErrMsg
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

mkAlgAltMsg4 :: Type -> Id -> ErrMsg
mkAlgAltMsg4 ty arg sty
  = ppAboves [
	ppStr "In some algebraic case alternative, type of argument doesn't match data constructor:",
	ppr sty ty,
	ppr sty arg
    ]

mkPrimAltMsg :: (Literal, CoreExpr) -> ErrMsg
mkPrimAltMsg alt sty
  = ppAbove
    (ppStr "In a primitive case alternative, type of literal doesn't match type of scrutinee:")
	    (ppr sty alt)

mkRhsMsg :: Id -> Type -> ErrMsg
mkRhsMsg binder ty sty
  = ppAboves
    [ppCat [ppStr "The type of this binder doesn't match the type of its RHS:",
	    ppr sty binder],
     ppCat [ppStr "Binder's type:", ppr sty (idType binder)],
     ppCat [ppStr "Rhs type:", ppr sty ty]]

mkRhsPrimMsg :: Id -> CoreExpr -> ErrMsg
mkRhsPrimMsg binder rhs sty
  = ppAboves [ppCat [ppStr "The type of this binder is primitive:",
		     ppr sty binder],
	      ppCat [ppStr "Binder's type:", ppr sty (idType binder)]
	     ]

mkSpecTyAppMsg :: CoreArg -> ErrMsg
mkSpecTyAppMsg arg sty
  = ppAbove
      (ppStr "Unboxed types in a type application (after specialisation):")
      (ppr sty arg)

pp_expr :: PprStyle -> CoreExpr -> Pretty
pp_expr sty expr
  = pprCoreExpr sty (pprBigCoreBinder sty) (pprTypedCoreBinder sty) (pprTypedCoreBinder sty) expr
\end{code}
