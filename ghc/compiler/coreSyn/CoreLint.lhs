%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
module CoreLint (
	lintCoreBindings,
	lintUnfolding
    ) where

#include "HsVersions.h"

import IO	( hPutStr, stderr )

import CmdLineOpts      ( opt_D_show_passes, opt_DoCoreLinting )
import CoreSyn
import CoreUtils	( idSpecVars )

import Bag
import Kind		( hasMoreBoxityInfo, Kind{-instance-} )
import Literal		( literalType, Literal{-instance-} )
import Id		( idType, isBottomingId, dataConRepType, isDataCon, isAlgCon,
			  dataConArgTys, GenId{-instances-},
			  emptyIdSet, mkIdSet, 
			  unionIdSets, elementOfIdSet, IdSet,
			  Id
			)
import Maybes		( catMaybes )
import Name		( isLocallyDefined, getSrcLoc, Name{-instance NamedThing-},
			  NamedThing(..)
			)
import PprCore
import ErrUtils		( doIfSet, ghcExit )
import PrimOp		( primOpType )
import PrimRep		( PrimRep(..) )
import SrcLoc		( SrcLoc )
import Type		( mkFunTy, splitFunTy_maybe, mkForAllTy,
			  splitForAllTy_maybe, tyVarsOfType,
			  isUnpointedType, typeKind, instantiateTy,
			  splitAlgTyConApp_maybe, Type
			)
import TyCon		( TyCon, isPrimTyCon, isDataTyCon )
import TyVar		( TyVar, tyVarKind, mkTyVarEnv, 
			  TyVarSet,
			    emptyTyVarSet, mkTyVarSet, isEmptyTyVarSet, 
			    minusTyVarSet, elementOfTyVarSet, tyVarSetToList,
			    unionTyVarSets, intersectTyVarSets
			)
import ErrUtils		( ErrMsg )
import Unique		( Unique )
import Util		( zipEqual )
import Outputable

infixr 9 `thenL`, `seqL`, `thenMaybeL`
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
lintCoreBindings :: String -> Bool -> [CoreBinding] -> IO ()

lintCoreBindings whoDunnit spec_done binds
  | not opt_DoCoreLinting
  = return ()

lintCoreBindings whoDunnit spec_done binds
  = case (initL (lint_binds binds) spec_done) of
      Nothing       -> doIfSet opt_D_show_passes
			(hPutStr stderr ("*** Core Linted result of " ++ whoDunnit ++ "\n"))

      Just bad_news -> printDump (display bad_news)	>>
		       ghcExit 1
  where
    lint_binds [] = returnL ()
    lint_binds (bind:binds)
      = lintCoreBinding bind `thenL` \binders ->
	addInScopeVars binders (lint_binds binds)

    display bad_news
      = vcat [
		text ("*** Core Lint Errors: in result of " ++ whoDunnit ++ " ***"),
		bad_news,
		ptext SLIT("*** Offending Program ***"),
		pprCoreBindings binds,
		ptext SLIT("*** End of Offense ***")
	]
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
	(vcat [msg,
		   ptext SLIT("*** Bad unfolding ***"),
		   ppr expr,
		   ptext SLIT("*** End unfolding ***")])
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
	-- Check (not isUnpointedType)
	checkIfSpecDoneL (not (isUnpointedType (idType binder)))
	  (mkRhsPrimMsg binder rhs)  `seqL`

        -- Check whether binder's specialisations contain any out-of-scope variables
        ifSpecDoneL (mapL (checkSpecIdInScope binder) spec_vars `seqL` returnL ())
	  
	-- We should check the unfolding, if any, but this is tricky because
	-- the unfolding is a SimplifiableCoreExpr. Give up for now.
    )
    where
     spec_vars = idSpecVars binder

\end{code}

%************************************************************************
%*									*
\subsection[lintCoreExpr]{lintCoreExpr}
%*									*
%************************************************************************

\begin{code}
lintCoreExpr :: CoreExpr -> LintM (Maybe Type)	-- Nothing if error found

lintCoreExpr (Var var) 
  | isAlgCon var = returnL (Just (idType var))
	-- Micro-hack here... Class decls generate applications of their
	-- dictionary constructor, but don't generate a binding for the
	-- constructor (since it would never be used).  After a single round
	-- of simplification, these dictionary constructors have been
	-- inlined (from their UnfoldInfo) to CoCons.  Just between
	-- desugaring and simplfication, though, they appear as naked, unbound
	-- variables as the function in an application.
	-- The hack here simply doesn't check for out-of-scope-ness for
	-- data constructors (at least, in a function position).

  | otherwise    = checkIdInScope var `seqL` returnL (Just (idType var))

lintCoreExpr (Lit lit) = returnL (Just (literalType lit))

lintCoreExpr (Note (Coerce to_ty from_ty) expr)
  = lintCoreExpr expr 	`thenMaybeL` \ expr_ty ->
    lintTy to_ty	`seqL`
    lintTy from_ty	`seqL`
    checkTys from_ty expr_ty (mkCoerceErr from_ty expr_ty)	`seqL`
    returnL (Just to_ty)

lintCoreExpr (Note other_note expr)
  = lintCoreExpr expr

lintCoreExpr (Let binds body)
  = lintCoreBinding binds `thenL` \binders ->
    if (null binders) then
	lintCoreExpr body  -- Can't add a new source location
    else
      addLoc (BodyOfLetRec binders)
	(addInScopeVars binders (lintCoreExpr body))

lintCoreExpr e@(Con con args)
  = checkL (isDataCon con) (mkConErrMsg e)	`seqL`
    lintCoreArgs {-False-} e (dataConRepType con) args
    -- Note: we don't check for primitive types in these arguments

lintCoreExpr e@(Prim op args)
  = lintCoreArgs {-True-} e (primOpType op) args
    -- Note: we do check for primitive types in these arguments

lintCoreExpr e@(App fun@(Var v) arg) | isBottomingId v
  = lintCoreExpr fun `thenMaybeL` \ ty -> lintCoreArg {-False-} e ty arg
    -- Note: we don't check for primitive types in argument to 'error'

lintCoreExpr e@(App fun arg)
  = lintCoreExpr fun `thenMaybeL` \ty -> lintCoreArg {-True-} e ty arg
    -- Note: we do check for primitive types in this argument

lintCoreExpr (Lam vb@(ValBinder var) expr)
  = addLoc (LambdaBodyOf vb)
      (addInScopeVars [var]
	(lintCoreExpr expr `thenMaybeL` \ty ->
	 returnL (Just (mkFunTy (idType var) ty))))

lintCoreExpr (Lam tb@(TyBinder tyvar) expr)
  = addLoc (LambdaBodyOf tb)  $
     addInScopeTyVars [tyvar] $
       lintCoreExpr expr			   `thenMaybeL` \ ty ->
       returnL (Just(mkForAllTy tyvar ty))

lintCoreExpr e@(Case scrut alts)
 = lintCoreExpr scrut `thenMaybeL` \ty ->
   lintCoreAlts alts ty
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArgs]{lintCoreArgs}
%*									*
%************************************************************************

The boolean argument indicates whether we should flag type
applications to primitive types as being errors.

\begin{code}
lintCoreArgs :: {-Bool ->-} CoreExpr -> Type -> [CoreArg] -> LintM (Maybe Type)

lintCoreArgs _ ty [] = returnL (Just ty)
lintCoreArgs e ty (a : args)
  = lintCoreArg  e ty  a `thenMaybeL` \ res ->
    lintCoreArgs e res args
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArg]{lintCoreArg}
%*									*
%************************************************************************

\begin{code}
lintCoreArg :: {-Bool ->-} CoreExpr -> Type -> CoreArg -> LintM (Maybe Type)

lintCoreArg e ty (LitArg lit)
  = -- Make sure function type matches argument
    case (splitFunTy_maybe ty) of
      Just (arg,res) | (lit_ty == arg) -> returnL(Just res)
      _ -> addErrL (mkAppMsg ty lit_ty e) `seqL` returnL Nothing
  where
    lit_ty = literalType lit

lintCoreArg e ty (VarArg v)
  = -- Make sure variable is bound
    checkIdInScope v `seqL`
    -- Make sure function type matches argument
    case (splitFunTy_maybe ty) of
      Just (arg,res) | (var_ty == arg) -> returnL(Just res)
      _ -> addErrL (mkAppMsg ty var_ty e) `seqL` returnL Nothing
  where
    var_ty = idType v

lintCoreArg e ty a@(TyArg arg_ty)
  = lintTy arg_ty			     `seqL`
    checkTyVarsInScope (tyVarsOfType arg_ty) `seqL`
    case (splitForAllTy_maybe ty) of
      Nothing -> addErrL (mkTyAppMsg SLIT("Illegal") ty arg_ty e) `seqL` returnL Nothing

      Just (tyvar,body) ->
	let
	    tyvar_kind = tyVarKind tyvar
	    argty_kind = typeKind arg_ty
	in
	if argty_kind `hasMoreBoxityInfo` tyvar_kind
		-- Arg type might be boxed for a function with an uncommitted
		-- tyvar; notably this is used so that we can give
		-- 	error :: forall a:*. String -> a
		-- and then apply it to both boxed and unboxed types.
	 then
	    returnL(Just(instantiateTy (mkTyVarEnv [(tyvar,arg_ty)]) body))
	else
	    pprTrace "lintCoreArg:kinds:" (hsep [ppr tyvar_kind, ppr argty_kind]) $
	    addErrL (mkKindErrMsg tyvar arg_ty e) `seqL` returnL Nothing
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreAlts]{lintCoreAlts}
%*									*
%************************************************************************

\begin{code}
lintCoreAlts :: CoreCaseAlts
	     -> Type  			-- Type of scrutinee
--	     -> TyCon			-- TyCon pinned on the case
	     -> LintM (Maybe Type)	-- Type of alternatives

lintCoreAlts whole_alts@(AlgAlts alts deflt) ty --tycon
  = -- Check tycon is not a primitive tycon
--    addErrIfL (isPrimTyCon tycon) (mkCasePrimMsg tycon)
--    `seqL`
    -- Check we are scrutinising a proper datatype
    -- (ToDo: robustify)
--    addErrIfL (not (tyConFamilySize tycon >= 1)) (mkCaseAbstractMsg tycon)
--    `seqL`
    lintDeflt deflt ty
    `thenL` \maybe_deflt_ty ->
    mapL (lintAlgAlt ty {-tycon-}) alts
    `thenL` \maybe_alt_tys ->
    -- Check the result types
    case catMaybes (maybe_deflt_ty : maybe_alt_tys) of
      []	     -> returnL Nothing

      (first_ty:tys) -> mapL check tys	`seqL`
			returnL (Just first_ty)
	where
	  check ty = checkTys first_ty ty (mkCaseAltMsg whole_alts)

lintCoreAlts whole_alts@(PrimAlts alts deflt) ty --tycon
  = -- Check tycon is a primitive tycon
--    addErrIfL (not (isPrimTyCon tycon)) (mkCaseNotPrimMsg tycon)
--    `seqL`
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

lintAlgAlt scrut_ty (con,args,rhs)
  = (case splitAlgTyConApp_maybe scrut_ty of
      Just (tycon, tys_applied, cons) | isDataTyCon tycon ->
	 let
	   arg_tys = dataConArgTys con tys_applied
	 in
	 checkL (con `elem` cons) (mkAlgAltMsg2 scrut_ty con) `seqL`
	 checkL (length arg_tys == length args) (mkAlgAltMsg3 con args)
								 `seqL`
	 mapL check (zipEqual "lintAlgAlt" arg_tys args)	 `seqL`
	 returnL ()

      other -> addErrL (mkAlgAltMsg1 scrut_ty)
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
\subsection[lint-types]{Types}
%*									*
%************************************************************************

\begin{code}
lintTy :: Type -> LintM ()
lintTy ty = returnL ()
-- ToDo: Check that ty is well-kinded and has no unbound tyvars
\end{code}

    
%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
type LintM a = Bool		-- True <=> specialisation has been done
	    -> [LintLocInfo] 	-- Locations
	    -> IdSet		-- Local vars in scope
	    -> TyVarSet		-- Local tyvars in scope
	    -> Bag ErrMsg	-- Error messages so far
	    -> (a, Bag ErrMsg)	-- Result and error messages (if any)

data LintLocInfo
  = RhsOf Id			-- The variable bound
  | LambdaBodyOf CoreBinder	-- The lambda-binder
  | BodyOfLetRec [Id]		-- One of the binders
  | ImportedUnfolding SrcLoc    -- Some imported unfolding (ToDo: say which)

instance Outputable LintLocInfo where
    ppr (RhsOf v)
      = ppr (getSrcLoc v) <> colon <+> 
	brackets (ptext SLIT("RHS of") <+> pp_binders [v])

    ppr (LambdaBodyOf (ValBinder b))
      = ppr (getSrcLoc b) <> colon <+>
	brackets (ptext SLIT("in body of lambda with binder") <+> pp_binder b)

    ppr (LambdaBodyOf (TyBinder b))
      = ppr (getSrcLoc b) <> colon <+>
	brackets (ptext SLIT("in body of lambda with type binder") <+> ppr b)

    ppr (BodyOfLetRec bs)
      = ppr (getSrcLoc (head bs)) <> colon <+>
	brackets (ptext SLIT("in body of letrec with binders") <+> pp_binders bs)

    ppr (ImportedUnfolding locn)
      = ppr locn <> colon <+>
	brackets (ptext SLIT("in an imported unfolding"))

pp_binders :: [Id] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Id -> SDoc
pp_binder b = hsep [ppr b, text "::", ppr (idType b)]
\end{code}

\begin{code}
initL :: LintM a -> Bool -> Maybe ErrMsg
initL m spec_done
  = case (m spec_done [] emptyIdSet emptyTyVarSet emptyBag) of { (_, errs) ->
    if isEmptyBag errs then
	Nothing
    else
	Just (vcat (bagToList errs))
    }

returnL :: a -> LintM a
returnL r spec loc scope tyscope errs = (r, errs)

thenL :: LintM a -> (a -> LintM b) -> LintM b
thenL m k spec loc scope tyscope errs
  = case m spec loc scope tyscope errs of
      (r, errs') -> k r spec loc scope tyscope errs'

seqL :: LintM a -> LintM b -> LintM b
seqL m k spec loc scope tyscope errs
  = case m spec loc scope tyscope errs of
      (_, errs') -> k spec loc scope tyscope errs'

thenMaybeL :: LintM (Maybe a) -> (a -> LintM (Maybe b)) -> LintM (Maybe b)
thenMaybeL m k spec loc scope tyscope errs
  = case m spec loc scope tyscope errs of
      (Nothing, errs2) -> (Nothing, errs2)
      (Just r,  errs2) -> k r spec loc scope tyscope errs2

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
checkL True  msg spec loc scope tyscope errs = ((), errs)
checkL False msg spec loc scope tyscope errs = ((), addErr errs msg loc)

checkIfSpecDoneL :: Bool -> ErrMsg -> LintM ()
checkIfSpecDoneL True  msg spec  loc scope tyscope errs = ((), errs)
checkIfSpecDoneL False msg True  loc scope tyscope errs = ((), addErr errs msg loc)
checkIfSpecDoneL False msg False loc scope tyscope errs = ((), errs)

ifSpecDoneL :: LintM () -> LintM ()
ifSpecDoneL m False loc scope tyscope errs = ((), errs)
ifSpecDoneL m True  loc scope tyscope errs = m True loc scope tyscope errs

addErrL :: ErrMsg -> LintM ()
addErrL msg spec loc scope tyscope errs = ((), addErr errs msg loc)

addErr :: Bag ErrMsg -> ErrMsg -> [LintLocInfo] -> Bag ErrMsg

addErr errs_so_far msg locs
  = ASSERT (not (null locs))
    errs_so_far `snocBag` (hang (ppr (head locs)) 4 msg)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m spec loc scope tyscope errs
  = m spec (extra_loc:loc) scope tyscope errs

addInScopeVars :: [Id] -> LintM a -> LintM a
addInScopeVars ids m spec loc scope tyscope errs
  = -- We check if these "new" ids are already
    -- in scope, i.e., we have *shadowing* going on.
    -- For now, it's just a "trace"; we may make
    -- a real error out of it...
    let
	new_set = mkIdSet ids

--	shadowed = scope `intersectIdSets` new_set
    in
--  After adding -fliberate-case, Simon decided he likes shadowed
--  names after all.  WDP 94/07
--  (if isEmptyUniqSet shadowed
--  then id
--  else pprTrace "Shadowed vars:" (ppr (uniqSetToList shadowed))) (
    m spec loc (scope `unionIdSets` new_set) tyscope errs
--  )

addInScopeTyVars :: [TyVar] -> LintM a -> LintM a
addInScopeTyVars tyvars m spec loc scope tyscope errs
  = m spec loc scope (tyscope `unionTyVarSets` new_set) errs
    where
     new_set	= mkTyVarSet tyvars
    
\end{code}

\begin{code}
checkIdInScope :: Id -> LintM ()
checkIdInScope id 
  = checkInScope (ptext SLIT("is out of scope")) id

checkSpecIdInScope :: Id -> Id -> LintM ()
checkSpecIdInScope binder id 
  = checkInScope msg id
    where
     msg = ptext SLIT("is out of scope inside specialisation info for") <+> 
	   ppr binder

checkInScope :: SDoc -> Id -> LintM ()
checkInScope loc_msg id spec loc scope tyscope errs
  = let
	id_name = getName id
    in
    if isLocallyDefined id_name && not (id `elementOfIdSet` scope) then
      ((), addErr errs (hsep [ppr id, loc_msg]) loc)
    else
      ((),errs)

checkTyVarsInScope :: TyVarSet -> LintM ()
checkTyVarsInScope tyvars spec loc scope tyscope errs
-- | not (isEmptyTyVarSet out_of_scope) = ((), errs')
 | otherwise			= ((), errs)
   where
    out_of_scope = tyvars `minusTyVarSet` tyscope
    errs'        = 
       foldr (\ tv errs -> addErr errs (hsep [ppr tv, ptext SLIT("is out of scope")]) loc)
	     errs
	     (tyVarSetToList out_of_scope)

checkTys :: Type -> Type -> ErrMsg -> LintM ()
checkTys ty1 ty2 msg spec loc scope tyscope errs
  = if ty1 == ty2 then ((), errs) else ((), addErr errs msg loc)
\end{code}

\begin{code}
mkConErrMsg e
  = ($$) (ptext SLIT("Application of newtype constructor:"))
	    (ppr e)


mkCaseAltMsg :: CoreCaseAlts -> ErrMsg
mkCaseAltMsg alts
  = ($$) (ptext SLIT("Type of case alternatives not the same:"))
	    (ppr alts)

mkCaseAbstractMsg :: TyCon -> ErrMsg
mkCaseAbstractMsg tycon
  = ($$) (ptext SLIT("An algebraic case on some weird type:"))
	    (ppr tycon)

mkDefltMsg :: CoreCaseDefault -> ErrMsg
mkDefltMsg deflt
  = ($$) (ptext SLIT("Binder in case default doesn't match type of scrutinee:"))
	    (ppr deflt)

mkAppMsg :: Type -> Type -> CoreExpr -> ErrMsg
mkAppMsg fun arg expr
  = vcat [ptext SLIT("Argument value doesn't match argument type:"),
	      hang (ptext SLIT("Fun type:")) 4 (ppr fun),
	      hang (ptext SLIT("Arg type:")) 4 (ppr arg),
	      hang (ptext SLIT("Expression:")) 4 (pprCoreExpr expr)]

mkKindErrMsg :: TyVar -> Type -> CoreExpr -> ErrMsg
mkKindErrMsg tyvar arg_ty expr
  = vcat [ptext SLIT("Kinds don't match in type application:"),
	  hang (ptext SLIT("Type variable:"))
		 4 (ppr tyvar <+> ptext SLIT("::") <+> ppr (tyVarKind tyvar)),
	  hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg_ty <+> ptext SLIT("::") <+> ppr (typeKind arg_ty)),
	  hang (ptext SLIT("Expression:")) 4 (pprCoreExpr expr)]

mkTyAppMsg :: FAST_STRING -> Type -> Type -> CoreExpr -> ErrMsg
mkTyAppMsg msg ty arg expr
  = vcat [hsep [ptext msg, ptext SLIT("type application:")],
	      hang (ptext SLIT("Exp type:"))
		 4 (ppr ty <+> ptext SLIT("::") <+> ppr (typeKind ty)),
	      hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg <+> ptext SLIT("::") <+> ppr (typeKind arg)),
	      hang (ptext SLIT("Expression:")) 4 (pprCoreExpr expr)]

mkAlgAltMsg1 :: Type -> ErrMsg
mkAlgAltMsg1 ty
  = ($$) (text "In some case statement, type of scrutinee is not a data type:")
	    (ppr ty)

mkAlgAltMsg2 :: Type -> Id -> ErrMsg
mkAlgAltMsg2 ty con
  = vcat [
	text "In some algebraic case alternative, constructor is not a constructor of scrutinee type:",
	ppr ty,
	ppr con
    ]

mkAlgAltMsg3 :: Id -> [Id] -> ErrMsg
mkAlgAltMsg3 con alts
  = vcat [
	text "In some algebraic case alternative, number of arguments doesn't match constructor:",
	ppr con,
	ppr alts
    ]

mkAlgAltMsg4 :: Type -> Id -> ErrMsg
mkAlgAltMsg4 ty arg
  = vcat [
	text "In some algebraic case alternative, type of argument doesn't match data constructor:",
	ppr ty,
	ppr arg
    ]

mkPrimAltMsg :: (Literal, CoreExpr) -> ErrMsg
mkPrimAltMsg alt
  = ($$)
    (text "In a primitive case alternative, type of literal doesn't match type of scrutinee:")
	    (ppr alt)

mkRhsMsg :: Id -> Type -> ErrMsg
mkRhsMsg binder ty
  = vcat
    [hsep [ptext SLIT("The type of this binder doesn't match the type of its RHS:"),
	    ppr binder],
     hsep [ptext SLIT("Binder's type:"), ppr (idType binder)],
     hsep [ptext SLIT("Rhs type:"), ppr ty]]

mkRhsPrimMsg :: Id -> CoreExpr -> ErrMsg
mkRhsPrimMsg binder rhs
  = vcat [hsep [ptext SLIT("The type of this binder is primitive:"),
		     ppr binder],
	      hsep [ptext SLIT("Binder's type:"), ppr (idType binder)]
	     ]

mkCoerceErr from_ty expr_ty
  = vcat [ptext SLIT("From-type of Coerce differs from type of enclosed expression"),
	  ptext SLIT("From-type:") <+> ppr from_ty,
	  ptext SLIT("Type of enclosed expr:") <+> ppr expr_ty
    ]
\end{code}
