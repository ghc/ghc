%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[CoreLint]{A ``lint'' pass to check for Core correctness}

\begin{code}
module CoreLint (
	lintCoreBindings,
	lintUnfolding, 
	showPass, endPass
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs		( idFreeVars )
import CoreUtils	( findDefault, exprOkForSpeculation, coreBindsSize )
import Unify      	( coreRefineTys )
import Bag
import Literal		( literalType )
import DataCon		( dataConRepType, isVanillaDataCon, dataConTyCon, dataConWorkId )
import TysWiredIn	( tupleCon )
import Var		( Var, Id, TyVar, idType, tyVarKind, mustHaveLocalBinding )
import VarSet
import Name		( getSrcLoc )
import PprCore
import ErrUtils		( dumpIfSet_core, ghcExit, Message, showPass,
			  mkLocMessage, debugTraceMsg )
import SrcLoc		( SrcLoc, noSrcLoc, mkSrcSpan )
import Type		( Type, tyVarsOfType, coreEqType,
			  splitFunTy_maybe, mkTyVarTys,
			  splitForAllTy_maybe, splitTyConApp_maybe,
			  isUnLiftedType, typeKind, mkForAllTy, mkFunTy,
			  isUnboxedTupleType, isSubKind,
			  substTyWith, emptyTvSubst, extendTvInScope, 
			  TvSubst, TvSubstEnv, mkTvSubst, setTvSubstEnv, substTy,
			  extendTvSubst, composeTvSubst, isInScope,
			  getTvSubstEnv, getTvInScope )
import TyCon		( isPrimTyCon )
import BasicTypes	( RecFlag(..), Boxity(..), isNonRec )
import StaticFlags	( opt_PprStyle_Debug )
import DynFlags		( DynFlags, DynFlag(..), dopt )
import Outputable

#ifdef DEBUG
import Util             ( notNull )
#endif

import Maybe

\end{code}

%************************************************************************
%*									*
\subsection{End pass}
%*									*
%************************************************************************

@showPass@ and @endPass@ don't really belong here, but it makes a convenient
place for them.  They print out stuff before and after core passes,
and do Core Lint when necessary.

\begin{code}
endPass :: DynFlags -> String -> DynFlag -> [CoreBind] -> IO [CoreBind]
endPass dflags pass_name dump_flag binds
  = do 
	-- Report result size if required
	-- This has the side effect of forcing the intermediate to be evaluated
	debugTraceMsg dflags $
		"    Result size = " ++ show (coreBindsSize binds)

	-- Report verbosely, if required
	dumpIfSet_core dflags dump_flag pass_name (pprCoreBindings binds)

	-- Type check
	lintCoreBindings dflags pass_name binds

	return binds
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

\begin{code}
lintCoreBindings :: DynFlags -> String -> [CoreBind] -> IO ()

lintCoreBindings dflags whoDunnit binds
  | not (dopt Opt_DoCoreLinting dflags)
  = return ()

lintCoreBindings dflags whoDunnit binds
  = case (initL (lint_binds binds)) of
      Nothing       -> showPass dflags ("Core Linted result of " ++ whoDunnit)
      Just bad_news -> printDump (display bad_news)	>>
		       ghcExit 1
  where
	-- Put all the top-level binders in scope at the start
	-- This is because transformation rules can bring something
	-- into use 'unexpectedly'
    lint_binds binds = addInScopeVars (bindersOfBinds binds) $
		       mapM lint_bind binds 

    lint_bind (Rec prs)		= mapM_ (lintSingleBinding Recursive) prs
    lint_bind (NonRec bndr rhs) = lintSingleBinding NonRecursive (bndr,rhs)

    display bad_news
      = vcat [  text ("*** Core Lint Errors: in result of " ++ whoDunnit ++ " ***"),
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
lintUnfolding :: SrcLoc
	      -> [Var]		-- Treat these as in scope
	      -> CoreExpr
	      -> Maybe Message	-- Nothing => OK

lintUnfolding locn vars expr
  = initL (addLoc (ImportedUnfolding locn) $
	   addInScopeVars vars	           $
	   lintCoreExpr expr)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreBinding]{lintCoreBinding}
%*									*
%************************************************************************

Check a core binding, returning the list of variables bound.

\begin{code}
lintSingleBinding rec_flag (binder,rhs)
  = addLoc (RhsOf binder) $
         -- Check the rhs 
    do { ty <- lintCoreExpr rhs	
       ; lintBinder binder -- Check match to RHS type
       ; binder_ty <- applySubst binder_ty
       ; checkTys binder_ty ty (mkRhsMsg binder ty)
        -- Check (not isUnLiftedType) (also checks for bogus unboxed tuples)
       ; checkL (not (isUnLiftedType binder_ty)
            || (isNonRec rec_flag && exprOkForSpeculation rhs))
 	   (mkRhsPrimMsg binder rhs)
        -- Check whether binder's specialisations contain any out-of-scope variables
       ; mapM_ (checkBndrIdInScope binder) bndr_vars }
	  
	-- We should check the unfolding, if any, but this is tricky because
	-- the unfolding is a SimplifiableCoreExpr. Give up for now.
  where
    binder_ty = idType binder
    bndr_vars = varSetElems (idFreeVars binder)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreExpr]{lintCoreExpr}
%*									*
%************************************************************************

\begin{code}
type InType  = Type	-- Substitution not yet applied
type OutType = Type	-- Substitution has been applied to this

lintCoreExpr :: CoreExpr -> LintM OutType
-- The returned type has the substitution from the monad 
-- already applied to it:
--	lintCoreExpr e subst = exprType (subst e)

lintCoreExpr (Var var)
  = do	{ checkIdInScope var 
 	; applySubst (idType var) }

lintCoreExpr (Lit lit)
  = return (literalType lit)

lintCoreExpr (Note (Coerce to_ty from_ty) expr)
  = do	{ expr_ty <- lintCoreExpr expr
	; to_ty <- lintTy to_ty
	; from_ty <- lintTy from_ty	
	; checkTys from_ty expr_ty (mkCoerceErr from_ty expr_ty)
	; return to_ty }

lintCoreExpr (Note other_note expr)
  = lintCoreExpr expr

lintCoreExpr (Let (NonRec bndr rhs) body)
  = do	{ lintSingleBinding NonRecursive (bndr,rhs)
	; addLoc (BodyOfLetRec [bndr])
		 (addInScopeVars [bndr] (lintCoreExpr body)) }

lintCoreExpr (Let (Rec pairs) body) 
  = addInScopeVars bndrs	$
    do	{ mapM (lintSingleBinding Recursive) pairs	
	; addLoc (BodyOfLetRec bndrs) (lintCoreExpr body) }
  where
    bndrs = map fst pairs

lintCoreExpr (App fun (Type ty))
-- This is like 'let' for types
-- It's needed when dealing with desugarer output for GADTs. Consider
--   data T = forall a. T a (a->Int) Bool
--    f :: T -> ... -> 
--    f (T x f True)  = <e1>
--    f (T y g False) = <e2>
-- After desugaring we get
--	f t b = case t of 
--		  T a (x::a) (f::a->Int) (b:Bool) ->
--		    case b of 
--			True -> <e1>
--			False -> (/\b. let y=x; g=f in <e2>) a
-- And for a reason I now forget, the ...<e2>... can mention a; so 
-- we want Lint to know that b=a.  Ugh.
--
-- I tried quite hard to make the necessity for this go away, by changing the 
-- desugarer, but the fundamental problem is this:
--	
--	T a (x::a) (y::Int) -> let fail::a = ...
--			       in (/\b. ...(case ... of       
--						True  -> x::b
--					 	False -> fail)
--				  ) a
-- Now the inner case look as though it has incompatible branches.
  = go fun [ty]
  where
    go (App fun (Type ty)) tys
	= do { go fun (ty:tys) }
    go (Lam tv body) (ty:tys)
	= do  { checkL (isTyVar tv) (mkKindErrMsg tv ty)	-- Not quite accurate
	      ; ty' <- lintTy ty; 
	      ; checkKinds tv ty'
		-- Now extend the substitution so we 
		-- take advantage of it in the body
	      ; addInScopeVars [tv] $
	        extendSubstL tv ty' $
		go body tys }
    go fun tys
	= do  { fun_ty <- lintCoreExpr fun
	      ; lintCoreArgs fun_ty (map Type tys) }

lintCoreExpr e@(App fun arg)
  = do	{ ty <- lintCoreExpr fun
	; addLoc (AnExpr e) $
          lintCoreArg ty arg }

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var) $
    do	{ body_ty <- addInScopeVars [var] $
                     lintCoreExpr expr
	; if isId var then do
		{ var_ty <- lintId var 	
		; return (mkFunTy var_ty body_ty) }
	  else
		return (mkForAllTy var body_ty)
	}
	-- The applySubst is needed to apply the subst to var

lintCoreExpr e@(Case scrut var alt_ty alts) =
       -- Check the scrutinee
  do { scrut_ty <- lintCoreExpr scrut
     ; alt_ty   <- lintTy alt_ty  
     ; var_ty   <- lintTy (idType var)	
	-- Don't use lintId on var, because unboxed tuple is legitimate

     ; checkTys var_ty scrut_ty (mkScrutMsg var scrut_ty)

     -- If the binder is an unboxed tuple type, don't put it in scope
     ; let vars = if (isUnboxedTupleType (idType var)) then [] else [var]
     ; addInScopeVars vars $
       do { -- Check the alternatives
            checkCaseAlts e scrut_ty alts
          ; mapM (lintCoreAlt scrut_ty alt_ty) alts
          ; return alt_ty } }

lintCoreExpr e@(Type ty)
  = addErrL (mkStrangeTyMsg e)
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArgs]{lintCoreArgs}
%*									*
%************************************************************************

The basic version of these functions checks that the argument is a
subtype of the required type, as one would expect.

\begin{code}
lintCoreArgs :: Type -> [CoreArg] -> LintM Type
lintCoreArg  :: Type -> CoreArg   -> LintM Type
-- First argument has already had substitution applied to it
\end{code}

\begin{code}
lintCoreArgs ty [] = return ty
lintCoreArgs ty (a : args) = 
  do { res <- lintCoreArg ty a
     ; lintCoreArgs res args }

lintCoreArg ty a@(Type arg_ty) = 
  do { arg_ty <- lintTy arg_ty	
     ; lintTyApp ty arg_ty }

lintCoreArg fun_ty arg = 
       -- Make sure function type matches argument
  do { arg_ty <- lintCoreExpr arg
     ; let err = mkAppMsg fun_ty arg_ty
     ; case splitFunTy_maybe fun_ty of
        Just (arg,res) -> 
          do { checkTys arg arg_ty err 
             ; return res }
        _ -> addErrL err }
\end{code}

\begin{code}
-- Both args have had substitution applied
lintTyApp ty arg_ty 
  = case splitForAllTy_maybe ty of
      Nothing -> addErrL (mkTyAppMsg ty arg_ty)

      Just (tyvar,body)
        -> do	{ checkL (isTyVar tyvar) (mkTyAppMsg ty arg_ty)
		; checkKinds tyvar arg_ty
		; return (substTyWith [tyvar] [arg_ty] body) }

lintTyApps fun_ty [] = return fun_ty

lintTyApps fun_ty (arg_ty : arg_tys) = 
  do { fun_ty' <- lintTyApp fun_ty arg_ty
     ; lintTyApps fun_ty' arg_tys }

checkKinds tyvar arg_ty
	-- Arg type might be boxed for a function with an uncommitted
	-- tyvar; notably this is used so that we can give
	-- 	error :: forall a:*. String -> a
	-- and then apply it to both boxed and unboxed types.
  = checkL (argty_kind `isSubKind` tyvar_kind)
	   (mkKindErrMsg tyvar arg_ty)
  where
    tyvar_kind = tyVarKind tyvar
    argty_kind = typeKind arg_ty
\end{code}


%************************************************************************
%*									*
\subsection[lintCoreAlts]{lintCoreAlts}
%*									*
%************************************************************************

\begin{code}
checkCaseAlts :: CoreExpr -> OutType -> [CoreAlt] -> LintM ()
-- a) Check that the alts are non-empty
-- b1) Check that the DEFAULT comes first, if it exists
-- b2) Check that the others are in increasing order
-- c) Check that there's a default for infinite types
-- NB: Algebraic cases are not necessarily exhaustive, because
--     the simplifer correctly eliminates case that can't 
--     possibly match.

checkCaseAlts e ty [] 
  = addErrL (mkNullAltsMsg e)

checkCaseAlts e ty alts = 
  do { checkL (all non_deflt con_alts) (mkNonDefltMsg e)
     ; checkL (increasing_tag con_alts) (mkNonIncreasingAltsMsg e)
     ; checkL (isJust maybe_deflt || not is_infinite_ty)
	   (nonExhaustiveAltsMsg e) }
  where
    (con_alts, maybe_deflt) = findDefault alts

	-- Check that successive alternatives have increasing tags 
    increasing_tag (alt1 : rest@( alt2 : _)) = alt1 `ltAlt` alt2 && increasing_tag rest
    increasing_tag other 		     = True

    non_deflt (DEFAULT, _, _) = False
    non_deflt alt	      = True

    is_infinite_ty = case splitTyConApp_maybe ty of
			Nothing			    -> False
			Just (tycon, tycon_arg_tys) -> isPrimTyCon tycon
\end{code}

\begin{code}
checkAltExpr :: CoreExpr -> OutType -> LintM ()
checkAltExpr expr ann_ty
  = do { actual_ty <- lintCoreExpr expr 
       ; checkTys actual_ty ann_ty (mkCaseAltMsg expr actual_ty ann_ty) }

lintCoreAlt :: OutType 		-- Type of scrutinee
            -> OutType          -- Type of the alternative
	    -> CoreAlt
	    -> LintM ()

lintCoreAlt scrut_ty alt_ty alt@(DEFAULT, args, rhs) = 
  do { checkL (null args) (mkDefaultArgsMsg args)
     ; checkAltExpr rhs alt_ty }

lintCoreAlt scrut_ty alt_ty alt@(LitAlt lit, args, rhs) = 
  do { checkL (null args) (mkDefaultArgsMsg args)
     ; checkTys lit_ty scrut_ty (mkBadPatMsg lit_ty scrut_ty)	
     ; checkAltExpr rhs alt_ty } 
  where
    lit_ty = literalType lit

lintCoreAlt scrut_ty alt_ty alt@(DataAlt con, args, rhs)
  | Just (tycon, tycon_arg_tys) <- splitTyConApp_maybe scrut_ty,
    tycon == dataConTyCon con
  = addLoc (CaseAlt alt) $
    addInScopeVars args $	-- Put the args in scope before lintBinder,
				-- because the Ids mention the type variables
    if isVanillaDataCon con then
    do	{ mapM lintBinder args 
         	-- FIX! Add check that all args are Ids.
		 -- Check the pattern
		 -- Scrutinee type must be a tycon applicn; checked by caller
		 -- This code is remarkably compact considering what it does!
		 -- NB: args must be in scope here so that the lintCoreArgs line works.
	         -- NB: relies on existential type args coming *after* ordinary type args

	; con_type <- lintTyApps (dataConRepType con) tycon_arg_tys
                 -- Can just map Var as we know that this is a vanilla datacon
	; con_result_ty <- lintCoreArgs con_type (map Var args)
	; checkTys con_result_ty scrut_ty (mkBadPatMsg con_result_ty scrut_ty) 
 	         -- Check the RHS
	; checkAltExpr rhs alt_ty }

    else 	-- GADT
    do	{ let (tvs,ids) = span isTyVar args
        ; subst <- getTvSubst 
	; let in_scope  = getTvInScope subst
	      subst_env = getTvSubstEnv subst
        ; case coreRefineTys in_scope con tvs scrut_ty of {
             Nothing          -> return () ;	-- Alternative is dead code
             Just (refine, _) -> updateTvSubstEnv (composeTvSubst in_scope refine subst_env) $
    do 	{ tvs'     <- mapM lintTy (mkTyVarTys tvs)
	; con_type <- lintTyApps (dataConRepType con) tvs'
	; mapM lintBinder ids	-- Lint Ids in the refined world
	; lintCoreArgs con_type (map Var ids)
	; let refined_alt_ty = substTy (mkTvSubst in_scope refine) alt_ty
		-- alt_ty is already an OutType, so don't re-apply 
		-- the current substitution.  But we must apply the
		-- refinement so that the check in checkAltExpr is ok
	; checkAltExpr rhs refined_alt_ty
    } } }

  | otherwise	-- Scrut-ty is wrong shape
  = addErrL (mkBadAltMsg scrut_ty alt)
\end{code}

%************************************************************************
%*									*
\subsection[lint-types]{Types}
%*									*
%************************************************************************

\begin{code}
lintBinder :: Var -> LintM ()
lintBinder var | isId var  = lintId var >> return ()
	       | otherwise = return ()

lintId :: Var -> LintM OutType
-- ToDo: lint its rules
lintId id
  = do 	{ checkL (not (isUnboxedTupleType (idType id))) 
		 (mkUnboxedTupleMsg id)
		-- No variable can be bound to an unboxed tuple.
	; lintTy (idType id) }

lintTy :: InType -> LintM OutType
-- Check the type, and apply the substitution to it
-- ToDo: check the kind structure of the type
lintTy ty 
  = do	{ ty' <- applySubst ty
	; mapM_ checkIdInScope (varSetElems (tyVarsOfType ty'))
	; return ty' }
\end{code}

    
%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
newtype LintM a = 
   LintM { unLintM :: 
            [LintLocInfo] ->         -- Locations
            TvSubst ->               -- Current type substitution; we also use this
				     -- to keep track of all the variables in scope,
				     -- both Ids and TyVars
	    Bag Message ->           -- Error messages so far
	    (Maybe a, Bag Message) } -- Result and error messages (if any)

instance Monad LintM where
  return x = LintM (\ loc subst errs -> (Just x, errs))
  fail err = LintM (\ loc subst errs -> (Nothing, addErr subst errs (text err) loc))
  m >>= k  = LintM (\ loc subst errs -> 
                       let (res, errs') = unLintM m loc subst errs in
                         case res of
                           Just r -> unLintM (k r) loc subst errs'
                           Nothing -> (Nothing, errs'))

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf Id	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | CaseAlt CoreAlt	-- Pattern of a case alternative
  | AnExpr CoreExpr	-- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
\end{code}

                 
\begin{code}
initL :: LintM a -> Maybe Message {- errors -}
initL m
  = case unLintM m [] emptyTvSubst emptyBag of
      (_, errs) | isEmptyBag errs -> Nothing
		| otherwise	  -> Just (vcat (punctuate (text "") (bagToList errs)))
\end{code}

\begin{code}
checkL :: Bool -> Message -> LintM ()
checkL True  msg = return ()
checkL False msg = addErrL msg

addErrL :: Message -> LintM a
addErrL msg = LintM (\ loc subst errs -> (Nothing, addErr subst errs msg loc))

addErr :: TvSubst -> Bag Message -> Message -> [LintLocInfo] -> Bag Message
addErr subst errs_so_far msg locs
  = ASSERT( notNull locs )
    errs_so_far `snocBag` mk_msg msg
  where
   (loc, cxt1) = dumpLoc (head locs)
   cxts        = [snd (dumpLoc loc) | loc <- locs]   
   context     | opt_PprStyle_Debug = vcat (reverse cxts) $$ cxt1 $$
				      ptext SLIT("Substitution:") <+> ppr subst
	       | otherwise	    = cxt1
 
   mk_msg msg = mkLocMessage (mkSrcSpan loc loc) (context $$ msg)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m =
  LintM (\ loc subst errs -> unLintM m (extra_loc:loc) subst errs)

addInScopeVars :: [Var] -> LintM a -> LintM a
addInScopeVars vars m = 
  LintM (\ loc subst errs -> unLintM m loc (extendTvInScope subst vars) errs)

updateTvSubstEnv :: TvSubstEnv -> LintM a -> LintM a
updateTvSubstEnv substenv m = 
  LintM (\ loc subst errs -> unLintM m loc (setTvSubstEnv subst substenv) errs)

getTvSubst :: LintM TvSubst
getTvSubst = LintM (\ loc subst errs -> (Just subst, errs))

applySubst :: Type -> LintM Type
applySubst ty = do { subst <- getTvSubst; return (substTy subst ty) }

extendSubstL :: TyVar -> Type -> LintM a -> LintM a
extendSubstL tv ty m
  = LintM (\ loc subst errs -> unLintM m loc (extendTvSubst subst tv ty) errs)
\end{code}

\begin{code}
checkIdInScope :: Var -> LintM ()
checkIdInScope id 
  = do { checkL (not (id == oneTupleDataConId))
		(ptext SLIT("Illegal one-tuple"))
       ; checkInScope (ptext SLIT("is out of scope")) id }

oneTupleDataConId :: Id	-- Should not happen
oneTupleDataConId = dataConWorkId (tupleCon Boxed 1)

checkBndrIdInScope :: Var -> Var -> LintM ()
checkBndrIdInScope binder id 
  = checkInScope msg id
    where
     msg = ptext SLIT("is out of scope inside info for") <+> 
	   ppr binder

checkInScope :: SDoc -> Var -> LintM ()
checkInScope loc_msg var =
 do { subst <- getTvSubst
    ; checkL (not (mustHaveLocalBinding var) || (var `isInScope` subst))
             (hsep [ppr var, loc_msg]) }

checkTys :: Type -> Type -> Message -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have alrady had the substitution applied
checkTys ty1 ty2 msg = checkL (ty1 `coreEqType` ty2) msg
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dumpLoc (RhsOf v)
  = (getSrcLoc v, brackets (ptext SLIT("RHS of") <+> pp_binders [v]))

dumpLoc (LambdaBodyOf b)
  = (getSrcLoc b, brackets (ptext SLIT("in body of lambda with binder") <+> pp_binder b))

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, brackets (ptext SLIT("In body of a letrec with no binders")))

dumpLoc (BodyOfLetRec bs@(_:_))
  = ( getSrcLoc (head bs), brackets (ptext SLIT("in body of letrec with binders") <+> pp_binders bs))

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, rhs))
  = (noSrcLoc, text "In a case alternative:" <+> parens (ppr con <+> ppr args))

dumpLoc (ImportedUnfolding locn)
  = (locn, brackets (ptext SLIT("in an imported unfolding")))

pp_binders :: [Var] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Var -> SDoc
pp_binder b | isId b    = hsep [ppr b, dcolon, ppr (idType b)]
            | isTyVar b = hsep [ppr b, dcolon, ppr (tyVarKind b)]
\end{code}

\begin{code}
------------------------------------------------------
--	Messages for case expressions

mkNullAltsMsg :: CoreExpr -> Message
mkNullAltsMsg e 
  = hang (text "Case expression with no alternatives:")
	 4 (ppr e)

mkDefaultArgsMsg :: [Var] -> Message
mkDefaultArgsMsg args 
  = hang (text "DEFAULT case with binders")
	 4 (ppr args)

mkCaseAltMsg :: CoreExpr -> Type -> Type -> Message
mkCaseAltMsg e ty1 ty2
  = hang (text "Type of case alternatives not the same as the annotation on case:")
	 4 (vcat [ppr ty1, ppr ty2, ppr e])

mkScrutMsg :: Id -> Type -> Message
mkScrutMsg var scrut_ty
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
	  text "Result binder type:" <+> ppr (idType var),
	  text "Scrutinee type:" <+> ppr scrut_ty]


mkNonDefltMsg e
  = hang (text "Case expression with DEFAULT not at the beginnning") 4 (ppr e)
mkNonIncreasingAltsMsg e
  = hang (text "Case expression with badly-ordered alternatives") 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> Message
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives") 4 (ppr e)

mkBadPatMsg :: Type -> Type -> Message
mkBadPatMsg con_result_ty scrut_ty
  = vcat [
	text "In a case alternative, pattern result type doesn't match scrutinee type:",
	text "Pattern result type:" <+> ppr con_result_ty,
	text "Scrutinee type:" <+> ppr scrut_ty
    ]

mkBadAltMsg :: Type -> CoreAlt -> Message
mkBadAltMsg scrut_ty alt
  = vcat [ text "Data alternative when scrutinee is not a tycon application",
	   text "Scrutinee type:" <+> ppr scrut_ty,
	   text "Alternative:" <+> pprCoreAlt alt ]

------------------------------------------------------
--	Other error messages

mkAppMsg :: Type -> Type -> Message
mkAppMsg fun arg
  = vcat [ptext SLIT("Argument value doesn't match argument type:"),
	      hang (ptext SLIT("Fun type:")) 4 (ppr fun),
	      hang (ptext SLIT("Arg type:")) 4 (ppr arg)]

mkKindErrMsg :: TyVar -> Type -> Message
mkKindErrMsg tyvar arg_ty
  = vcat [ptext SLIT("Kinds don't match in type application:"),
	  hang (ptext SLIT("Type variable:"))
		 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
	  hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkTyAppMsg :: Type -> Type -> Message
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
	      hang (ptext SLIT("Exp type:"))
		 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
	      hang (ptext SLIT("Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkRhsMsg :: Id -> Type -> Message
mkRhsMsg binder ty
  = vcat
    [hsep [ptext SLIT("The type of this binder doesn't match the type of its RHS:"),
	    ppr binder],
     hsep [ptext SLIT("Binder's type:"), ppr (idType binder)],
     hsep [ptext SLIT("Rhs type:"), ppr ty]]

mkRhsPrimMsg :: Id -> CoreExpr -> Message
mkRhsPrimMsg binder rhs
  = vcat [hsep [ptext SLIT("The type of this binder is primitive:"),
		     ppr binder],
	      hsep [ptext SLIT("Binder's type:"), ppr (idType binder)]
	     ]

mkUnboxedTupleMsg :: Id -> Message
mkUnboxedTupleMsg binder
  = vcat [hsep [ptext SLIT("A variable has unboxed tuple type:"), ppr binder],
	  hsep [ptext SLIT("Binder's type:"), ppr (idType binder)]]

mkCoerceErr from_ty expr_ty
  = vcat [ptext SLIT("From-type of Coerce differs from type of enclosed expression"),
	  ptext SLIT("From-type:") <+> ppr from_ty,
	  ptext SLIT("Type of enclosed expr:") <+> ppr expr_ty
    ]

mkStrangeTyMsg e
  = ptext SLIT("Type where expression expected:") <+> ppr e
\end{code}
