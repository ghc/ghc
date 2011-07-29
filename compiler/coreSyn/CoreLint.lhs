
%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

A ``lint'' pass to check for Core correctness

\begin{code}
module CoreLint ( lintCoreBindings, lintUnfolding ) where

#include "HsVersions.h"

import Demand
import CoreSyn
import CoreFVs
import CoreUtils
import Pair
import Bag
import Literal
import DataCon
import TysWiredIn
import Var
import VarEnv
import VarSet
import Name
import Id
import PprCore
import ErrUtils
import SrcLoc
import Kind
import Type
import TypeRep
import Coercion
import TyCon
import Class
import BasicTypes
import StaticFlags
import ListSetOps
import PrelNames
import Outputable
import FastString
import Util
import Control.Monad
import Data.Maybe
import Data.Traversable (traverse)
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
    --  * Unsaturated type app before specialisation has been done;
    --
    --  * Oversaturated type app after specialisation (eta reduction
    --   may well be happening...);


Note [Linting type lets]
~~~~~~~~~~~~~~~~~~~~~~~~
In the desugarer, it's very very convenient to be able to say (in effect)
	let a = Type Int in <body>
That is, use a type let.   See Note [Type let] in CoreSyn.

However, when linting <body> we need to remember that a=Int, else we might
reject a correct program.  So we carry a type substitution (in this example 
[a -> Int]) and apply this substitution before comparing types.  The functin
	lintInTy :: Type -> LintM Type
returns a substituted type; that's the only reason it returns anything.

When we encounter a binder (like x::a) we must apply the substitution
to the type of the binding variable.  lintBinders does this.

For Ids, the type-substituted Id is added to the in_scope set (which 
itself is part of the TvSubst we are carrying down), and when we
find an occurence of an Id, we fetch it from the in-scope set.


\begin{code}
lintCoreBindings :: [CoreBind] -> (Bag Message, Bag Message)
--   Returns (warnings, errors)
lintCoreBindings binds
  = initL $ 
    addLoc TopLevelBindings $
    addInScopeVars binders  $
	-- Put all the top-level binders in scope at the start
	-- This is because transformation rules can bring something
	-- into use 'unexpectedly'
    do { checkL (null dups) (dupVars dups)
       ; checkL (null ext_dups) (dupExtVars ext_dups)
       ; mapM lint_bind binds }
  where
    binders = bindersOfBinds binds
    (_, dups) = removeDups compare binders

    -- dups_ext checks for names with different uniques
    -- but but the same External name M.n.  We don't
    -- allow this at top level:
    --    M.n{r3}  = ...
    --    M.n{r29} = ...
    -- becuase they both get the same linker symbol
    ext_dups = snd (removeDups ord_ext (map Var.varName binders))
    ord_ext n1 n2 | Just m1 <- nameModule_maybe n1
                  , Just m2 <- nameModule_maybe n2
                  = compare (m1, nameOccName n1) (m2, nameOccName n2)
                  | otherwise = LT

    lint_bind (Rec prs)		= mapM_ (lintSingleBinding TopLevel Recursive) prs
    lint_bind (NonRec bndr rhs) = lintSingleBinding TopLevel NonRecursive (bndr,rhs)
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
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL (addLoc (ImportedUnfolding locn) $
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
lintSingleBinding :: TopLevelFlag -> RecFlag -> (Id, CoreExpr) -> LintM ()
lintSingleBinding top_lvl_flag rec_flag (binder,rhs)
  = addLoc (RhsOf binder) $
         -- Check the rhs 
    do { ty <- lintCoreExpr rhs	
       ; lintBinder binder -- Check match to RHS type
       ; binder_ty <- applySubstTy binder_ty
       ; checkTys binder_ty ty (mkRhsMsg binder ty)
        -- Check (not isUnLiftedType) (also checks for bogus unboxed tuples)
       ; checkL (not (isUnLiftedType binder_ty)
            || (isNonRec rec_flag && exprOkForSpeculation rhs))
 	   (mkRhsPrimMsg binder rhs)
        -- Check that if the binder is top-level or recursive, it's not demanded
       ; checkL (not (isStrictId binder)
            || (isNonRec rec_flag && not (isTopLevel top_lvl_flag)))
           (mkStrictMsg binder)
        -- Check whether binder's specialisations contain any out-of-scope variables
       ; mapM_ (checkBndrIdInScope binder) bndr_vars 

       ; when (isStrongLoopBreaker (idOccInfo binder) && isInlinePragma (idInlinePragma binder))
              (addWarnL (ptext (sLit "INLINE binder is (non-rule) loop breaker:") <+> ppr binder))
	      -- Only non-rule loop breakers inhibit inlining

      -- Check whether arity and demand type are consistent (only if demand analysis
      -- already happened)
       ; checkL (case maybeDmdTy of
                  Just (StrictSig dmd_ty) -> idArity binder >= dmdTypeDepth dmd_ty || exprIsTrivial rhs
                  Nothing -> True)
           (mkArityMsg binder) }
	  
	-- We should check the unfolding, if any, but this is tricky because
 	-- the unfolding is a SimplifiableCoreExpr. Give up for now.
   where
    binder_ty                  = idType binder
    maybeDmdTy                 = idStrictness_maybe binder
    bndr_vars                  = varSetElems (idFreeVars binder)
    lintBinder var | isId var  = lintIdBndr var $ \_ -> (return ())
	           | otherwise = return ()
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreExpr]{lintCoreExpr}
%*									*
%************************************************************************

\begin{code}
type InType      = Type	-- Substitution not yet applied
type InCoercion  = Coercion
type InVar       = Var
type InTyVar     = TyVar

type OutType     = Type	-- Substitution has been applied to this
type OutCoercion = Coercion
type OutVar      = Var
type OutTyVar    = TyVar

lintCoreExpr :: CoreExpr -> LintM OutType
-- The returned type has the substitution from the monad 
-- already applied to it:
--	lintCoreExpr e subst = exprType (subst e)
--
-- The returned "type" can be a kind, if the expression is (Type ty)

lintCoreExpr (Var var)
  = do	{ checkL (not (var == oneTupleDataConId))
		 (ptext (sLit "Illegal one-tuple"))

        ; checkL (isId var && not (isCoVar var))
                 (ptext (sLit "Non term variable") <+> ppr var)

        ; checkDeadIdOcc var
	; var' <- lookupIdInScope var
        ; return (idType var') }

lintCoreExpr (Lit lit)
  = return (literalType lit)

lintCoreExpr (Cast expr co)
  = do { expr_ty <- lintCoreExpr expr
       ; co' <- applySubstCo co
       ; (from_ty, to_ty) <- lintCoercion co'
       ; checkTys from_ty expr_ty (mkCastErr from_ty expr_ty)
       ; return to_ty }

lintCoreExpr (Note _ expr)
  = lintCoreExpr expr

lintCoreExpr (Let (NonRec tv (Type ty)) body)
  | isTyVar tv
  =	-- See Note [Linting type lets]
    do	{ ty' <- addLoc (RhsOf tv) $ lintInTy ty
        ; lintTyBndr tv              $ \ tv' -> 
          addLoc (BodyOfLetRec [tv]) $ 
          extendSubstL tv' ty'       $ do
        { checkTyKind tv' ty'
		-- Now extend the substitution so we 
		-- take advantage of it in the body
        ; lintCoreExpr body } }

lintCoreExpr (Let (NonRec bndr rhs) body)
  | isId bndr
  = do	{ lintSingleBinding NotTopLevel NonRecursive (bndr,rhs)
	; addLoc (BodyOfLetRec [bndr]) 
		 (lintAndScopeId bndr $ \_ -> (lintCoreExpr body)) }

  | otherwise
  = failWithL (mkLetErr bndr rhs)	-- Not quite accurate

lintCoreExpr (Let (Rec pairs) body) 
  = lintAndScopeIds bndrs	$ \_ ->
    do	{ checkL (null dups) (dupVars dups)
        ; mapM_ (lintSingleBinding NotTopLevel Recursive) pairs	
	; addLoc (BodyOfLetRec bndrs) (lintCoreExpr body) }
  where
    bndrs = map fst pairs
    (_, dups) = removeDups compare bndrs

lintCoreExpr e@(App fun arg)
  = do	{ fun_ty <- lintCoreExpr fun
	; addLoc (AnExpr e) $
          lintCoreArg fun_ty arg }

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var) $
    lintBinders [var] $ \ vars' ->
    do { let [var'] = vars'  
       ; body_ty <- lintCoreExpr expr
       ; if isId var' then 
             return (mkFunTy (idType var') body_ty) 
	 else
	     return (mkForAllTy var' body_ty)
       }
	-- The applySubstTy is needed to apply the subst to var

lintCoreExpr e@(Case scrut var alt_ty alts) =
       -- Check the scrutinee
  do { scrut_ty <- lintCoreExpr scrut
     ; alt_ty   <- lintInTy alt_ty  
     ; var_ty   <- lintInTy (idType var)	

     ; let mb_tc_app = splitTyConApp_maybe (idType var)
     ; case mb_tc_app of 
         Just (tycon, _)
              | debugIsOn &&
                isAlgTyCon tycon && 
		not (isFamilyTyCon tycon || isAbstractTyCon tycon) &&
                null (tyConDataCons tycon) -> 
                  pprTrace "Lint warning: case binder's type has no constructors" (ppr var <+> ppr (idType var))
			-- This can legitimately happen for type families
                      $ return ()
         _otherwise -> return ()

	-- Don't use lintIdBndr on var, because unboxed tuple is legitimate

     ; subst <- getTvSubst 
     ; checkTys var_ty scrut_ty (mkScrutMsg var var_ty scrut_ty subst)

     -- If the binder is an unboxed tuple type, don't put it in scope
     ; let scope = if (isUnboxedTupleType (idType var)) then 
                       pass_var 
                   else lintAndScopeId var
     ; scope $ \_ ->
       do { -- Check the alternatives
            mapM_ (lintCoreAlt scrut_ty alt_ty) alts
          ; checkCaseAlts e scrut_ty alts
          ; return alt_ty } }
  where
    pass_var f = f var

lintCoreExpr (Type ty)
  = do { ty' <- lintInTy ty
       ; return (typeKind ty') }

lintCoreExpr (Coercion co)
  = do { co' <- lintInCo co
       ; let Pair ty1 ty2 = coercionKind co'
       ; return (mkPredTy $ EqPred ty1 ty2) }
\end{code}

%************************************************************************
%*									*
\subsection[lintCoreArgs]{lintCoreArgs}
%*									*
%************************************************************************

The basic version of these functions checks that the argument is a
subtype of the required type, as one would expect.

\begin{code}
lintCoreArg  :: OutType -> CoreArg -> LintM OutType
lintCoreArg fun_ty (Type arg_ty)
  = do { arg_ty' <- applySubstTy arg_ty
       ; lintTyApp fun_ty arg_ty' }

lintCoreArg fun_ty arg
  = do { arg_ty <- lintCoreExpr arg
       ; lintValApp arg fun_ty arg_ty }

-----------------
lintAltBinders :: OutType     -- Scrutinee type
	       -> OutType     -- Constructor type
               -> [OutVar]    -- Binders
               -> LintM ()
lintAltBinders scrut_ty con_ty [] 
  = checkTys con_ty scrut_ty (mkBadPatMsg con_ty scrut_ty) 
lintAltBinders scrut_ty con_ty (bndr:bndrs)
  | isTyVar bndr
  = do { con_ty' <- lintTyApp con_ty (mkTyVarTy bndr)
       ; lintAltBinders scrut_ty con_ty' bndrs }
  | otherwise
  = do { con_ty' <- lintValApp (Var bndr) con_ty (idType bndr)
       ; lintAltBinders scrut_ty con_ty' bndrs } 

-----------------
lintTyApp :: OutType -> OutType -> LintM OutType
lintTyApp fun_ty arg_ty
  | Just (tyvar,body_ty) <- splitForAllTy_maybe fun_ty
  , isTyVar tyvar
  = do	{ checkTyKind tyvar arg_ty
        ; return (substTyWith [tyvar] [arg_ty] body_ty) }

  | otherwise
  = failWithL (mkTyAppMsg fun_ty arg_ty)
   
-----------------
lintValApp :: CoreExpr -> OutType -> OutType -> LintM OutType
lintValApp arg fun_ty arg_ty
  | Just (arg,res) <- splitFunTy_maybe fun_ty
  = do { checkTys arg arg_ty err1
       ; return res }
  | otherwise
  = failWithL err2
  where
    err1 = mkAppMsg       fun_ty arg_ty arg
    err2 = mkNonFunAppMsg fun_ty arg_ty arg
\end{code}

\begin{code}
checkTyKind :: OutTyVar -> OutType -> LintM ()
-- Both args have had substitution applied
checkTyKind tyvar arg_ty
	-- Arg type might be boxed for a function with an uncommitted
	-- tyvar; notably this is used so that we can give
	-- 	error :: forall a:*. String -> a
	-- and then apply it to both boxed and unboxed types.
  = do { arg_kind <- lintType arg_ty
       ; unless (arg_kind `isSubKind` tyvar_kind)
                (addErrL (mkKindErrMsg tyvar arg_ty)) }
  where
    tyvar_kind = tyVarKind tyvar

-- Check that the kinds of a type variable and a coercion match, that
-- is, if tv :: k  then co :: t1 ~ t2  where t1 :: k and t2 :: k.
checkTyCoKind :: TyVar -> OutCoercion -> LintM (OutType, OutType)
checkTyCoKind tv co
  = do { (t1,t2) <- lintCoercion co
       ; k1      <- lintType t1
       ; k2      <- lintType t2
       ; unless ((k1 `isSubKind` tyvar_kind) && (k2 `isSubKind` tyvar_kind))
                (addErrL (mkTyCoAppErrMsg tv co))
       ; return (t1,t2) }
  where 
    tyvar_kind = tyVarKind tv

checkTyCoKinds :: [TyVar] -> [OutCoercion] -> LintM [(OutType, OutType)]
checkTyCoKinds = zipWithM checkTyCoKind

checkDeadIdOcc :: Id -> LintM ()
-- Occurrences of an Id should never be dead....
-- except when we are checking a case pattern
checkDeadIdOcc id
  | isDeadOcc (idOccInfo id)
  = do { in_case <- inCasePat
       ; checkL in_case
		(ptext (sLit "Occurrence of a dead Id") <+> ppr id) }
  | otherwise
  = return ()
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

checkCaseAlts e _ []
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
    increasing_tag _                         = True

    non_deflt (DEFAULT, _, _) = False
    non_deflt _               = True

    is_infinite_ty = case splitTyConApp_maybe ty of
                        Nothing         -> False
                        Just (tycon, _) -> isPrimTyCon tycon
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

lintCoreAlt _ alt_ty (DEFAULT, args, rhs) =
  do { checkL (null args) (mkDefaultArgsMsg args)
     ; checkAltExpr rhs alt_ty }

lintCoreAlt scrut_ty alt_ty (LitAlt lit, args, rhs) = 
  do { checkL (null args) (mkDefaultArgsMsg args)
     ; checkTys lit_ty scrut_ty (mkBadPatMsg lit_ty scrut_ty)	
     ; checkAltExpr rhs alt_ty } 
  where
    lit_ty = literalType lit

lintCoreAlt scrut_ty alt_ty alt@(DataAlt con, args, rhs)
  | isNewTyCon (dataConTyCon con) 
  = addErrL (mkNewTyDataConAltMsg scrut_ty alt)
  | Just (tycon, tycon_arg_tys) <- splitTyConApp_maybe scrut_ty
  = addLoc (CaseAlt alt) $  do
    {   -- First instantiate the universally quantified 
	-- type variables of the data constructor
	-- We've already check
      checkL (tycon == dataConTyCon con) (mkBadConMsg tycon con)
    ; let con_payload_ty = applyTys (dataConRepType con) tycon_arg_tys

	-- And now bring the new binders into scope
    ; lintBinders args $ \ args' -> do
    { addLoc (CasePat alt) (lintAltBinders scrut_ty con_payload_ty args')
    ; checkAltExpr rhs alt_ty } }

  | otherwise	-- Scrut-ty is wrong shape
  = addErrL (mkBadAltMsg scrut_ty alt)
\end{code}

%************************************************************************
%*									*
\subsection[lint-types]{Types}
%*									*
%************************************************************************

\begin{code}
-- When we lint binders, we (one at a time and in order):
--  1. Lint var types or kinds (possibly substituting)
--  2. Add the binder to the in scope set, and if its a coercion var,
--     we may extend the substitution to reflect its (possibly) new kind
lintBinders :: [Var] -> ([Var] -> LintM a) -> LintM a
lintBinders [] linterF = linterF []
lintBinders (var:vars) linterF = lintBinder var $ \var' ->
				 lintBinders vars $ \ vars' ->
				 linterF (var':vars')

lintBinder :: Var -> (Var -> LintM a) -> LintM a
lintBinder var linterF
  | isId var  = lintIdBndr var linterF
  | otherwise = lintTyBndr var linterF

lintTyBndr :: InTyVar -> (OutTyVar -> LintM a) -> LintM a
lintTyBndr tv thing_inside
  = do { subst <- getTvSubst
       ; let (subst', tv') = Type.substTyVarBndr subst tv
       ; lintTyBndrKind tv'
       ; updateTvSubst subst' (thing_inside tv') }

lintIdBndr :: Id -> (Id -> LintM a) -> LintM a
-- Do substitution on the type of a binder and add the var with this 
-- new type to the in-scope set of the second argument
-- ToDo: lint its rules

lintIdBndr id linterF 
  = do 	{ checkL (not (isUnboxedTupleType (idType id))) 
		 (mkUnboxedTupleMsg id)
		-- No variable can be bound to an unboxed tuple.
        ; lintAndScopeId id $ \id' -> linterF id' }

lintAndScopeIds :: [Var] -> ([Var] -> LintM a) -> LintM a
lintAndScopeIds ids linterF 
  = go ids
  where
    go []       = linterF []
    go (id:ids) = lintAndScopeId id $ \id ->
                  lintAndScopeIds ids $ \ids ->
                  linterF (id:ids)

lintAndScopeId :: InVar -> (OutVar -> LintM a) -> LintM a
lintAndScopeId id linterF 
  = do { ty <- lintInTy (idType id)
       ; let id' = setIdType id ty
       ; addInScopeVar id' $ (linterF id') }
\end{code}


%************************************************************************
%*									*
\subsection[lint-monad]{The Lint monad}
%*									*
%************************************************************************

\begin{code}
lintInTy :: InType -> LintM OutType
-- Check the type, and apply the substitution to it
-- See Note [Linting type lets]
-- ToDo: check the kind structure of the type
lintInTy ty 
  = addLoc (InType ty) $
    do	{ ty' <- applySubstTy ty
	; _ <- lintType ty'
	; return ty' }

lintInCo :: InCoercion -> LintM OutCoercion
-- Check the coercion, and apply the substitution to it
-- See Note [Linting type lets]
lintInCo co
  = addLoc (InCo co) $
    do  { co' <- applySubstCo co
        ; _   <- lintCoercion co'
        ; return co' }

-------------------
lintKind :: Kind -> LintM ()
-- Check well-formedness of kinds: *, *->*, etc
lintKind (TyConApp tc []) 
  | getUnique tc `elem` kindKeys
  = return ()
lintKind (FunTy k1 k2)
  = lintKind k1 >> lintKind k2
lintKind kind 
  = addErrL (hang (ptext (sLit "Malformed kind:")) 2 (quotes (ppr kind)))

-------------------
lintTyBndrKind :: OutTyVar -> LintM ()
lintTyBndrKind tv = lintKind (tyVarKind tv)

-------------------
lintCoercion :: OutCoercion -> LintM (OutType, OutType)
-- Check the kind of a coercion term, returning the kind
lintCoercion (Refl ty)
  = do { ty' <- lintInTy ty
       ; return (ty', ty') }

lintCoercion co@(TyConAppCo tc cos)
  = do { (ss,ts) <- mapAndUnzipM lintCoercion cos
       ; check_co_app co (tyConKind tc) ss
       ; return (mkTyConApp tc ss, mkTyConApp tc ts) }

lintCoercion co@(AppCo co1 co2)
  = do { (s1,t1) <- lintCoercion co1
       ; (s2,t2) <- lintCoercion co2
       ; check_co_app co (typeKind s1) [s2]
       ; return (mkAppTy s1 s2, mkAppTy t1 t2) }

lintCoercion (ForAllCo v co)
  = do { lintKind (tyVarKind v)
       ; (s,t) <- addInScopeVar v (lintCoercion co)
       ; return (ForAllTy v s, ForAllTy v t) }

lintCoercion (CoVarCo cv)
  = do { checkTyCoVarInScope cv
       ; return (coVarKind cv) }

lintCoercion (AxiomInstCo (CoAxiom { co_ax_tvs = tvs
                                   , co_ax_lhs = lhs
                                   , co_ax_rhs = rhs }) 
                           cos)
  = do { (tys1, tys2) <- liftM unzip (checkTyCoKinds tvs cos)
       ; return (substTyWith tvs tys1 lhs,
                 substTyWith tvs tys2 rhs) }

lintCoercion (UnsafeCo ty1 ty2)
  = do { ty1' <- lintInTy ty1
       ; ty2' <- lintInTy ty2
       ; return (ty1', ty2') }

lintCoercion (SymCo co) 
  = do { (ty1, ty2) <- lintCoercion co
       ; return (ty2, ty1) }

lintCoercion co@(TransCo co1 co2)
  = do { (ty1a, ty1b) <- lintCoercion co1
       ; (ty2a, ty2b) <- lintCoercion co2
       ; checkL (ty1b `eqType` ty2a)
                (hang (ptext (sLit "Trans coercion mis-match:") <+> ppr co)
                    2 (vcat [ppr ty1a, ppr ty1b, ppr ty2a, ppr ty2b]))
       ; return (ty1a, ty2b) }

lintCoercion the_co@(NthCo d co)
  = do { (s,t) <- lintCoercion co
       ; sn <- checkTcApp the_co d s
       ; tn <- checkTcApp the_co d t
       ; return (sn, tn) }

lintCoercion (InstCo co arg_ty)
  = do { co_tys    <- lintCoercion co
       ; arg_kind  <- lintType arg_ty
       ; case splitForAllTy_maybe `traverse` toPair co_tys of
          Just (Pair (tv1,ty1) (tv2,ty2))
            | arg_kind `isSubKind` tyVarKind tv1
            -> return (substTyWith [tv1] [arg_ty] ty1, 
                       substTyWith [tv2] [arg_ty] ty2) 
            | otherwise
            -> failWithL (ptext (sLit "Kind mis-match in inst coercion"))
	  Nothing -> failWithL (ptext (sLit "Bad argument of inst")) }

----------
checkTcApp :: Coercion -> Int -> Type -> LintM Type
checkTcApp co n ty
  | Just (_, tys) <- splitTyConApp_maybe ty
  , n < length tys
  = return (tys !! n)
  | otherwise
  = failWithL (hang (ptext (sLit "Bad getNth:") <+> ppr co)
                  2 (ptext (sLit "Offending type:") <+> ppr ty))

-------------------
lintType :: OutType -> LintM Kind
lintType (TyVarTy tv)
  = do { checkTyCoVarInScope tv
       ; return (tyVarKind tv) }

lintType ty@(AppTy t1 t2) 
  = do { k1 <- lintType t1
       ; lint_ty_app ty k1 [t2] }

lintType ty@(FunTy t1 t2)
  = lint_ty_app ty (tyConKind funTyCon) [t1,t2]

lintType ty@(TyConApp tc tys)
  | tc `hasKey` eqPredPrimTyConKey	-- See Note [The (~) TyCon] in TysPrim
  = lint_eq_pred ty tys
  | tyConHasKind tc
  = lint_ty_app ty (tyConKind tc) tys
  | otherwise
  = failWithL (hang (ptext (sLit "Malformed type:")) 2 (ppr ty))

lintType (ForAllTy tv ty)
  = do { lintTyBndrKind tv
       ; addInScopeVar tv (lintType ty) }

lintType ty@(PredTy (ClassP cls tys))
  = lint_ty_app ty (tyConKind (classTyCon cls)) tys

lintType (PredTy (IParam _ p_ty))
  = lintType p_ty

lintType ty@(PredTy (EqPred t1 t2))
  = do { k1 <- lintType t1
       ; k2 <- lintType t2
       ; unless (k1 `eqKind` k2) 
                (addErrL (sep [ ptext (sLit "Kind mis-match in equality predicate:")
                              , nest 2 (ppr ty) ]))
       ; return unliftedTypeKind }

----------------
lint_ty_app :: Type -> Kind -> [OutType] -> LintM Kind
lint_ty_app ty k tys 
  = do { ks <- mapM lintType tys
       ; lint_kind_app (ptext (sLit "type") <+> quotes (ppr ty)) k ks }

lint_eq_pred :: Type -> [OutType] -> LintM Kind
lint_eq_pred ty arg_tys
  | [ty1,ty2] <- arg_tys
  = do { k1 <- lintType ty1
       ; k2 <- lintType ty2
       ; checkL (k1 `eqKind` k2) 
                (ptext (sLit "Mismatched arg kinds:") <+> ppr ty)
       ; return unliftedTypeKind }
  | otherwise
  = failWithL (ptext (sLit "Unsaturated (~) type") <+> ppr ty)

----------------
check_co_app :: Coercion -> Kind -> [OutType] -> LintM ()
check_co_app ty k tys 
  = do { _ <- lint_kind_app (ptext (sLit "coercion") <+> quotes (ppr ty))  
                            k (map typeKind tys)
       ; return () }
                      
----------------
lint_kind_app :: SDoc -> Kind -> [Kind] -> LintM Kind
lint_kind_app doc kfn ks = go kfn ks
  where
    fail_msg = vcat [hang (ptext (sLit "Kind application error in")) 2 doc,
               	     nest 2 (ptext (sLit "Function kind =") <+> ppr kfn),
               	     nest 2 (ptext (sLit "Arg kinds =") <+> ppr ks)]

    go kfn []     = return kfn
    go kfn (k:ks) = case splitKindFunTy_maybe kfn of
       	              Nothing         -> failWithL fail_msg
		      Just (kfa, kfb) -> do { unless (k `isSubKind` kfa)
                                                     (addErrL fail_msg)
                                            ; go kfb ks } 
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
	    WarnsAndErrs ->           -- Error and warning messages so far
	    (Maybe a, WarnsAndErrs) } -- Result and messages (if any)

type WarnsAndErrs = (Bag Message, Bag Message)

{-	Note [Type substitution]
	~~~~~~~~~~~~~~~~~~~~~~~~
Why do we need a type substitution?  Consider
	/\(a:*). \(x:a). /\(a:*). id a x
This is ill typed, because (renaming variables) it is really
	/\(a:*). \(x:a). /\(b:*). id b x
Hence, when checking an application, we can't naively compare x's type
(at its binding site) with its expected type (at a use site).  So we
rename type binders as we go, maintaining a substitution.

The same substitution also supports let-type, current expressed as
	(/\(a:*). body) ty
Here we substitute 'ty' for 'a' in 'body', on the fly.
-}

instance Monad LintM where
  return x = LintM (\ _   _     errs -> (Just x, errs))
  fail err = failWithL (text err)
  m >>= k  = LintM (\ loc subst errs -> 
                       let (res, errs') = unLintM m loc subst errs in
                         case res of
                           Just r -> unLintM (k r) loc subst errs'
                           Nothing -> (Nothing, errs'))

data LintLocInfo
  = RhsOf Id		-- The variable bound
  | LambdaBodyOf Id	-- The lambda-binder
  | BodyOfLetRec [Id]	-- One of the binders
  | CaseAlt CoreAlt	-- Case alternative
  | CasePat CoreAlt	-- The *pattern* of the case alternative
  | AnExpr CoreExpr	-- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
  | TopLevelBindings
  | InType Type		-- Inside a type
  | InCo   Coercion     -- Inside a coercion
\end{code}

                 
\begin{code}
initL :: LintM a -> WarnsAndErrs    -- Errors and warnings
initL m
  = case unLintM m [] emptyTvSubst (emptyBag, emptyBag) of
      (_, errs) -> errs
\end{code}

\begin{code}
checkL :: Bool -> Message -> LintM ()
checkL True  _   = return ()
checkL False msg = failWithL msg

failWithL :: Message -> LintM a
failWithL msg = LintM $ \ loc subst (warns,errs) ->
                (Nothing, (warns, addMsg subst errs msg loc))

addErrL :: Message -> LintM ()
addErrL msg = LintM $ \ loc subst (warns,errs) -> 
              (Just (), (warns, addMsg subst errs msg loc))

addWarnL :: Message -> LintM ()
addWarnL msg = LintM $ \ loc subst (warns,errs) -> 
              (Just (), (addMsg subst warns msg loc, errs))

addMsg :: TvSubst ->  Bag Message -> Message -> [LintLocInfo] -> Bag Message
addMsg subst msgs msg locs
  = ASSERT( notNull locs )
    msgs `snocBag` mk_msg msg
  where
   (loc, cxt1) = dumpLoc (head locs)
   cxts        = [snd (dumpLoc loc) | loc <- locs]   
   context     | opt_PprStyle_Debug = vcat (reverse cxts) $$ cxt1 $$
				      ptext (sLit "Substitution:") <+> ppr subst
	       | otherwise	    = cxt1
 
   mk_msg msg = mkLocMessage (mkSrcSpan loc loc) (context $$ msg)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m =
  LintM (\ loc subst errs -> unLintM m (extra_loc:loc) subst errs)

inCasePat :: LintM Bool		-- A slight hack; see the unique call site
inCasePat = LintM $ \ loc _ errs -> (Just (is_case_pat loc), errs)
  where
    is_case_pat (CasePat {} : _) = True
    is_case_pat _other           = False

addInScopeVars :: [Var] -> LintM a -> LintM a
addInScopeVars vars m
  = LintM (\ loc subst errs -> unLintM m loc (extendTvInScopeList subst vars) errs)

addInScopeVar :: Var -> LintM a -> LintM a
addInScopeVar var m
  = LintM (\ loc subst errs -> unLintM m loc (extendTvInScope subst var) errs)

updateTvSubst :: TvSubst -> LintM a -> LintM a
updateTvSubst subst' m = 
  LintM (\ loc _ errs -> unLintM m loc subst' errs)

getTvSubst :: LintM TvSubst
getTvSubst = LintM (\ _ subst errs -> (Just subst, errs))

applySubstTy :: Type -> LintM Type
applySubstTy ty = do { subst <- getTvSubst; return (Type.substTy subst ty) }

applySubstCo :: Coercion -> LintM Coercion
applySubstCo co = do { subst <- getTvSubst; return (substCo (tvCvSubst subst) co) }

extendSubstL :: TyVar -> Type -> LintM a -> LintM a
extendSubstL tv ty m
  = LintM (\ loc subst errs -> unLintM m loc (Type.extendTvSubst subst tv ty) errs)
\end{code}

\begin{code}
lookupIdInScope :: Id -> LintM Id
lookupIdInScope id 
  | not (mustHaveLocalBinding id)
  = return id	-- An imported Id
  | otherwise	
  = do	{ subst <- getTvSubst
	; case lookupInScope (getTvInScope subst) id of
		Just v  -> return v
		Nothing -> do { addErrL out_of_scope
			      ; return id } }
  where
    out_of_scope = ppr id <+> ptext (sLit "is out of scope")


oneTupleDataConId :: Id	-- Should not happen
oneTupleDataConId = dataConWorkId (tupleCon Boxed 1)

checkBndrIdInScope :: Var -> Var -> LintM ()
checkBndrIdInScope binder id 
  = checkInScope msg id
    where
     msg = ptext (sLit "is out of scope inside info for") <+> 
	   ppr binder

checkTyCoVarInScope :: Var -> LintM ()
checkTyCoVarInScope v = checkInScope (ptext (sLit "is out of scope")) v

checkInScope :: SDoc -> Var -> LintM ()
checkInScope loc_msg var =
 do { subst <- getTvSubst
    ; checkL (not (mustHaveLocalBinding var) || (var `isInScope` subst))
             (hsep [ppr var, loc_msg]) }

checkTys :: OutType -> OutType -> Message -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have alrady had the substitution applied
checkTys ty1 ty2 msg = checkL (ty1 `eqType` ty2) msg
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dumpLoc :: LintLocInfo -> (SrcLoc, SDoc)

dumpLoc (RhsOf v)
  = (getSrcLoc v, brackets (ptext (sLit "RHS of") <+> pp_binders [v]))

dumpLoc (LambdaBodyOf b)
  = (getSrcLoc b, brackets (ptext (sLit "in body of lambda with binder") <+> pp_binder b))

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, brackets (ptext (sLit "In body of a letrec with no binders")))

dumpLoc (BodyOfLetRec bs@(_:_))
  = ( getSrcLoc (head bs), brackets (ptext (sLit "in body of letrec with binders") <+> pp_binders bs))

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, _))
  = (noSrcLoc, text "In a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (CasePat (con, args, _))
  = (noSrcLoc, text "In the pattern of a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (ImportedUnfolding locn)
  = (locn, brackets (ptext (sLit "in an imported unfolding")))
dumpLoc TopLevelBindings
  = (noSrcLoc, empty)
dumpLoc (InType ty)
  = (noSrcLoc, text "In the type" <+> quotes (ppr ty))
dumpLoc (InCo co)
  = (noSrcLoc, text "In the coercion" <+> quotes (ppr co))

pp_binders :: [Var] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Var -> SDoc
pp_binder b | isId b    = hsep [ppr b, dcolon, ppr (idType b)]
            | otherwise = hsep [ppr b, dcolon, ppr (tyVarKind b)]
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

mkScrutMsg :: Id -> Type -> Type -> TvSubst -> Message
mkScrutMsg var var_ty scrut_ty subst
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
	  text "Result binder type:" <+> ppr var_ty,--(idType var),
	  text "Scrutinee type:" <+> ppr scrut_ty,
     hsep [ptext (sLit "Current TV subst"), ppr subst]]

mkNonDefltMsg, mkNonIncreasingAltsMsg :: CoreExpr -> Message
mkNonDefltMsg e
  = hang (text "Case expression with DEFAULT not at the beginnning") 4 (ppr e)
mkNonIncreasingAltsMsg e
  = hang (text "Case expression with badly-ordered alternatives") 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> Message
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives") 4 (ppr e)

mkBadConMsg :: TyCon -> DataCon -> Message
mkBadConMsg tycon datacon
  = vcat [
	text "In a case alternative, data constructor isn't in scrutinee type:",
	text "Scrutinee type constructor:" <+> ppr tycon,
	text "Data con:" <+> ppr datacon
    ]

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

mkNewTyDataConAltMsg :: Type -> CoreAlt -> Message
mkNewTyDataConAltMsg scrut_ty alt
  = vcat [ text "Data alternative for newtype datacon",
	   text "Scrutinee type:" <+> ppr scrut_ty,
	   text "Alternative:" <+> pprCoreAlt alt ]


------------------------------------------------------
--	Other error messages

mkAppMsg :: Type -> Type -> CoreExpr -> Message
mkAppMsg fun_ty arg_ty arg
  = vcat [ptext (sLit "Argument value doesn't match argument type:"),
	      hang (ptext (sLit "Fun type:")) 4 (ppr fun_ty),
	      hang (ptext (sLit "Arg type:")) 4 (ppr arg_ty),
	      hang (ptext (sLit "Arg:")) 4 (ppr arg)]

mkNonFunAppMsg :: Type -> Type -> CoreExpr -> Message
mkNonFunAppMsg fun_ty arg_ty arg
  = vcat [ptext (sLit "Non-function type in function position"),
	      hang (ptext (sLit "Fun type:")) 4 (ppr fun_ty),
	      hang (ptext (sLit "Arg type:")) 4 (ppr arg_ty),
	      hang (ptext (sLit "Arg:")) 4 (ppr arg)]

mkLetErr :: TyVar -> CoreExpr -> Message
mkLetErr bndr rhs
  = vcat [ptext (sLit "Bad `let' binding:"),
	  hang (ptext (sLit "Variable:"))
		 4 (ppr bndr <+> dcolon <+> ppr (varType bndr)),
	  hang (ptext (sLit "Rhs:"))   
	         4 (ppr rhs)]

mkTyCoAppErrMsg :: TyVar -> Coercion -> Message
mkTyCoAppErrMsg tyvar arg_co
  = vcat [ptext (sLit "Kinds don't match in lifted coercion application:"),
          hang (ptext (sLit "Type variable:"))
		 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
	  hang (ptext (sLit "Arg coercion:"))   
	         4 (ppr arg_co <+> dcolon <+> pprEqPred (coercionKind arg_co))]

mkTyAppMsg :: Type -> Type -> Message
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
	      hang (ptext (sLit "Exp type:"))
		 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
	      hang (ptext (sLit "Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkRhsMsg :: Id -> Type -> Message
mkRhsMsg binder ty
  = vcat
    [hsep [ptext (sLit "The type of this binder doesn't match the type of its RHS:"),
	    ppr binder],
     hsep [ptext (sLit "Binder's type:"), ppr (idType binder)],
     hsep [ptext (sLit "Rhs type:"), ppr ty]]

mkRhsPrimMsg :: Id -> CoreExpr -> Message
mkRhsPrimMsg binder _rhs
  = vcat [hsep [ptext (sLit "The type of this binder is primitive:"),
		     ppr binder],
	      hsep [ptext (sLit "Binder's type:"), ppr (idType binder)]
	     ]

mkStrictMsg :: Id -> Message
mkStrictMsg binder
  = vcat [hsep [ptext (sLit "Recursive or top-level binder has strict demand info:"),
		     ppr binder],
	      hsep [ptext (sLit "Binder's demand info:"), ppr (idDemandInfo binder)]
	     ]


mkKindErrMsg :: TyVar -> Type -> Message
mkKindErrMsg tyvar arg_ty
  = vcat [ptext (sLit "Kinds don't match in type application:"),
	  hang (ptext (sLit "Type variable:"))
		 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
	  hang (ptext (sLit "Arg type:"))   
	         4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkArityMsg :: Id -> Message
mkArityMsg binder
  = vcat [hsep [ptext (sLit "Demand type has "),
                     ppr (dmdTypeDepth dmd_ty),
                     ptext (sLit " arguments, rhs has "),
                     ppr (idArity binder),
                     ptext (sLit "arguments, "),
		     ppr binder],
	      hsep [ptext (sLit "Binder's strictness signature:"), ppr dmd_ty]

         ]
           where (StrictSig dmd_ty) = idStrictness binder

mkUnboxedTupleMsg :: Id -> Message
mkUnboxedTupleMsg binder
  = vcat [hsep [ptext (sLit "A variable has unboxed tuple type:"), ppr binder],
	  hsep [ptext (sLit "Binder's type:"), ppr (idType binder)]]

mkCastErr :: Type -> Type -> Message
mkCastErr from_ty expr_ty
  = vcat [ptext (sLit "From-type of Cast differs from type of enclosed expression"),
	  ptext (sLit "From-type:") <+> ppr from_ty,
	  ptext (sLit "Type of enclosed expr:") <+> ppr expr_ty
    ]

dupVars :: [[Var]] -> Message
dupVars vars
  = hang (ptext (sLit "Duplicate variables brought into scope"))
       2 (ppr vars)

dupExtVars :: [[Name]] -> Message
dupExtVars vars
  = hang (ptext (sLit "Duplicate top-level variables with the same qualified name"))
       2 (ppr vars)
\end{code}

-------------- DEAD CODE  -------------------

-------------------
checkCoKind :: CoVar -> OutCoercion -> LintM ()
-- Both args have had substitution applied
checkCoKind covar arg_co
  = do { (s2,t2) <- lintCoercion arg_co
       ; unless (s1 `eqType` s2 && t1 `coreEqType` t2)
                (addErrL (mkCoAppErrMsg covar arg_co)) }
  where
    (s1,t1) = coVarKind covar

lintCoVarKind :: OutCoVar -> LintM ()
-- Check the kind of a coercion binder
lintCoVarKind tv
  = do { (ty1,ty2) <- lintSplitCoVar tv
       ; lintEqType ty1 ty2


-------------------
lintSplitCoVar :: CoVar -> LintM (Type,Type)
lintSplitCoVar cv
  = case coVarKind_maybe cv of
      Just ts -> return ts
      Nothing -> failWithL (sep [ ptext (sLit "Coercion variable with non-equality kind:")
                                , nest 2 (ppr cv <+> dcolon <+> ppr (tyVarKind cv))])

mkCoVarLetErr :: CoVar -> Coercion -> Message
mkCoVarLetErr covar co
  = vcat [ptext (sLit "Bad `let' binding for coercion variable:"),
	  hang (ptext (sLit "Coercion variable:"))
		 4 (ppr covar <+> dcolon <+> ppr (coVarKind covar)),
	  hang (ptext (sLit "Arg coercion:"))   
	         4 (ppr co)]

mkCoAppErrMsg :: CoVar -> Coercion -> Message
mkCoAppErrMsg covar arg_co
  = vcat [ptext (sLit "Kinds don't match in coercion application:"),
	  hang (ptext (sLit "Coercion variable:"))
		 4 (ppr covar <+> dcolon <+> ppr (coVarKind covar)),
	  hang (ptext (sLit "Arg coercion:"))   
	         4 (ppr arg_co <+> dcolon <+> pprEqPred (coercionKind arg_co))]


mkCoAppMsg :: Type -> Coercion -> Message
mkCoAppMsg ty arg_co
  = vcat [text "Illegal type application:",
	      hang (ptext (sLit "exp type:"))
		 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
	      hang (ptext (sLit "arg type:"))   
	         4 (ppr arg_co <+> dcolon <+> ppr (coercionKind arg_co))]

