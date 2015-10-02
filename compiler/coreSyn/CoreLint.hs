{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


A ``lint'' pass to check for Core correctness
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fprof-auto #-}

module CoreLint (
    lintCoreBindings, lintUnfolding,
    lintPassResult, lintInteractiveExpr, lintExpr,
    lintAnnots,

    -- ** Debug output
    CoreLint.showPass, showPassIO, endPass, endPassIO,
    dumpPassResult,
    CoreLint.dumpIfSet,
 ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils
import CoreMonad
import Bag
import Literal
import DataCon
import TysWiredIn
import TysPrim
import TcType ( isFloatingTy )
import Var
import VarEnv
import VarSet
import Name
import Id
import PprCore
import ErrUtils
import Coercion
import SrcLoc
import Kind
import Type
import TypeRep
import TyCon
import CoAxiom
import BasicTypes
import ErrUtils as Err
import StaticFlags
import ListSetOps
import PrelNames
import Outputable
import FastString
import Util
import InstEnv     ( instanceDFunId )
import OptCoercion ( checkAxInstCo )
import UniqSupply

import HscTypes
import DynFlags
import Control.Monad
import MonadUtils
import Data.Maybe
import Pair

{-
Note [GHC Formalism]
~~~~~~~~~~~~~~~~~~~~
This file implements the type-checking algorithm for System FC, the "official"
name of the Core language. Type safety of FC is heart of the claim that
executables produced by GHC do not have segmentation faults. Thus, it is
useful to be able to reason about System FC independently of reading the code.
To this purpose, there is a document ghc.pdf built in docs/core-spec that
contains a formalism of the types and functions dealt with here. If you change
just about anything in this file or you change other types/functions throughout
the Core language (all signposted to this note), you should update that
formalism. See docs/core-spec/README for more info about how to do so.

Summary of checks
~~~~~~~~~~~~~~~~~
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
find an occurrence of an Id, we fetch it from the in-scope set.

************************************************************************
*                                                                      *
                 Beginning and ending passes
*                                                                      *
************************************************************************

These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a conveneint place.  place for them.  They print out
stuff before and after core passes, and do Core Lint when necessary.
-}

showPass :: CoreToDo -> CoreM ()
showPass pass = do { dflags <- getDynFlags
                   ; liftIO $ showPassIO dflags pass }

showPassIO :: DynFlags -> CoreToDo -> IO ()
showPassIO dflags pass = Err.showPass dflags (showPpr dflags pass)

endPass :: CoreToDo -> CoreProgram -> [CoreRule] -> CoreM ()
endPass pass binds rules
  = do { hsc_env <- getHscEnv
       ; print_unqual <- getPrintUnqualified
       ; liftIO $ endPassIO hsc_env print_unqual pass binds rules }

endPassIO :: HscEnv -> PrintUnqualified
          -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
-- Used by the IO-is CorePrep too
endPassIO hsc_env print_unqual pass binds rules
  = do { dumpPassResult dflags print_unqual mb_flag
                        (ppr pass) (pprPassDetails pass) binds rules
       ; lintPassResult hsc_env pass binds }
  where
    dflags  = hsc_dflags hsc_env
    mb_flag = case coreDumpFlag pass of
                Just flag | dopt flag dflags                    -> Just flag
                          | dopt Opt_D_verbose_core2core dflags -> Just flag
                _ -> Nothing

dumpIfSet :: DynFlags -> Bool -> CoreToDo -> SDoc -> SDoc -> IO ()
dumpIfSet dflags dump_me pass extra_info doc
  = Err.dumpIfSet dflags dump_me (showSDoc dflags (ppr pass <+> extra_info)) doc

dumpPassResult :: DynFlags
               -> PrintUnqualified
               -> Maybe DumpFlag        -- Just df => show details in a file whose
                                        --            name is specified by df
               -> SDoc                  -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult dflags unqual mb_flag hdr extra_info binds rules
  | Just flag <- mb_flag
  = Err.dumpSDoc dflags unqual flag (showSDoc dflags hdr) dump_doc

  | otherwise
  = Err.debugTraceMsg dflags 2 size_doc
          -- Report result size
          -- This has the side effect of forcing the intermediate to be evaluated

  where
    size_doc = sep [text "Result size of" <+> hdr, nest 2 (equals <+> ppr (coreBindsStats binds))]

    dump_doc  = vcat [ nest 2 extra_info
                     , size_doc
                     , blankLine
                     , pprCoreBindings binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , ptext (sLit "------ Local rules for imported ids --------")
                    , pprRules rules ]

coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag CoreLiberateCase         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoStrictness         = Just Opt_D_dump_stranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse
coreDumpFlag CoreDoVectorisation      = Just Opt_D_dump_vect
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep

coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing

{-
************************************************************************
*                                                                      *
                 Top-level interfaces
*                                                                      *
************************************************************************
-}

lintPassResult :: HscEnv -> CoreToDo -> CoreProgram -> IO ()
lintPassResult hsc_env pass binds
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | otherwise
  = do { let (warns, errs) = lintCoreBindings pass (interactiveInScope hsc_env) binds
       ; Err.showPass dflags ("Core Linted result of " ++ showPpr dflags pass)
       ; displayLintResults dflags pass warns errs binds  }
  where
    dflags = hsc_dflags hsc_env

displayLintResults :: DynFlags -> CoreToDo
                   -> Bag Err.MsgDoc -> Bag Err.MsgDoc -> CoreProgram
                   -> IO ()
displayLintResults dflags pass warns errs binds
  | not (isEmptyBag errs)
  = do { log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
           (vcat [ lint_banner "errors" (ppr pass), Err.pprMessageBag errs
                 , ptext (sLit "*** Offending Program ***")
                 , pprCoreBindings binds
                 , ptext (sLit "*** End of Offense ***") ])
       ; Err.ghcExit dflags 1 }

  | not (isEmptyBag warns)
  , not opt_NoDebugOutput
  , showLintWarnings pass
  = log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
        (lint_banner "warnings" (ppr pass) $$ Err.pprMessageBag warns)

  | otherwise = return ()
  where

lint_banner :: String -> SDoc -> SDoc
lint_banner string pass = ptext (sLit "*** Core Lint")      <+> text string
                          <+> ptext (sLit ": in result of") <+> pass
                          <+> ptext (sLit "***")

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings (CoreDoSimplify _ (SimplMode { sm_phase = InitialPhase })) = False
showLintWarnings _ = True

lintInteractiveExpr :: String -> HscEnv -> CoreExpr -> IO ()
lintInteractiveExpr what hsc_env expr
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | Just err <- lintExpr (interactiveInScope hsc_env) expr
  = do { display_lint_err err
       ; Err.ghcExit dflags 1 }
  | otherwise
  = return ()
  where
    dflags = hsc_dflags hsc_env

    display_lint_err err
      = do { log_action dflags dflags Err.SevDump noSrcSpan defaultDumpStyle
               (vcat [ lint_banner "errors" (text what)
                     , err
                     , ptext (sLit "*** Offending Program ***")
                     , pprCoreExpr expr
                     , ptext (sLit "*** End of Offense ***") ])
           ; Err.ghcExit dflags 1 }

interactiveInScope :: HscEnv -> [Var]
-- In GHCi we may lint expressions, or bindings arising from 'deriving'
-- clauses, that mention variables bound in the interactive context.
-- These are Local things (see Note [Interactively-bound Ids in GHCi] in HscTypes).
-- So we have to tell Lint about them, lest it reports them as out of scope.
--
-- We do this by find local-named things that may appear free in interactive
-- context.  This function is pretty revolting and quite possibly not quite right.
-- When we are not in GHCi, the interactive context (hsc_IC hsc_env) is empty
-- so this is a (cheap) no-op.
--
-- See Trac #8215 for an example
interactiveInScope hsc_env
  = varSetElems tyvars ++ ids
  where
    -- C.f. TcRnDriver.setInteractiveContext, Desugar.deSugarExpr
    ictxt                   = hsc_IC hsc_env
    (cls_insts, _fam_insts) = ic_instances ictxt
    te1    = mkTypeEnvWithImplicits (ic_tythings ictxt)
    te     = extendTypeEnvWithIds te1 (map instanceDFunId cls_insts)
    ids    = typeEnvIds te
    tyvars = mapUnionVarSet (tyVarsOfType . idType) ids
              -- Why the type variables?  How can the top level envt have free tyvars?
              -- I think it's because of the GHCi debugger, which can bind variables
              --   f :: [t] -> [t]
              -- where t is a RuntimeUnk (see TcType)

lintCoreBindings :: CoreToDo -> [Var] -> CoreProgram -> (Bag MsgDoc, Bag MsgDoc)
--   Returns (warnings, errors)
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreBindings pass local_in_scope binds
  = initL flags $
    addLoc TopLevelBindings        $
    addInScopeVars local_in_scope  $
    addInScopeVars binders         $
        -- Put all the top-level binders in scope at the start
        -- This is because transformation rules can bring something
        -- into use 'unexpectedly'
    do { checkL (null dups) (dupVars dups)
       ; checkL (null ext_dups) (dupExtVars ext_dups)
       ; mapM lint_bind binds }
  where
    flags = LF { lf_check_global_ids = check_globals
               , lf_check_inline_loop_breakers = check_lbs }

    -- See Note [Checking for global Ids]
    check_globals = case pass of
                      CoreTidy -> False
                      CorePrep -> False
                      _        -> True

    -- See Note [Checking for INLINE loop breakers]
    check_lbs = case pass of
                      CoreDesugar    -> False
                      CoreDesugarOpt -> False
                      _              -> True

    binders = bindersOfBinds binds
    (_, dups) = removeDups compare binders

    -- dups_ext checks for names with different uniques
    -- but but the same External name M.n.  We don't
    -- allow this at top level:
    --    M.n{r3}  = ...
    --    M.n{r29} = ...
    -- because they both get the same linker symbol
    ext_dups = snd (removeDups ord_ext (map Var.varName binders))
    ord_ext n1 n2 | Just m1 <- nameModule_maybe n1
                  , Just m2 <- nameModule_maybe n2
                  = compare (m1, nameOccName n1) (m2, nameOccName n2)
                  | otherwise = LT

    -- If you edit this function, you may need to update the GHC formalism
    -- See Note [GHC Formalism]
    lint_bind (Rec prs)         = mapM_ (lintSingleBinding TopLevel Recursive) prs
    lint_bind (NonRec bndr rhs) = lintSingleBinding TopLevel NonRecursive (bndr,rhs)

{-
************************************************************************
*                                                                      *
\subsection[lintUnfolding]{lintUnfolding}
*                                                                      *
************************************************************************

We use this to check all unfoldings that come in from interfaces
(it is very painful to catch errors otherwise):
-}

lintUnfolding :: SrcLoc
              -> [Var]          -- Treat these as in scope
              -> CoreExpr
              -> Maybe MsgDoc   -- Nothing => OK

lintUnfolding locn vars expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL defaultLintFlags linter
    linter = addLoc (ImportedUnfolding locn) $
             addInScopeVars vars             $
             lintCoreExpr expr

lintExpr :: [Var]               -- Treat these as in scope
         -> CoreExpr
         -> Maybe MsgDoc        -- Nothing => OK

lintExpr vars expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL defaultLintFlags linter
    linter = addLoc TopLevelBindings $
             addInScopeVars vars     $
             lintCoreExpr expr

{-
************************************************************************
*                                                                      *
\subsection[lintCoreBinding]{lintCoreBinding}
*                                                                      *
************************************************************************

Check a core binding, returning the list of variables bound.
-}

lintSingleBinding :: TopLevelFlag -> RecFlag -> (Id, CoreExpr) -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintSingleBinding top_lvl_flag rec_flag (binder,rhs)
  = addLoc (RhsOf binder) $
         -- Check the rhs
    do { ty <- lintCoreExpr rhs
       ; lintBinder binder -- Check match to RHS type
       ; binder_ty <- applySubstTy binder_ty
       ; checkTys binder_ty ty (mkRhsMsg binder (ptext (sLit "RHS")) ty)

        -- Check the let/app invariant
        -- See Note [CoreSyn let/app invariant] in CoreSyn
       ; checkL (not (isUnLiftedType binder_ty)
            || (isNonRec rec_flag && exprOkForSpeculation rhs))
           (mkRhsPrimMsg binder rhs)

        -- Check that if the binder is top-level or recursive, it's not demanded
       ; checkL (not (isStrictId binder)
            || (isNonRec rec_flag && not (isTopLevel top_lvl_flag)))
           (mkStrictMsg binder)

        -- Check that if the binder is local, it is not marked as exported
       ; checkL (not (isExportedId binder) || isTopLevel top_lvl_flag)
           (mkNonTopExportedMsg binder)

        -- Check that if the binder is local, it does not have an external name
       ; checkL (not (isExternalName (Var.varName binder)) || isTopLevel top_lvl_flag)
           (mkNonTopExternalNameMsg binder)

        -- Check whether binder's specialisations contain any out-of-scope variables
       ; mapM_ (checkBndrIdInScope binder) bndr_vars

       ; flags <- getLintFlags
       ; when (lf_check_inline_loop_breakers flags
               && isStrongLoopBreaker (idOccInfo binder)
               && isInlinePragma (idInlinePragma binder))
              (addWarnL (ptext (sLit "INLINE binder is (non-rule) loop breaker:") <+> ppr binder))
              -- Only non-rule loop breakers inhibit inlining

      -- Check whether arity and demand type are consistent (only if demand analysis
      -- already happened)
      --
      -- Note (Apr 2014): this is actually ok.  See Note [Demand analysis for trivial right-hand sides]
      --                  in DmdAnal.  After eta-expansion in CorePrep the rhs is no longer trivial.
      --       ; let dmdTy = idStrictness binder
      --       ; checkL (case dmdTy of
      --                  StrictSig dmd_ty -> idArity binder >= dmdTypeDepth dmd_ty || exprIsTrivial rhs)
      --           (mkArityMsg binder)

       ; lintIdUnfolding binder binder_ty (idUnfolding binder) }

        -- We should check the unfolding, if any, but this is tricky because
        -- the unfolding is a SimplifiableCoreExpr. Give up for now.
   where
    binder_ty                  = idType binder
    bndr_vars                  = varSetElems (idFreeVars binder)

    -- If you edit this function, you may need to update the GHC formalism
    -- See Note [GHC Formalism]
    lintBinder var | isId var  = lintIdBndr var $ \_ -> (return ())
                   | otherwise = return ()

lintIdUnfolding :: Id -> Type -> Unfolding -> LintM ()
lintIdUnfolding bndr bndr_ty (CoreUnfolding { uf_tmpl = rhs, uf_src = src })
  | isStableSource src
  = do { ty <- lintCoreExpr rhs
       ; checkTys bndr_ty ty (mkRhsMsg bndr (ptext (sLit "unfolding")) ty) }
lintIdUnfolding  _ _ _
  = return ()       -- We could check more

{-
Note [Checking for INLINE loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very suspicious if a strong loop breaker is marked INLINE.

However, the desugarer generates instance methods with INLINE pragmas
that form a mutually recursive group.  Only after a round of
simplification are they unravelled.  So we suppress the test for
the desugarer.

************************************************************************
*                                                                      *
\subsection[lintCoreExpr]{lintCoreExpr}
*                                                                      *
************************************************************************
-}

--type InKind      = Kind       -- Substitution not yet applied
type InType      = Type
type InCoercion  = Coercion
type InVar       = Var
type InTyVar     = TyVar

type OutKind     = Kind -- Substitution has been applied to this,
                        -- but has not been linted yet
type LintedKind  = Kind -- Substitution applied, and type is linted

type OutType     = Type -- Substitution has been applied to this,
                        -- but has not been linted yet

type LintedType  = Type -- Substitution applied, and type is linted

type OutCoercion = Coercion
type OutVar      = Var
type OutTyVar    = TyVar

lintCoreExpr :: CoreExpr -> LintM OutType
-- The returned type has the substitution from the monad
-- already applied to it:
--      lintCoreExpr e subst = exprType (subst e)
--
-- The returned "type" can be a kind, if the expression is (Type ty)

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreExpr (Var var)
  = do  { checkL (not (var == oneTupleDataConId))
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
       ; (_, from_ty, to_ty, r) <- lintCoercion co'
       ; checkRole co' Representational r
       ; checkTys from_ty expr_ty (mkCastErr expr co' from_ty expr_ty)
       ; return to_ty }

lintCoreExpr (Tick (Breakpoint _ ids) expr)
  = do forM_ ids $ \id -> do
         checkDeadIdOcc id
         lookupIdInScope id
       lintCoreExpr expr

lintCoreExpr (Tick _other_tickish expr)
  = lintCoreExpr expr

lintCoreExpr (Let (NonRec tv (Type ty)) body)
  | isTyVar tv
  =     -- See Note [Linting type lets]
    do  { ty' <- applySubstTy ty
        ; lintTyBndr tv              $ \ tv' ->
    do  { addLoc (RhsOf tv) $ checkTyKind tv' ty'
                -- Now extend the substitution so we
                -- take advantage of it in the body
        ; extendSubstL tv' ty'       $
          addLoc (BodyOfLetRec [tv]) $
          lintCoreExpr body } }

lintCoreExpr (Let (NonRec bndr rhs) body)
  | isId bndr
  = do  { lintSingleBinding NotTopLevel NonRecursive (bndr,rhs)
        ; addLoc (BodyOfLetRec [bndr])
                 (lintAndScopeId bndr $ \_ -> (lintCoreExpr body)) }

  | otherwise
  = failWithL (mkLetErr bndr rhs)       -- Not quite accurate

lintCoreExpr (Let (Rec pairs) body)
  = lintAndScopeIds bndrs       $ \_ ->
    do  { checkL (null dups) (dupVars dups)
        ; mapM_ (lintSingleBinding NotTopLevel Recursive) pairs
        ; addLoc (BodyOfLetRec bndrs) (lintCoreExpr body) }
  where
    bndrs = map fst pairs
    (_, dups) = removeDups compare bndrs

lintCoreExpr e@(App _ _)
    = do { fun_ty <- lintCoreExpr fun
         ; addLoc (AnExpr e) $ foldM lintCoreArg fun_ty args }
  where
    (fun, args) = collectArgs e

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var) $
    lintBinder var $ \ var' ->
    do { body_ty <- lintCoreExpr expr
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

     -- See Note [Rules for floating-point comparisons] in PrelRules
     ; let isLitPat (LitAlt _, _ , _) = True
           isLitPat _                 = False
     ; checkL (not $ isFloatingTy scrut_ty && any isLitPat alts)
         (ptext (sLit $ "Lint warning: Scrutinising floating-point " ++
                        "expression with literal pattern in case " ++
                        "analysis (see Trac #9238).")
          $$ text "scrut" <+> ppr scrut)

     ; case tyConAppTyCon_maybe (idType var) of
         Just tycon
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

     ; lintAndScopeId var $ \_ ->
       do { -- Check the alternatives
            mapM_ (lintCoreAlt scrut_ty alt_ty) alts
          ; checkCaseAlts e scrut_ty alts
          ; return alt_ty } }

-- This case can't happen; linting types in expressions gets routed through
-- lintCoreArgs
lintCoreExpr (Type ty)
  = pprPanic "lintCoreExpr" (ppr ty)

lintCoreExpr (Coercion co)
  = do { (_kind, ty1, ty2, role) <- lintInCo co
       ; return (mkCoercionType role ty1 ty2) }

{-
Note [Kind instantiation in coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following coercion axiom:
  ax_co [(k_ag :: BOX), (f_aa :: k_ag -> Constraint)] :: T k_ag f_aa ~ f_aa

Consider the following instantiation:
  ax_co <* -> *> <Monad>

We need to split the co_ax_tvs into kind and type variables in order
to find out the coercion kind instantiations. Those can only be Refl
since we don't have kind coercions. This is just a way to represent
kind instantiation.

We use the number of kind variables to know how to split the coercions
instantiations between kind coercions and type coercions. We lint the
kind coercions and produce the following substitution which is to be
applied in the type variables:
  k_ag   ~~>   * -> *

Note [No alternatives lint check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions with no alternatives are odd beasts, and worth looking at
in the linter (cf Trac #10180).  We check two things:

* exprIsHNF is false: certainly, it would be terribly wrong if the
  scrutinee was already in head normal form.

* exprIsBottom is true: we should be able to see why GHC believes the
  scrutinee is diverging for sure.

In principle, the first check is redundant: exprIsBottom == True will
always imply exprIsHNF == False.  But the first check is reliable: If
exprIsHNF == True, then there definitely is a problem (exprIsHNF errs
on the right side).  If the second check triggers then it may be the
case that the compiler got smarter elsewhere, and the empty case is
correct, but that exprIsBottom is unable to see it. In particular, the
empty-type check in exprIsBottom is an approximation. Therefore, this
check is not fully reliable, and we keep both around.

************************************************************************
*                                                                      *
\subsection[lintCoreArgs]{lintCoreArgs}
*                                                                      *
************************************************************************

The basic version of these functions checks that the argument is a
subtype of the required type, as one would expect.
-}

lintCoreArg  :: OutType -> CoreArg -> LintM OutType
lintCoreArg fun_ty (Type arg_ty)
  = do { arg_ty' <- applySubstTy arg_ty
       ; lintTyApp fun_ty arg_ty' }

lintCoreArg fun_ty arg
  = do { arg_ty <- lintCoreExpr arg
       ; checkL (not (isUnLiftedType arg_ty) || exprOkForSpeculation arg)
                (mkLetAppMsg arg)
       ; lintValApp arg fun_ty arg_ty }

-----------------
lintAltBinders :: OutType     -- Scrutinee type
               -> OutType     -- Constructor type
               -> [OutVar]    -- Binders
               -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
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
  = do  { checkTyKind tyvar arg_ty
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

checkTyKind :: OutTyVar -> OutType -> LintM ()
-- Both args have had substitution applied

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
checkTyKind tyvar arg_ty
  | isSuperKind tyvar_kind  -- kind forall
  = lintKind arg_ty
        -- Arg type might be boxed for a function with an uncommitted
        -- tyvar; notably this is used so that we can give
        --      error :: forall a:*. String -> a
        -- and then apply it to both boxed and unboxed types.
  | otherwise  -- type forall
  = do { arg_kind <- lintType arg_ty
       ; unless (arg_kind `isSubKind` tyvar_kind)
                (addErrL (mkKindErrMsg tyvar arg_ty $$ (text "xx" <+> ppr arg_kind))) }
  where
    tyvar_kind = tyVarKind tyvar

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

{-
************************************************************************
*                                                                      *
\subsection[lintCoreAlts]{lintCoreAlts}
*                                                                      *
************************************************************************
-}

checkCaseAlts :: CoreExpr -> OutType -> [CoreAlt] -> LintM ()
-- a) Check that the alts are non-empty
-- b1) Check that the DEFAULT comes first, if it exists
-- b2) Check that the others are in increasing order
-- c) Check that there's a default for infinite types
-- NB: Algebraic cases are not necessarily exhaustive, because
--     the simplifer correctly eliminates case that can't
--     possibly match.

checkCaseAlts e ty alts =
  do { checkL (all non_deflt con_alts) (mkNonDefltMsg e)
     ; checkL (increasing_tag con_alts) (mkNonIncreasingAltsMsg e)

          -- For types Int#, Word# with an infinite (well, large!) number of
          -- possible values, there should usually be a DEFAULT case
          -- But (see Note [Empty case alternatives] in CoreSyn) it's ok to
          -- have *no* case alternatives.
          -- In effect, this is a kind of partial test. I suppose it's possible
          -- that we might *know* that 'x' was 1 or 2, in which case
          --   case x of { 1 -> e1; 2 -> e2 }
          -- would be fine.
     ; checkL (isJust maybe_deflt || not is_infinite_ty || null alts)
              (nonExhaustiveAltsMsg e) }
  where
    (con_alts, maybe_deflt) = findDefault alts

        -- Check that successive alternatives have increasing tags
    increasing_tag (alt1 : rest@( alt2 : _)) = alt1 `ltAlt` alt2 && increasing_tag rest
    increasing_tag _                         = True

    non_deflt (DEFAULT, _, _) = False
    non_deflt _               = True

    is_infinite_ty = case tyConAppTyCon_maybe ty of
                        Nothing    -> False
                        Just tycon -> isPrimTyCon tycon

checkAltExpr :: CoreExpr -> OutType -> LintM ()
checkAltExpr expr ann_ty
  = do { actual_ty <- lintCoreExpr expr
       ; checkTys actual_ty ann_ty (mkCaseAltMsg expr actual_ty ann_ty) }

lintCoreAlt :: OutType          -- Type of scrutinee
            -> OutType          -- Type of the alternative
            -> CoreAlt
            -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreAlt _ alt_ty (DEFAULT, args, rhs) =
  do { checkL (null args) (mkDefaultArgsMsg args)
     ; checkAltExpr rhs alt_ty }

lintCoreAlt scrut_ty alt_ty (LitAlt lit, args, rhs)
  | litIsLifted lit
  = failWithL integerScrutinisedMsg
  | otherwise
  = do { checkL (null args) (mkDefaultArgsMsg args)
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

  | otherwise   -- Scrut-ty is wrong shape
  = addErrL (mkBadAltMsg scrut_ty alt)

{-
************************************************************************
*                                                                      *
\subsection[lint-types]{Types}
*                                                                      *
************************************************************************
-}

-- When we lint binders, we (one at a time and in order):
--  1. Lint var types or kinds (possibly substituting)
--  2. Add the binder to the in scope set, and if its a coercion var,
--     we may extend the substitution to reflect its (possibly) new kind
lintBinders :: [Var] -> ([Var] -> LintM a) -> LintM a
lintBinders [] linterF = linterF []
lintBinders (var:vars) linterF = lintBinder var $ \var' ->
                                 lintBinders vars $ \ vars' ->
                                 linterF (var':vars')

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
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
  = do  { lintAndScopeId id $ \id' -> linterF id' }

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
  = do { flags <- getLintFlags
       ; checkL (not (lf_check_global_ids flags) || isLocalId id)
                (ptext (sLit "Non-local Id binder") <+> ppr id)
                -- See Note [Checking for global Ids]
       ; ty <- lintInTy (idType id)
       ; let id' = setIdType id ty
       ; addInScopeVar id' $ (linterF id') }

{-
************************************************************************
*                                                                      *
             Types and kinds
*                                                                      *
************************************************************************

We have a single linter for types and kinds.  That is convenient
because sometimes it's not clear whether the thing we are looking
at is a type or a kind.
-}

lintInTy :: InType -> LintM LintedType
-- Types only, not kinds
-- Check the type, and apply the substitution to it
-- See Note [Linting type lets]
lintInTy ty
  = addLoc (InType ty) $
    do  { ty' <- applySubstTy ty
        ; _k  <- lintType ty'
        ; return ty' }

-------------------
lintTyBndrKind :: OutTyVar -> LintM ()
-- Handles both type and kind foralls.
lintTyBndrKind tv = lintKind (tyVarKind tv)

-------------------
lintType :: OutType -> LintM LintedKind
-- The returned Kind has itself been linted

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintType (TyVarTy tv)
  = do { checkTyCoVarInScope tv
       ; return (tyVarKind tv) }
         -- We checked its kind when we added it to the envt

lintType ty@(AppTy t1 t2)
  = do { k1 <- lintType t1
       ; k2 <- lintType t2
       ; lint_ty_app ty k1 [(t2,k2)] }

lintType ty@(FunTy t1 t2)    -- (->) has two different rules, for types and kinds
  = do { k1 <- lintType t1
       ; k2 <- lintType t2
       ; lintArrow (ptext (sLit "type or kind") <+> quotes (ppr ty)) k1 k2 }

lintType ty@(TyConApp tc tys)
  | Just ty' <- coreView ty
  = lintType ty'   -- Expand type synonyms, so that we do not bogusly complain
                   --  about un-saturated type synonyms

  | isUnLiftedTyCon tc || isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
       -- See Note [The kind invariant] in TypeRep
       -- Also type synonyms and type families
  , length tys < tyConArity tc
  = failWithL (hang (ptext (sLit "Un-saturated type application")) 2 (ppr ty))

  | otherwise
  = do { ks <- mapM lintType tys
       ; lint_ty_app ty (tyConKind tc) (tys `zip` ks) }

lintType (ForAllTy tv ty)
  = do { lintTyBndrKind tv
       ; addInScopeVar tv (lintType ty) }

lintType ty@(LitTy l) = lintTyLit l >> return (typeKind ty)

lintKind :: OutKind -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintKind k = do { sk <- lintType k
                ; unless (isSuperKind sk)
                         (addErrL (hang (ptext (sLit "Ill-kinded kind:") <+> ppr k)
                                      2 (ptext (sLit "has kind:") <+> ppr sk))) }

lintArrow :: SDoc -> LintedKind -> LintedKind -> LintM LintedKind
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintArrow what k1 k2   -- Eg lintArrow "type or kind `blah'" k1 k2
                       -- or lintarrow "coercion `blah'" k1 k2
  | isSuperKind k1
  = return superKind
  | otherwise
  = do { unless (okArrowArgKind k1)    (addErrL (msg (ptext (sLit "argument")) k1))
       ; unless (okArrowResultKind k2) (addErrL (msg (ptext (sLit "result"))   k2))
       ; return liftedTypeKind }
  where
    msg ar k
      = vcat [ hang (ptext (sLit "Ill-kinded") <+> ar)
                  2 (ptext (sLit "in") <+> what)
             , what <+> ptext (sLit "kind:") <+> ppr k ]

lint_ty_app :: Type -> LintedKind -> [(LintedType,LintedKind)] -> LintM LintedKind
lint_ty_app ty k tys
  = lint_app (ptext (sLit "type") <+> quotes (ppr ty)) k tys

----------------
lint_co_app :: Coercion -> LintedKind -> [(LintedType,LintedKind)] -> LintM LintedKind
lint_co_app ty k tys
  = lint_app (ptext (sLit "coercion") <+> quotes (ppr ty)) k tys

----------------
lintTyLit :: TyLit -> LintM ()
lintTyLit (NumTyLit n)
  | n >= 0    = return ()
  | otherwise = failWithL msg
    where msg = ptext (sLit "Negative type literal:") <+> integer n
lintTyLit (StrTyLit _) = return ()

lint_app :: SDoc -> LintedKind -> [(LintedType,LintedKind)] -> LintM Kind
-- (lint_app d fun_kind arg_tys)
--    We have an application (f arg_ty1 .. arg_tyn),
--    where f :: fun_kind
-- Takes care of linting the OutTypes

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lint_app doc kfn kas
    = foldlM go_app kfn kas
  where
    fail_msg = vcat [ hang (ptext (sLit "Kind application error in")) 2 doc
                    , nest 2 (ptext (sLit "Function kind =") <+> ppr kfn)
                    , nest 2 (ptext (sLit "Arg kinds =") <+> ppr kas) ]

    go_app kfn ka
      | Just kfn' <- coreView kfn
      = go_app kfn' ka

    go_app (FunTy kfa kfb) (_,ka)
      = do { unless (ka `isSubKind` kfa) (addErrL fail_msg)
           ; return kfb }

    go_app (ForAllTy kv kfn) (ta,ka)
      = do { unless (ka `isSubKind` tyVarKind kv) (addErrL fail_msg)
           ; return (substKiWith [kv] [ta] kfn) }

    go_app _ _ = failWithL fail_msg

{-
************************************************************************
*                                                                      *
         Linting coercions
*                                                                      *
************************************************************************
-}

lintInCo :: InCoercion -> LintM (LintedKind, LintedType, LintedType, Role)
-- Check the coercion, and apply the substitution to it
-- See Note [Linting type lets]
lintInCo co
  = addLoc (InCo co) $
    do  { co' <- applySubstCo co
        ; lintCoercion co' }

lintCoercion :: OutCoercion -> LintM (LintedKind, LintedType, LintedType, Role)
-- Check the kind of a coercion term, returning the kind
-- Post-condition: the returned OutTypes are lint-free
--                 and have the same kind as each other

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoercion (Refl r ty)
  = do { k <- lintType ty
       ; return (k, ty, ty, r) }

lintCoercion co@(TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [co1,co2] <- cos
  = do { (k1,s1,t1,r1) <- lintCoercion co1
       ; (k2,s2,t2,r2) <- lintCoercion co2
       ; rk <- lintArrow (ptext (sLit "coercion") <+> quotes (ppr co)) k1 k2
       ; checkRole co1 r r1
       ; checkRole co2 r r2
       ; return (rk, mkFunTy s1 s2, mkFunTy t1 t2, r) }

  | Just {} <- synTyConDefn_maybe tc
  = failWithL (ptext (sLit "Synonym in TyConAppCo:") <+> ppr co)

  | otherwise
  = do { (ks,ss,ts,rs) <- mapAndUnzip4M lintCoercion cos
       ; rk <- lint_co_app co (tyConKind tc) (ss `zip` ks)
       ; _ <- zipWith3M checkRole cos (tyConRolesX r tc) rs
       ; return (rk, mkTyConApp tc ss, mkTyConApp tc ts, r) }

lintCoercion co@(AppCo co1 co2)
  = do { (k1,s1,t1,r1) <- lintCoercion co1
       ; (k2,s2,t2,r2) <- lintCoercion co2
       ; rk <- lint_co_app co k1 [(s2,k2)]
       ; if r1 == Phantom
         then checkL (r2 == Phantom || r2 == Nominal)
                     (ptext (sLit "Second argument in AppCo cannot be R:") $$
                      ppr co)
         else checkRole co Nominal r2
       ; return (rk, mkAppTy s1 s2, mkAppTy t1 t2, r1) }

lintCoercion (ForAllCo tv co)
  = do { lintTyBndrKind tv
       ; (k, s, t, r) <- addInScopeVar tv (lintCoercion co)
       ; return (k, mkForAllTy tv s, mkForAllTy tv t, r) }

lintCoercion (CoVarCo cv)
  | not (isCoVar cv)
  = failWithL (hang (ptext (sLit "Bad CoVarCo:") <+> ppr cv)
                  2 (ptext (sLit "With offending type:") <+> ppr (varType cv)))
  | otherwise
  = do { checkTyCoVarInScope cv
       ; cv' <- lookupIdInScope cv
       ; let (s,t) = coVarKind cv'
             k     = typeKind s
             r     = coVarRole cv'
       ; when (isSuperKind k) $
         do { checkL (r == Nominal) (hang (ptext (sLit "Non-nominal kind equality"))
                                     2 (ppr cv))
            ; checkL (s `eqKind` t) (hang (ptext (sLit "Non-refl kind equality"))
                                     2 (ppr cv)) }
       ; return (k, s, t, r) }

lintCoercion (UnivCo _prov r ty1 ty2)
  = do { k1 <- lintType ty1
       ; _k2 <- lintType ty2
--       ; unless (k1 `eqKind` k2) $
--         failWithL (hang (ptext (sLit "Unsafe coercion changes kind"))
--                       2 (ppr co))
       ; return (k1, ty1, ty2, r) }

lintCoercion (SymCo co)
  = do { (k, ty1, ty2, r) <- lintCoercion co
       ; return (k, ty2, ty1, r) }

lintCoercion co@(TransCo co1 co2)
  = do { (k1, ty1a, ty1b, r1) <- lintCoercion co1
       ; (_,  ty2a, ty2b, r2) <- lintCoercion co2
       ; checkL (ty1b `eqType` ty2a)
                (hang (ptext (sLit "Trans coercion mis-match:") <+> ppr co)
                    2 (vcat [ppr ty1a, ppr ty1b, ppr ty2a, ppr ty2b]))
       ; checkRole co r1 r2
       ; return (k1, ty1a, ty2b, r1) }

lintCoercion the_co@(NthCo n co)
  = do { (_,s,t,r) <- lintCoercion co
       ; case (splitTyConApp_maybe s, splitTyConApp_maybe t) of
           (Just (tc_s, tys_s), Just (tc_t, tys_t))
             | tc_s == tc_t
             , isDistinctTyCon tc_s || r /= Representational
                 -- see Note [NthCo and newtypes] in Coercion
             , tys_s `equalLength` tys_t
             , n < length tys_s
             -> return (ks, ts, tt, tr)
             where
               ts = getNth tys_s n
               tt = getNth tys_t n
               tr = nthRole r tc_s n
               ks = typeKind ts

           _ -> failWithL (hang (ptext (sLit "Bad getNth:"))
                              2 (ppr the_co $$ ppr s $$ ppr t)) }

lintCoercion the_co@(LRCo lr co)
  = do { (_,s,t,r) <- lintCoercion co
       ; checkRole co Nominal r
       ; case (splitAppTy_maybe s, splitAppTy_maybe t) of
           (Just s_pr, Just t_pr)
             -> return (k, s_pick, t_pick, Nominal)
             where
               s_pick = pickLR lr s_pr
               t_pick = pickLR lr t_pr
               k = typeKind s_pick

           _ -> failWithL (hang (ptext (sLit "Bad LRCo:"))
                              2 (ppr the_co $$ ppr s $$ ppr t)) }

lintCoercion (InstCo co arg_ty)
  = do { (k,s,t,r) <- lintCoercion co
       ; arg_kind  <- lintType arg_ty
       ; case (splitForAllTy_maybe s, splitForAllTy_maybe t) of
          (Just (tv1,ty1), Just (tv2,ty2))
            | arg_kind `isSubKind` tyVarKind tv1
            -> return (k, substTyWith [tv1] [arg_ty] ty1,
                          substTyWith [tv2] [arg_ty] ty2, r)
            | otherwise
            -> failWithL (ptext (sLit "Kind mis-match in inst coercion"))
          _ -> failWithL (ptext (sLit "Bad argument of inst")) }

lintCoercion co@(AxiomInstCo con ind cos)
  = do { unless (0 <= ind && ind < brListLength (coAxiomBranches con))
                (bad_ax (ptext (sLit "index out of range")))
         -- See Note [Kind instantiation in coercions]
       ; let CoAxBranch { cab_tvs   = ktvs
                        , cab_roles = roles
                        , cab_lhs   = lhs
                        , cab_rhs   = rhs } = coAxiomNthBranch con ind
       ; unless (equalLength ktvs cos) (bad_ax (ptext (sLit "lengths")))
       ; in_scope <- getInScope
       ; let empty_subst = mkTvSubst in_scope emptyTvSubstEnv
       ; (subst_l, subst_r) <- foldlM check_ki
                                      (empty_subst, empty_subst)
                                      (zip3 ktvs roles cos)
       ; let lhs' = Type.substTys subst_l lhs
             rhs' = Type.substTy subst_r rhs
       ; case checkAxInstCo co of
           Just bad_branch -> bad_ax $ ptext (sLit "inconsistent with") <+> (pprCoAxBranch (coAxiomTyCon con) bad_branch)
           Nothing -> return ()
       ; return (typeKind rhs', mkTyConApp (coAxiomTyCon con) lhs', rhs', coAxiomRole con) }
  where
    bad_ax what = addErrL (hang (ptext (sLit "Bad axiom application") <+> parens what)
                        2 (ppr co))

    check_ki (subst_l, subst_r) (ktv, role, co)
      = do { (k, t1, t2, r) <- lintCoercion co
           ; checkRole co role r
           ; let ktv_kind = Type.substTy subst_l (tyVarKind ktv)
                  -- Using subst_l is ok, because subst_l and subst_r
                  -- must agree on kind equalities
           ; unless (k `isSubKind` ktv_kind)
                    (bad_ax (ptext (sLit "check_ki2") <+> vcat [ ppr co, ppr k, ppr ktv, ppr ktv_kind ] ))
           ; return (Type.extendTvSubst subst_l ktv t1,
                     Type.extendTvSubst subst_r ktv t2) }

lintCoercion co@(SubCo co')
  = do { (k,s,t,r) <- lintCoercion co'
       ; checkRole co Nominal r
       ; return (k,s,t,Representational) }


lintCoercion this@(AxiomRuleCo co ts cs)
  = do _ks <- mapM lintType ts
       eqs <- mapM lintCoercion cs

       let tyNum = length ts

       case compare (coaxrTypeArity co) tyNum of
         EQ -> return ()
         LT -> err "Too many type arguments"
                    [ txt "expected" <+> int (coaxrTypeArity co)
                    , txt "provided" <+> int tyNum ]
         GT -> err "Not enough type arguments"
                    [ txt "expected" <+> int (coaxrTypeArity co)
                          , txt "provided" <+> int tyNum ]
       checkRoles 0 (coaxrAsmpRoles co) eqs

       case coaxrProves co ts [ Pair l r | (_,l,r,_) <- eqs ] of
         Nothing -> err "Malformed use of AxiomRuleCo" [ ppr this ]
         Just (Pair l r) ->
           do kL <- lintType l
              kR <- lintType r
              unless (eqKind kL kR)
                $ err "Kind error in CoAxiomRule"
                       [ppr kL <+> txt "/=" <+> ppr kR]
              return (kL, l, r, coaxrRole co)
  where
  txt       = ptext . sLit
  err m xs  = failWithL $
                hang (txt m) 2 $ vcat (txt "Rule:" <+> ppr (coaxrName co) : xs)

  checkRoles n (e : es) ((_,_,_,r) : rs)
    | e == r    = checkRoles (n+1) es rs
    | otherwise = err "Argument roles mismatch"
                      [ txt "In argument:" <+> int (n+1)
                      , txt "Expected:" <+> ppr e
                      , txt "Found:" <+> ppr r ]
  checkRoles _ [] []  = return ()
  checkRoles n [] rs  = err "Too many coercion arguments"
                          [ txt "Expected:" <+> int n
                          , txt "Provided:" <+> int (n + length rs) ]

  checkRoles n es []  = err "Not enough coercion arguments"
                          [ txt "Expected:" <+> int (n + length es)
                          , txt "Provided:" <+> int n ]

{-
************************************************************************
*                                                                      *
\subsection[lint-monad]{The Lint monad}
*                                                                      *
************************************************************************
-}

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism]
data LintEnv
  = LE { le_flags :: LintFlags       -- Linting the result of this pass
       , le_loc   :: [LintLocInfo]   -- Locations
       , le_subst :: TvSubst         -- Current type substitution; we also use this
    }                                -- to keep track of all the variables in scope,
                                     -- both Ids and TyVars

data LintFlags
  = LF { lf_check_global_ids           :: Bool -- See Note [Checking for global Ids]
       , lf_check_inline_loop_breakers :: Bool -- See Note [Checking for INLINE loop breakers]
    }

defaultLintFlags :: LintFlags
defaultLintFlags = LF { lf_check_global_ids = False
                      , lf_check_inline_loop_breakers = True }

newtype LintM a =
   LintM { unLintM ::
            LintEnv ->
            WarnsAndErrs ->           -- Error and warning messages so far
            (Maybe a, WarnsAndErrs) } -- Result and messages (if any)

type WarnsAndErrs = (Bag MsgDoc, Bag MsgDoc)

{- Note [Checking for global Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before CoreTidy, all locally-bound Ids must be LocalIds, even
top-level ones. See Note [Exported LocalIds] and Trac #9857.

Note [Type substitution]
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

instance Functor LintM where
      fmap = liftM

instance Applicative LintM where
      pure = return
      (<*>) = ap

instance Monad LintM where
  return x = LintM (\ _ errs -> (Just x, errs))
  fail err = failWithL (text err)
  m >>= k  = LintM (\ env errs ->
                       let (res, errs') = unLintM m env errs in
                         case res of
                           Just r -> unLintM (k r) env errs'
                           Nothing -> (Nothing, errs'))

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | LambdaBodyOf Id     -- The lambda-binder
  | BodyOfLetRec [Id]   -- One of the binders
  | CaseAlt CoreAlt     -- Case alternative
  | CasePat CoreAlt     -- The *pattern* of the case alternative
  | AnExpr CoreExpr     -- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
  | TopLevelBindings
  | InType Type         -- Inside a type
  | InCo   Coercion     -- Inside a coercion

initL :: LintFlags -> LintM a -> WarnsAndErrs    -- Errors and warnings
initL flags m
  = case unLintM m env (emptyBag, emptyBag) of
      (_, errs) -> errs
  where
    env = LE { le_flags = flags, le_subst = emptyTvSubst, le_loc = [] }

getLintFlags :: LintM LintFlags
getLintFlags = LintM $ \ env errs -> (Just (le_flags env), errs)

checkL :: Bool -> MsgDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = failWithL msg

failWithL :: MsgDoc -> LintM a
failWithL msg = LintM $ \ env (warns,errs) ->
                (Nothing, (warns, addMsg env errs msg))

addErrL :: MsgDoc -> LintM ()
addErrL msg = LintM $ \ env (warns,errs) ->
              (Just (), (warns, addMsg env errs msg))

addWarnL :: MsgDoc -> LintM ()
addWarnL msg = LintM $ \ env (warns,errs) ->
              (Just (), (addMsg env warns msg, errs))

addMsg :: LintEnv ->  Bag MsgDoc -> MsgDoc -> Bag MsgDoc
addMsg env msgs msg
  = ASSERT( notNull locs )
    msgs `snocBag` mk_msg msg
  where
   locs = le_loc env
   (loc, cxt1) = dumpLoc (head locs)
   cxts        = [snd (dumpLoc loc) | loc <- locs]
   context     | opt_PprStyle_Debug = vcat (reverse cxts) $$ cxt1 $$
                                      ptext (sLit "Substitution:") <+> ppr (le_subst env)
               | otherwise          = cxt1

   mk_msg msg = mkLocMessage SevWarning (mkSrcSpan loc loc) (context $$ msg)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m
  = LintM $ \ env errs ->
    unLintM m (env { le_loc = extra_loc : le_loc env }) errs

inCasePat :: LintM Bool         -- A slight hack; see the unique call site
inCasePat = LintM $ \ env errs -> (Just (is_case_pat env), errs)
  where
    is_case_pat (LE { le_loc = CasePat {} : _ }) = True
    is_case_pat _other                           = False

addInScopeVars :: [Var] -> LintM a -> LintM a
addInScopeVars vars m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = extendTvInScopeList (le_subst env) vars })
              errs

addInScopeVar :: Var -> LintM a -> LintM a
addInScopeVar var m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = extendTvInScope (le_subst env) var }) errs

extendSubstL :: TyVar -> Type -> LintM a -> LintM a
extendSubstL tv ty m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = Type.extendTvSubst (le_subst env) tv ty }) errs

updateTvSubst :: TvSubst -> LintM a -> LintM a
updateTvSubst subst' m
  = LintM $ \ env errs -> unLintM m (env { le_subst = subst' }) errs

getTvSubst :: LintM TvSubst
getTvSubst = LintM (\ env errs -> (Just (le_subst env), errs))

getInScope :: LintM InScopeSet
getInScope = LintM (\ env errs -> (Just (getTvInScope (le_subst env)), errs))

applySubstTy :: InType -> LintM OutType
applySubstTy ty = do { subst <- getTvSubst; return (Type.substTy subst ty) }

applySubstCo :: InCoercion -> LintM OutCoercion
applySubstCo co = do { subst <- getTvSubst; return (substCo (tvCvSubst subst) co) }

lookupIdInScope :: Id -> LintM Id
lookupIdInScope id
  | not (mustHaveLocalBinding id)
  = return id   -- An imported Id
  | otherwise
  = do  { subst <- getTvSubst
        ; case lookupInScope (getTvInScope subst) id of
                Just v  -> return v
                Nothing -> do { addErrL out_of_scope
                              ; return id } }
  where
    out_of_scope = pprBndr LetBind id <+> ptext (sLit "is out of scope")


oneTupleDataConId :: Id -- Should not happen
oneTupleDataConId = dataConWorkId (tupleCon BoxedTuple 1)

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
             (hsep [pprBndr LetBind var, loc_msg]) }

checkTys :: OutType -> OutType -> MsgDoc -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have alrady had the substitution applied
checkTys ty1 ty2 msg = checkL (ty1 `eqType` ty2) msg

checkRole :: Coercion
          -> Role      -- expected
          -> Role      -- actual
          -> LintM ()
checkRole co r1 r2
  = checkL (r1 == r2)
           (ptext (sLit "Role incompatibility: expected") <+> ppr r1 <> comma <+>
            ptext (sLit "got") <+> ppr r2 $$
            ptext (sLit "in") <+> ppr co)

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

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
  = (noSrcLoc, Outputable.empty)
dumpLoc (InType ty)
  = (noSrcLoc, text "In the type" <+> quotes (ppr ty))
dumpLoc (InCo co)
  = (noSrcLoc, text "In the coercion" <+> quotes (ppr co))

pp_binders :: [Var] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Var -> SDoc
pp_binder b | isId b    = hsep [ppr b, dcolon, ppr (idType b)]
            | otherwise = hsep [ppr b, dcolon, ppr (tyVarKind b)]

------------------------------------------------------
--      Messages for case expressions

mkDefaultArgsMsg :: [Var] -> MsgDoc
mkDefaultArgsMsg args
  = hang (text "DEFAULT case with binders")
         4 (ppr args)

mkCaseAltMsg :: CoreExpr -> Type -> Type -> MsgDoc
mkCaseAltMsg e ty1 ty2
  = hang (text "Type of case alternatives not the same as the annotation on case:")
         4 (vcat [ppr ty1, ppr ty2, ppr e])

mkScrutMsg :: Id -> Type -> Type -> TvSubst -> MsgDoc
mkScrutMsg var var_ty scrut_ty subst
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
          text "Result binder type:" <+> ppr var_ty,--(idType var),
          text "Scrutinee type:" <+> ppr scrut_ty,
     hsep [ptext (sLit "Current TV subst"), ppr subst]]

mkNonDefltMsg, mkNonIncreasingAltsMsg :: CoreExpr -> MsgDoc
mkNonDefltMsg e
  = hang (text "Case expression with DEFAULT not at the beginnning") 4 (ppr e)
mkNonIncreasingAltsMsg e
  = hang (text "Case expression with badly-ordered alternatives") 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> MsgDoc
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives") 4 (ppr e)

mkBadConMsg :: TyCon -> DataCon -> MsgDoc
mkBadConMsg tycon datacon
  = vcat [
        text "In a case alternative, data constructor isn't in scrutinee type:",
        text "Scrutinee type constructor:" <+> ppr tycon,
        text "Data con:" <+> ppr datacon
    ]

mkBadPatMsg :: Type -> Type -> MsgDoc
mkBadPatMsg con_result_ty scrut_ty
  = vcat [
        text "In a case alternative, pattern result type doesn't match scrutinee type:",
        text "Pattern result type:" <+> ppr con_result_ty,
        text "Scrutinee type:" <+> ppr scrut_ty
    ]

integerScrutinisedMsg :: MsgDoc
integerScrutinisedMsg
  = text "In a LitAlt, the literal is lifted (probably Integer)"

mkBadAltMsg :: Type -> CoreAlt -> MsgDoc
mkBadAltMsg scrut_ty alt
  = vcat [ text "Data alternative when scrutinee is not a tycon application",
           text "Scrutinee type:" <+> ppr scrut_ty,
           text "Alternative:" <+> pprCoreAlt alt ]

mkNewTyDataConAltMsg :: Type -> CoreAlt -> MsgDoc
mkNewTyDataConAltMsg scrut_ty alt
  = vcat [ text "Data alternative for newtype datacon",
           text "Scrutinee type:" <+> ppr scrut_ty,
           text "Alternative:" <+> pprCoreAlt alt ]


------------------------------------------------------
--      Other error messages

mkAppMsg :: Type -> Type -> CoreExpr -> MsgDoc
mkAppMsg fun_ty arg_ty arg
  = vcat [ptext (sLit "Argument value doesn't match argument type:"),
              hang (ptext (sLit "Fun type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg type:")) 4 (ppr arg_ty),
              hang (ptext (sLit "Arg:")) 4 (ppr arg)]

mkNonFunAppMsg :: Type -> Type -> CoreExpr -> MsgDoc
mkNonFunAppMsg fun_ty arg_ty arg
  = vcat [ptext (sLit "Non-function type in function position"),
              hang (ptext (sLit "Fun type:")) 4 (ppr fun_ty),
              hang (ptext (sLit "Arg type:")) 4 (ppr arg_ty),
              hang (ptext (sLit "Arg:")) 4 (ppr arg)]

mkLetErr :: TyVar -> CoreExpr -> MsgDoc
mkLetErr bndr rhs
  = vcat [ptext (sLit "Bad `let' binding:"),
          hang (ptext (sLit "Variable:"))
                 4 (ppr bndr <+> dcolon <+> ppr (varType bndr)),
          hang (ptext (sLit "Rhs:"))
                 4 (ppr rhs)]

mkTyAppMsg :: Type -> Type -> MsgDoc
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
              hang (ptext (sLit "Exp type:"))
                 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
              hang (ptext (sLit "Arg type:"))
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkRhsMsg :: Id -> SDoc -> Type -> MsgDoc
mkRhsMsg binder what ty
  = vcat
    [hsep [ptext (sLit "The type of this binder doesn't match the type of its") <+> what <> colon,
            ppr binder],
     hsep [ptext (sLit "Binder's type:"), ppr (idType binder)],
     hsep [ptext (sLit "Rhs type:"), ppr ty]]

mkLetAppMsg :: CoreExpr -> MsgDoc
mkLetAppMsg e
  = hang (ptext (sLit "This argument does not satisfy the let/app invariant:"))
       2 (ppr e)

mkRhsPrimMsg :: Id -> CoreExpr -> MsgDoc
mkRhsPrimMsg binder _rhs
  = vcat [hsep [ptext (sLit "The type of this binder is primitive:"),
                     ppr binder],
              hsep [ptext (sLit "Binder's type:"), ppr (idType binder)]
             ]

mkStrictMsg :: Id -> MsgDoc
mkStrictMsg binder
  = vcat [hsep [ptext (sLit "Recursive or top-level binder has strict demand info:"),
                     ppr binder],
              hsep [ptext (sLit "Binder's demand info:"), ppr (idDemandInfo binder)]
             ]

mkNonTopExportedMsg :: Id -> MsgDoc
mkNonTopExportedMsg binder
  = hsep [ptext (sLit "Non-top-level binder is marked as exported:"), ppr binder]

mkNonTopExternalNameMsg :: Id -> MsgDoc
mkNonTopExternalNameMsg binder
  = hsep [ptext (sLit "Non-top-level binder has an external name:"), ppr binder]

mkKindErrMsg :: TyVar -> Type -> MsgDoc
mkKindErrMsg tyvar arg_ty
  = vcat [ptext (sLit "Kinds don't match in type application:"),
          hang (ptext (sLit "Type variable:"))
                 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
          hang (ptext (sLit "Arg type:"))
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

{- Not needed now
mkArityMsg :: Id -> MsgDoc
mkArityMsg binder
  = vcat [hsep [ptext (sLit "Demand type has"),
                ppr (dmdTypeDepth dmd_ty),
                ptext (sLit "arguments, rhs has"),
                ppr (idArity binder),
                ptext (sLit "arguments,"),
                ppr binder],
              hsep [ptext (sLit "Binder's strictness signature:"), ppr dmd_ty]

         ]
           where (StrictSig dmd_ty) = idStrictness binder
-}
mkCastErr :: CoreExpr -> Coercion -> Type -> Type -> MsgDoc
mkCastErr expr co from_ty expr_ty
  = vcat [ptext (sLit "From-type of Cast differs from type of enclosed expression"),
          ptext (sLit "From-type:") <+> ppr from_ty,
          ptext (sLit "Type of enclosed expr:") <+> ppr expr_ty,
          ptext (sLit "Actual enclosed expr:") <+> ppr expr,
          ptext (sLit "Coercion used in cast:") <+> ppr co
         ]

dupVars :: [[Var]] -> MsgDoc
dupVars vars
  = hang (ptext (sLit "Duplicate variables brought into scope"))
       2 (ppr vars)

dupExtVars :: [[Name]] -> MsgDoc
dupExtVars vars
  = hang (ptext (sLit "Duplicate top-level variables with the same qualified name"))
       2 (ppr vars)

{-
************************************************************************
*                                                                      *
\subsection{Annotation Linting}
*                                                                      *
************************************************************************
-}

-- | This checks whether a pass correctly looks through debug
-- annotations (@SourceNote@). This works a bit different from other
-- consistency checks: We check this by running the given task twice,
-- noting all differences between the results.
lintAnnots :: SDoc -> (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
lintAnnots pname pass guts = do
  -- Run the pass as we normally would
  dflags <- getDynFlags
  when (gopt Opt_DoAnnotationLinting dflags) $
    liftIO $ Err.showPass dflags "Annotation linting - first run"
  nguts <- pass guts
  -- If appropriate re-run it without debug annotations to make sure
  -- that they made no difference.
  when (gopt Opt_DoAnnotationLinting dflags) $ do
    liftIO $ Err.showPass dflags "Annotation linting - second run"
    nguts' <- withoutAnnots pass guts
    -- Finally compare the resulting bindings
    liftIO $ Err.showPass dflags "Annotation linting - comparison"
    let binds = flattenBinds $ mg_binds nguts
        binds' = flattenBinds $ mg_binds nguts'
        (diffs,_) = diffBinds True (mkRnEnv2 emptyInScopeSet) binds binds'
    when (not (null diffs)) $ CoreMonad.putMsg $ vcat
      [ lint_banner "warning" pname
      , text "Core changes with annotations:"
      , withPprStyle defaultDumpStyle $ nest 2 $ vcat diffs
      ]
  -- Return actual new guts
  return nguts

-- | Run the given pass without annotations. This means that we both
-- remove the @Opt_Debug@ flag from the environment as well as all
-- annotations from incoming modules.
withoutAnnots :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
withoutAnnots pass guts = do
  -- Remove debug flag from environment.
  dflags <- getDynFlags
  let removeFlag env = env{hsc_dflags = gopt_unset dflags Opt_Debug}
      withoutFlag corem =
        liftIO =<< runCoreM <$> fmap removeFlag getHscEnv <*> getRuleBase <*>
                                getUniqueSupplyM <*> getModule <*>
                                getPrintUnqualified <*> pure corem
  -- Nuke existing ticks in module.
  -- TODO: Ticks in unfoldings. Maybe change unfolding so it removes
  -- them in absence of @Opt_Debug@?
  let nukeTicks = stripTicksE (not . tickishIsCode)
      nukeAnnotsBind :: CoreBind -> CoreBind
      nukeAnnotsBind bind = case bind of
        Rec bs     -> Rec $ map (\(b,e) -> (b, nukeTicks e)) bs
        NonRec b e -> NonRec b $ nukeTicks e
      nukeAnnotsMod mg@ModGuts{mg_binds=binds}
        = mg{mg_binds = map nukeAnnotsBind binds}
  -- Perform pass with all changes applied
  fmap fst $ withoutFlag $ pass (nukeAnnotsMod guts)
