{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


A ``lint'' pass to check for Core correctness
-}

{-# LANGUAGE CPP #-}

module CoreLint (
    lintCoreBindings, lintUnfolding,
    lintPassResult, lintInteractiveExpr, lintExpr,
    lintAnnots,

    -- ** Debug output
    endPass, endPassIO,
    dumpPassResult,
    CoreLint.dumpIfSet,
 ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils
import CoreStats   ( coreBindsStats )
import CoreMonad
import Bag
import Literal
import DataCon
import TysWiredIn
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
import RepType
import TyCoRep       -- checks validity of types/coercions
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
import CoreArity ( typeArity )
import Demand ( splitStrictSig, isBotRes )

import HscTypes
import DynFlags
import Control.Monad
#if __GLASGOW_HASKELL__ > 710
import qualified Control.Monad.Fail as MonadFail
#endif
import MonadUtils
import Data.Function (fix)
import Data.Maybe
import Pair
import qualified GHC.LanguageExtensions as LangExt

{-
Note [GHC Formalism]
~~~~~~~~~~~~~~~~~~~~
This file implements the type-checking algorithm for System FC, the "official"
name of the Core language. Type safety of FC is heart of the claim that
executables produced by GHC do not have segmentation faults. Thus, it is
useful to be able to reason about System FC independently of reading the code.
To this purpose, there is a document core-spec.pdf built in docs/core-spec that
contains a formalism of the types and functions dealt with here. If you change
just about anything in this file or you change other types/functions throughout
the Core language (all signposted to this note), you should update that
formalism. See docs/core-spec/README for more info about how to do so.

Note [check vs lint]
~~~~~~~~~~~~~~~~~~~~
This file implements both a type checking algorithm and also general sanity
checking. For example, the "sanity checking" checks for TyConApp on the left
of an AppTy, which should never happen. These sanity checks don't really
affect any notion of type soundness. Yet, it is convenient to do the sanity
checks at the same time as the type checks. So, we use the following naming
convention:

- Functions that begin with 'lint'... are involved in type checking. These
  functions might also do some sanity checking.

- Functions that begin with 'check'... are *not* involved in type checking.
  They exist only for sanity checking.

Issues surrounding variable naming, shadowing, and such are considered *not*
to be part of type checking, as the formalism omits these details.

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
        (e) Incorrect unsafe coercions

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
        lintInTy :: Type -> LintM (Type, Kind)
returns a substituted type.

When we encounter a binder (like x::a) we must apply the substitution
to the type of the binding variable.  lintBinders does this.

For Ids, the type-substituted Id is added to the in_scope set (which
itself is part of the TCvSubst we are carrying down), and when we
find an occurrence of an Id, we fetch it from the in-scope set.

Note [Bad unsafe coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~

For discussion see https://ghc.haskell.org/trac/ghc/wiki/BadUnsafeCoercions
Linter introduces additional rules that checks improper coercion between
different types, called bad coercions. Following coercions are forbidden:

  (a) coercions between boxed and unboxed values;
  (b) coercions between unlifted values of the different sizes, here
      active size is checked, i.e. size of the actual value but not
      the space allocated for value;
  (c) coercions between floating and integral boxed values, this check
      is not yet supported for unboxed tuples, as no semantics were
      specified for that;
  (d) coercions from / to vector type
  (e) If types are unboxed tuples then tuple (# A_1,..,A_n #) can be
      coerced to (# B_1,..,B_m #) if n=m and for each pair A_i, B_i rules
      (a-e) holds.

************************************************************************
*                                                                      *
                 Beginning and ending passes
*                                                                      *
************************************************************************

These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a conveneint place.  place for them.  They print out
stuff before and after core passes, and do Core Lint when necessary.
-}

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
  = do { forM_ mb_flag $ \flag ->
           Err.dumpSDoc dflags unqual flag (showSDoc dflags hdr) dump_doc

         -- Report result size
         -- This has the side effect of forcing the intermediate to be evaluated
         -- if it's not already forced by a -ddump flag.
       ; Err.debugTraceMsg dflags 2 size_doc
       }

  where
    size_doc = sep [text "Result size of" <+> hdr, nest 2 (equals <+> ppr (coreBindsStats binds))]

    dump_doc  = vcat [ nest 2 extra_info
                     , size_doc
                     , blankLine
                     , pprCoreBindingsWithSize binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
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
  = do { let (warns, errs) = lintCoreBindings dflags pass (interactiveInScope hsc_env) binds
       ; Err.showPass dflags ("Core Linted result of " ++ showPpr dflags pass)
       ; displayLintResults dflags pass warns errs binds  }
  where
    dflags = hsc_dflags hsc_env

displayLintResults :: DynFlags -> CoreToDo
                   -> Bag Err.MsgDoc -> Bag Err.MsgDoc -> CoreProgram
                   -> IO ()
displayLintResults dflags pass warns errs binds
  | not (isEmptyBag errs)
  = do { log_action dflags dflags NoReason Err.SevDump noSrcSpan defaultDumpStyle
           (vcat [ lint_banner "errors" (ppr pass), Err.pprMessageBag errs
                 , text "*** Offending Program ***"
                 , pprCoreBindings binds
                 , text "*** End of Offense ***" ])
       ; Err.ghcExit dflags 1 }

  | not (isEmptyBag warns)
  , not opt_NoDebugOutput
  , showLintWarnings pass
  = log_action dflags dflags NoReason Err.SevDump noSrcSpan defaultDumpStyle
        (lint_banner "warnings" (ppr pass) $$ Err.pprMessageBag warns)

  | otherwise = return ()
  where

lint_banner :: String -> SDoc -> SDoc
lint_banner string pass = text "*** Core Lint"      <+> text string
                          <+> text ": in result of" <+> pass
                          <+> text "***"

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings (CoreDoSimplify _ (SimplMode { sm_phase = InitialPhase })) = False
showLintWarnings _ = True

lintInteractiveExpr :: String -> HscEnv -> CoreExpr -> IO ()
lintInteractiveExpr what hsc_env expr
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | Just err <- lintExpr dflags (interactiveInScope hsc_env) expr
  = do { display_lint_err err
       ; Err.ghcExit dflags 1 }
  | otherwise
  = return ()
  where
    dflags = hsc_dflags hsc_env

    display_lint_err err
      = do { log_action dflags dflags NoReason Err.SevDump
               noSrcSpan defaultDumpStyle
               (vcat [ lint_banner "errors" (text what)
                     , err
                     , text "*** Offending Program ***"
                     , pprCoreExpr expr
                     , text "*** End of Offense ***" ])
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
  = tyvars ++ ids
  where
    -- C.f. TcRnDriver.setInteractiveContext, Desugar.deSugarExpr
    ictxt                   = hsc_IC hsc_env
    (cls_insts, _fam_insts) = ic_instances ictxt
    te1    = mkTypeEnvWithImplicits (ic_tythings ictxt)
    te     = extendTypeEnvWithIds te1 (map instanceDFunId cls_insts)
    ids    = typeEnvIds te
    tyvars = tyCoVarsOfTypesList $ map idType ids
              -- Why the type variables?  How can the top level envt have free tyvars?
              -- I think it's because of the GHCi debugger, which can bind variables
              --   f :: [t] -> [t]
              -- where t is a RuntimeUnk (see TcType)

lintCoreBindings :: DynFlags -> CoreToDo -> [Var] -> CoreProgram -> (Bag MsgDoc, Bag MsgDoc)
--   Returns (warnings, errors)
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreBindings dflags pass local_in_scope binds
  = initL dflags flags $
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
               , lf_check_inline_loop_breakers = check_lbs
               , lf_check_static_ptrs = check_static_ptrs }

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

    -- See Note [Checking StaticPtrs]
    check_static_ptrs = xopt LangExt.StaticPointers dflags &&
                        case pass of
                          CoreDoFloatOutwards _ -> True
                          CoreTidy              -> True
                          CorePrep              -> True
                          _                     -> False

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

lintUnfolding :: DynFlags
              -> SrcLoc
              -> VarSet         -- Treat these as in scope
              -> CoreExpr
              -> Maybe MsgDoc   -- Nothing => OK

lintUnfolding dflags locn vars expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL dflags defaultLintFlags linter
    linter = addLoc (ImportedUnfolding locn) $
             addInScopeVarSet vars           $
             lintCoreExpr expr

lintExpr :: DynFlags
         -> [Var]               -- Treat these as in scope
         -> CoreExpr
         -> Maybe MsgDoc        -- Nothing => OK

lintExpr dflags vars expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL dflags defaultLintFlags linter
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
  -- CorePrep introduces bindings for data constructors. This is just a hack for the
  -- code generator, the definitions do not matter (and would fail some of the
  -- checks below). Therefore, skip these.
  -- See Note [Data constructor workers] in CorePrep
  | isDataConWorkId binder
  = return ()
  | otherwise
  = addLoc (RhsOf binder) $
         -- Check the rhs
    do { ty <- lintRhs rhs
       ; lintBinder binder -- Check match to RHS type
       ; binder_ty <- applySubstTy (idType binder)
       ; ensureEqTys binder_ty ty (mkRhsMsg binder (text "RHS") ty)

        -- Check the let/app invariant
        -- See Note [CoreSyn let/app invariant] in CoreSyn
       ; checkL (not (isUnliftedType binder_ty)
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

       ; flags <- getLintFlags
       ; when (lf_check_inline_loop_breakers flags
               && isStrongLoopBreaker (idOccInfo binder)
               && isInlinePragma (idInlinePragma binder))
              (addWarnL (text "INLINE binder is (non-rule) loop breaker:" <+> ppr binder))
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

       -- Check that the binder's arity is within the bounds imposed by
       -- the type and the strictness signature. See Note [exprArity invariant]
       -- and Note [Trimming arity]
       ; checkL (idArity binder <= length (typeArity (idType binder)))
           (text "idArity" <+> ppr (idArity binder) <+>
           text "exceeds typeArity" <+>
           ppr (length (typeArity (idType binder))) <> colon <+>
           ppr binder)

       ; case splitStrictSig (idStrictness binder) of
           (demands, result_info) | isBotRes result_info ->
             checkL (idArity binder <= length demands)
               (text "idArity" <+> ppr (idArity binder) <+>
               text "exceeds arity imposed by the strictness signature" <+>
               ppr (idStrictness binder) <> colon <+>
               ppr binder)
           _ -> return ()

       ; mapM_ (lintCoreRule binder_ty) (idCoreRules binder)
       ; lintIdUnfolding binder binder_ty (idUnfolding binder) }

        -- We should check the unfolding, if any, but this is tricky because
        -- the unfolding is a SimplifiableCoreExpr. Give up for now.
   where
    -- If you edit this function, you may need to update the GHC formalism
    -- See Note [GHC Formalism]
    lintBinder var | isId var  = lintIdBndr var $ \_ -> (return ())
                   | otherwise = return ()

-- | Checks the RHS of top-level bindings. It only differs from 'lintCoreExpr'
-- in that it doesn't reject applications of the data constructor @StaticPtr@
-- when they appear at the top level.
--
-- See Note [Checking StaticPtrs].
lintRhs :: CoreExpr -> LintM OutType
-- Allow applications of the data constructor @StaticPtr@ at the top
-- but produce errors otherwise.
lintRhs rhs
    | (binders0, rhs') <- collectTyBinders rhs
    , Just (fun, args) <- collectStaticPtrSatArgs rhs'
    = flip fix binders0 $ \loopBinders binders -> case binders of
        -- imitate @lintCoreExpr (Lam ...)@
        var : vars -> addLoc (LambdaBodyOf var) $
                      lintBinder var $ \var' ->
                      do { body_ty <- loopBinders vars
                         ; return $ mkLamType var' body_ty }
        -- imitate @lintCoreExpr (App ...)@
        [] -> do
          fun_ty <- lintCoreExpr fun
          addLoc (AnExpr rhs') $ foldM lintCoreArg fun_ty args
-- Rejects applications of the data constructor @StaticPtr@ if it finds any.
lintRhs rhs = lintCoreExpr rhs

lintIdUnfolding :: Id -> Type -> Unfolding -> LintM ()
lintIdUnfolding bndr bndr_ty (CoreUnfolding { uf_tmpl = rhs, uf_src = src })
  | isStableSource src
  = do { ty <- lintCoreExpr rhs
       ; ensureEqTys bndr_ty ty (mkRhsMsg bndr (text "unfolding") ty) }
lintIdUnfolding  _ _ _
  = return ()       -- Do not Lint unstable unfoldings, because that leads
                    -- to exponential behaviour; c.f. CoreFVs.idUnfoldingVars

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

type InType      = Type
type InCoercion  = Coercion
type InVar       = Var
type InTyVar     = Var
type InCoVar     = Var

type OutType     = Type -- Substitution has been applied to this,
                        -- but has not been linted yet
type OutKind     = Kind

type LintedType  = Type -- Substitution applied, and type is linted
type LintedKind  = Kind

type OutCoercion    = Coercion
type OutVar         = Var
type OutTyVar       = TyVar
type OutCoVar       = Var

lintCoreExpr :: CoreExpr -> LintM OutType
-- The returned type has the substitution from the monad
-- already applied to it:
--      lintCoreExpr e subst = exprType (subst e)
--
-- The returned "type" can be a kind, if the expression is (Type ty)

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreExpr (Var var)
  = do  { checkL (isNonCoVarId var)
                 (text "Non term variable" <+> ppr var)

        ; checkBadDataConWorker var
        ; checkDeadIdOcc var
        ; var' <- lookupIdInScope var
        ; return (idType var') }

lintCoreExpr (Lit lit)
  = return (literalType lit)

lintCoreExpr (Cast expr co)
  = do { expr_ty <- lintCoreExpr expr
       ; co' <- applySubstCo co
       ; (_, k2, from_ty, to_ty, r) <- lintCoercion co'
       ; lintL (classifiesTypeWithValues k2)
               (text "Target of cast not # or *:" <+> ppr co)
       ; lintRole co' Representational r
       ; ensureEqTys from_ty expr_ty (mkCastErr expr co' from_ty expr_ty)
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
    do  { addLoc (RhsOf tv) $ lintTyKind tv' ty'
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
    = do lf <- getLintFlags
         -- Check for a nested occurrence of the StaticPtr constructor.
         -- See Note [Checking StaticPtrs].
         -- TODO #12618 remove here eventually
         case fun of
           Var b | lf_check_static_ptrs lf
                 , Just con <- isDataConId_maybe b
                 , dataConName con == staticPtrDataConName
                 -> do
              failWithL $ text "Found StaticPtr nested in an expression: " <+>
                          ppr e
           Var b | Just con <- isDataConWorkId_maybe b
                 , dataConRepFullArity con <= length args
                 -> do
              addErrL $ text "Found saturated use of data con worker (should use ConApp): " <+> ppr e
              go
           ConApp _ _ -> do
              addErrL $ text "Found ConApp in argument position. Is that possible?" <+> ppr e $$ ppr args
              go
           _     -> go
  where
    go = do { fun_ty <- lintCoreExpr fun
            ; addLoc (AnExpr e) $ foldM lintCoreArg fun_ty args }

    (fun, args) = collectArgs e

lintCoreExpr e@(ConApp dc args)
    = do lf <- getLintFlags
         -- Check for a nested occurrence of the StaticPtr constructor.
         -- See Note [Checking StaticPtrs].
         when (lf_check_static_ptrs lf && dataConName dc == staticPtrDataConName) $
            failWithL $ text "Found StaticPtr nested in an expression: " <+>
                        ppr e
         when (length args /= dataConRepFullArity dc) $
            addErrL $ hang (text "Un-saturated data con application") 2 (ppr e)
         when (isNewTyCon (dataConTyCon dc)) $
            addErrL $ hang (text "ConApp with newtype constructor") 2 (ppr e)
         let dc_ty = dataConRepType dc
         addLoc (AnExpr e) $ foldM lintCoreArg dc_ty args

lintCoreExpr (Lam var expr)
  = addLoc (LambdaBodyOf var) $
    lintBinder var $ \ var' ->
    do { body_ty <- lintCoreExpr expr
       ; return $ mkLamType var' body_ty }

lintCoreExpr e@(Case scrut var alt_ty alts) =
       -- Check the scrutinee
  do { let scrut_diverges = exprIsBottom scrut
     ; scrut_ty <- lintCoreExpr scrut
     ; (alt_ty, _) <- lintInTy alt_ty
     ; (var_ty, _) <- lintInTy (idType var)

     -- See Note [No alternatives lint check]
     ; when (null alts) $
     do { checkL (not (exprIsHNF scrut))
          (text "No alternatives for a case scrutinee in head-normal form:" <+> ppr scrut)
        ; checkWarnL scrut_diverges
          (text "No alternatives for a case scrutinee not known to diverge for sure:" <+> ppr scrut)
        }

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
              | debugIsOn
              , isAlgTyCon tycon
              , not (isAbstractTyCon tycon)
              , null (tyConDataCons tycon)
              , not scrut_diverges
              -> pprTrace "Lint warning: case binder's type has no constructors" (ppr var <+> ppr (idType var))
                        -- This can legitimately happen for type families
                      $ return ()
         _otherwise -> return ()

        -- Don't use lintIdBndr on var, because unboxed tuple is legitimate

     ; subst <- getTCvSubst
     ; ensureEqTys var_ty scrut_ty (mkScrutMsg var var_ty scrut_ty subst)

     ; lintAndScopeId var $ \_ ->
       do { -- Check the alternatives
            mapM_ (lintCoreAlt scrut_ty alt_ty) alts
          ; checkCaseAlts e scrut_ty alts
          ; return alt_ty } }

-- This case can't happen; linting types in expressions gets routed through
-- lintCoreArgs
lintCoreExpr (Type ty)
  = failWithL (text "Type found as expression" <+> ppr ty)

lintCoreExpr (Coercion co)
  = do { (k1, k2, ty1, ty2, role) <- lintInCo co
       ; return (mkHeteroCoercionType role k1 k2 ty1 ty2) }

{-
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
  = do { checkL (not (isCoercionTy arg_ty))
                (text "Unnecessary coercion-to-type injection:"
                  <+> ppr arg_ty)
       ; arg_ty' <- applySubstTy arg_ty
       ; lintTyApp fun_ty arg_ty' }

lintCoreArg fun_ty arg
  = do { arg_ty <- lintCoreExpr arg
       ; checkL (not (isUnliftedType arg_ty) || exprOkForSpeculation arg)
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
  = ensureEqTys con_ty scrut_ty (mkBadPatMsg con_ty scrut_ty)
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
  | Just (tv,body_ty) <- splitForAllTy_maybe fun_ty
  = do  { lintTyKind tv arg_ty
        ; in_scope <- getInScope
        -- substTy needs the set of tyvars in scope to avoid generating
        -- uniques that are already in scope.
        -- See Note [The substitution invariant] in TyCoRep
        ; return (substTyWithInScope in_scope [tv] [arg_ty] body_ty) }

  | otherwise
  = failWithL (mkTyAppMsg fun_ty arg_ty)

-----------------
lintValApp :: CoreExpr -> OutType -> OutType -> LintM OutType
lintValApp arg fun_ty arg_ty
  | Just (arg,res) <- splitFunTy_maybe fun_ty
  = do { ensureEqTys arg arg_ty err1
       ; return res }
  | otherwise
  = failWithL err2
  where
    err1 = mkAppMsg       fun_ty arg_ty arg
    err2 = mkNonFunAppMsg fun_ty arg_ty arg

lintTyKind :: OutTyVar -> OutType -> LintM ()
-- Both args have had substitution applied

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintTyKind tyvar arg_ty
        -- Arg type might be boxed for a function with an uncommitted
        -- tyvar; notably this is used so that we can give
        --      error :: forall a:*. String -> a
        -- and then apply it to both boxed and unboxed types.
  = do { arg_kind <- lintType arg_ty
       ; unless (arg_kind `eqType` tyvar_kind)
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
                (text "Occurrence of a dead Id" <+> ppr id) }
  | otherwise
  = return ()

checkBadDataConWorker :: Id -> LintM ()
-- We do not want to see data con workers here, but for newtypes
-- (It should either be a ConApp or a reference to the wrapper)
checkBadDataConWorker id
  | Just dc <- isDataConWorkId_maybe id
  , dataConName dc /= staticPtrDataConName
  = checkL (isNewTyCon (dataConTyCon dc))
           (text "data constructor worker found" <+> ppr id) 
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
--     the simplifier correctly eliminates case that can't
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

lintAltExpr :: CoreExpr -> OutType -> LintM ()
lintAltExpr expr ann_ty
  = do { actual_ty <- lintCoreExpr expr
       ; ensureEqTys actual_ty ann_ty (mkCaseAltMsg expr actual_ty ann_ty) }

lintCoreAlt :: OutType          -- Type of scrutinee
            -> OutType          -- Type of the alternative
            -> CoreAlt
            -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreAlt _ alt_ty (DEFAULT, args, rhs) =
  do { lintL (null args) (mkDefaultArgsMsg args)
     ; lintAltExpr rhs alt_ty }

lintCoreAlt scrut_ty alt_ty (LitAlt lit, args, rhs)
  | litIsLifted lit
  = failWithL integerScrutinisedMsg
  | otherwise
  = do { lintL (null args) (mkDefaultArgsMsg args)
       ; ensureEqTys lit_ty scrut_ty (mkBadPatMsg lit_ty scrut_ty)
       ; lintAltExpr rhs alt_ty }
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
      lintL (tycon == dataConTyCon con) (mkBadConMsg tycon con)
    ; let con_payload_ty = piResultTys (dataConRepType con) tycon_arg_tys

        -- And now bring the new binders into scope
    ; lintBinders args $ \ args' -> do
    { addLoc (CasePat alt) (lintAltBinders scrut_ty con_payload_ty args')
    ; lintAltExpr rhs alt_ty } }

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
  | isTyVar var = lintTyBndr var linterF
  | isCoVar var = lintCoBndr var linterF
  | otherwise   = lintIdBndr var linterF

lintTyBndr :: InTyVar -> (OutTyVar -> LintM a) -> LintM a
lintTyBndr tv thing_inside
  = do { subst <- getTCvSubst
       ; let (subst', tv') = substTyVarBndr subst tv
       ; lintKind (varType tv')
       ; updateTCvSubst subst' (thing_inside tv') }

lintCoBndr :: InCoVar -> (OutCoVar -> LintM a) -> LintM a
lintCoBndr cv thing_inside
  = do { subst <- getTCvSubst
       ; let (subst', cv') = substCoVarBndr subst cv
       ; lintKind (varType cv')
       ; lintL (isCoercionType (varType cv'))
               (text "CoVar with non-coercion type:" <+> pprTvBndr cv)
       ; updateTCvSubst subst' (thing_inside cv') }

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
                (text "Non-local Id binder" <+> ppr id)
                -- See Note [Checking for global Ids]
       ; (ty, k) <- lintInTy (idType id)
       ; lintL (not (isRuntimeRepPolymorphic k))
           (text "RuntimeRep-polymorphic binder:" <+>
                 (ppr id <+> dcolon <+> parens (ppr ty <+> dcolon <+> ppr k)))
       ; let id' = setIdType id ty
       ; addInScopeVar id' $ (linterF id') }

{-
%************************************************************************
%*                                                                      *
             Types
%*                                                                      *
%************************************************************************
-}

lintInTy :: InType -> LintM (LintedType, LintedKind)
-- Types only, not kinds
-- Check the type, and apply the substitution to it
-- See Note [Linting type lets]
lintInTy ty
  = addLoc (InType ty) $
    do  { ty' <- applySubstTy ty
        ; k  <- lintType ty'
        ; lintKind k
        ; return (ty', k) }

checkTyCon :: TyCon -> LintM ()
checkTyCon tc
  = checkL (not (isTcTyCon tc)) (text "Found TcTyCon:" <+> ppr tc)

-------------------
lintType :: OutType -> LintM LintedKind
-- The returned Kind has itself been linted

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintType (TyVarTy tv)
  = do { checkL (isTyVar tv) (mkBadTyVarMsg tv)
       ; lintTyCoVarInScope tv
       ; return (tyVarKind tv) }
         -- We checked its kind when we added it to the envt

lintType ty@(AppTy t1 t2)
  | TyConApp {} <- t1
  = failWithL $ text "TyConApp to the left of AppTy:" <+> ppr ty
  | otherwise
  = do { k1 <- lintType t1
       ; k2 <- lintType t2
       ; lint_ty_app ty k1 [(t2,k2)] }

lintType ty@(TyConApp tc tys)
  | Just ty' <- coreView ty
  = lintType ty'   -- Expand type synonyms, so that we do not bogusly complain
                   --  about un-saturated type synonyms

  | isUnliftedTyCon tc || isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
       -- Also type synonyms and type families
  , length tys < tyConArity tc
  = failWithL (hang (text "Un-saturated type application") 2 (ppr ty))

  | otherwise
  = do { checkTyCon tc
       ; ks <- mapM lintType tys
       ; lint_ty_app ty (tyConKind tc) (tys `zip` ks) }

-- arrows can related *unlifted* kinds, so this has to be separate from
-- a dependent forall.
lintType ty@(FunTy t1 t2)
  = do { k1 <- lintType t1
       ; k2 <- lintType t2
       ; lintArrow (text "type or kind" <+> quotes (ppr ty)) k1 k2 }

lintType t@(ForAllTy (TvBndr tv _vis) ty)
  = do { lintL (isTyVar tv) (text "Covar bound in type:" <+> ppr t)
       ; lintTyBndr tv $ \tv' ->
          do { k <- lintType ty
             ; lintL (not (tv' `elemVarSet` tyCoVarsOfType k))
                     (text "Variable escape in forall:" <+> ppr t)
             ; lintL (classifiesTypeWithValues k)
                     (text "Non-* and non-# kind in forall:" <+> ppr t)
             ; return k }}

lintType ty@(LitTy l) = lintTyLit l >> return (typeKind ty)

lintType (CastTy ty co)
  = do { k1 <- lintType ty
       ; (k1', k2) <- lintStarCoercion co
       ; ensureEqTys k1 k1' (mkCastErr ty co k1' k1)
       ; return k2 }

lintType (CoercionTy co)
  = do { (k1, k2, ty1, ty2, r) <- lintCoercion co
       ; return $ mkHeteroCoercionType r k1 k2 ty1 ty2 }

lintKind :: OutKind -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintKind k = do { sk <- lintType k
                ; unless ((isStarKind sk) || (isUnliftedTypeKind sk))
                         (addErrL (hang (text "Ill-kinded kind:" <+> ppr k)
                                      2 (text "has kind:" <+> ppr sk))) }

-- confirms that a type is really *
lintStar :: SDoc -> OutKind -> LintM ()
lintStar doc k
  = lintL (classifiesTypeWithValues k)
          (text "Non-*-like kind when *-like expected:" <+> ppr k $$
           text "when checking" <+> doc)

lintArrow :: SDoc -> LintedKind -> LintedKind -> LintM LintedKind
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintArrow what k1 k2   -- Eg lintArrow "type or kind `blah'" k1 k2
                       -- or lintarrow "coercion `blah'" k1 k2
  = do { unless (okArrowArgKind k1)    (addErrL (msg (text "argument") k1))
       ; unless (okArrowResultKind k2) (addErrL (msg (text "result")   k2))
       ; return liftedTypeKind }
  where
    msg ar k
      = vcat [ hang (text "Ill-kinded" <+> ar)
                  2 (text "in" <+> what)
             , what <+> text "kind:" <+> ppr k ]

lint_ty_app :: Type -> LintedKind -> [(LintedType,LintedKind)] -> LintM LintedKind
lint_ty_app ty k tys
  = lint_app (text "type" <+> quotes (ppr ty)) k tys

----------------
lint_co_app :: Coercion -> LintedKind -> [(LintedType,LintedKind)] -> LintM LintedKind
lint_co_app ty k tys
  = lint_app (text "coercion" <+> quotes (ppr ty)) k tys

----------------
lintTyLit :: TyLit -> LintM ()
lintTyLit (NumTyLit n)
  | n >= 0    = return ()
  | otherwise = failWithL msg
    where msg = text "Negative type literal:" <+> integer n
lintTyLit (StrTyLit _) = return ()

lint_app :: SDoc -> LintedKind -> [(LintedType,LintedKind)] -> LintM Kind
-- (lint_app d fun_kind arg_tys)
--    We have an application (f arg_ty1 .. arg_tyn),
--    where f :: fun_kind
-- Takes care of linting the OutTypes

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lint_app doc kfn kas
    = do { in_scope <- getInScope
         -- We need the in_scope set to satisfy the invariant in
         -- Note [The substitution invariant] in TyCoRep
         ; foldlM (go_app in_scope) kfn kas }
  where
    fail_msg = vcat [ hang (text "Kind application error in") 2 doc
                    , nest 2 (text "Function kind =" <+> ppr kfn)
                    , nest 2 (text "Arg kinds =" <+> ppr kas) ]

    go_app in_scope kfn ka
      | Just kfn' <- coreView kfn
      = go_app in_scope kfn' ka

    go_app _ (FunTy kfa kfb) (_,ka)
      = do { unless (ka `eqType` kfa) (addErrL fail_msg)
           ; return kfb }

    go_app in_scope (ForAllTy (TvBndr kv _vis) kfn) (ta,ka)
      = do { unless (ka `eqType` tyVarKind kv) (addErrL fail_msg)
           ; return (substTyWithInScope in_scope [kv] [ta] kfn) }

    go_app _ _ _ = failWithL fail_msg

{- *********************************************************************
*                                                                      *
        Linting rules
*                                                                      *
********************************************************************* -}

lintCoreRule :: OutType -> CoreRule -> LintM ()
lintCoreRule _ (BuiltinRule {})
  = return ()  -- Don't bother

lintCoreRule fun_ty (Rule { ru_name = name, ru_bndrs = bndrs
                          , ru_args = args, ru_rhs = rhs })
  = lintBinders bndrs $ \ _ ->
    do { lhs_ty <- foldM lintCoreArg fun_ty args
       ; rhs_ty <- lintCoreExpr rhs
       ; ensureEqTys lhs_ty rhs_ty $
         (rule_doc <+> vcat [ text "lhs type:" <+> ppr lhs_ty
                            , text "rhs type:" <+> ppr rhs_ty ])
       ; let bad_bndrs = filterOut (`elemVarSet` exprsFreeVars args) $
                         filter (`elemVarSet` exprFreeVars rhs) $
                         bndrs

       ; checkL (null bad_bndrs)
                (rule_doc <+> text "unbound" <+> ppr bad_bndrs)
            -- See Note [Linting rules]
    }
  where
    rule_doc = text "Rule" <+> doubleQuotes (ftext name) <> colon

{- Note [Linting rules]
~~~~~~~~~~~~~~~~~~~~~~~
It's very bad if simplifying a rule means that one of the template
variables (ru_bndrs) that /is/ mentioned on the RHS becomes
not-mentioned in the LHS (ru_args).  How can that happen?  Well, in
Trac #10602, SpecConstr stupidly constructed a rule like

  forall x,c1,c2.
     f (x |> c1 |> c2) = ....

But simplExpr collapses those coercions into one.  (Indeed in
Trac #10602, it collapsed to the identity and was removed altogether.)

We don't have a great story for what to do here, but at least
this check will nail it.

NB (Trac #11643): it's possible that a variable listed in the
binders becomes not-mentioned on both LHS and RHS.  Here's a silly
example:
   RULE forall x y. f (g x y) = g (x+1 (y-1)
And suppose worker/wrapper decides that 'x' is Absent.  Then
we'll end up with
   RULE forall x y. f ($gw y) = $gw (x+1)
This seems sufficiently obscure that there isn't enough payoff to
try to trim the forall'd binder list.
-}

{-
************************************************************************
*                                                                      *
         Linting coercions
*                                                                      *
************************************************************************
-}

lintInCo :: InCoercion -> LintM (LintedKind, LintedKind, LintedType, LintedType, Role)
-- Check the coercion, and apply the substitution to it
-- See Note [Linting type lets]
lintInCo co
  = addLoc (InCo co) $
    do  { co' <- applySubstCo co
        ; lintCoercion co' }

-- lints a coercion, confirming that its lh kind and its rh kind are both *
-- also ensures that the role is Nominal
lintStarCoercion :: OutCoercion -> LintM (LintedType, LintedType)
lintStarCoercion g
  = do { (k1, k2, t1, t2, r) <- lintCoercion g
       ; lintStar (text "the kind of the left type in" <+> ppr g) k1
       ; lintStar (text "the kind of the right type in" <+> ppr g) k2
       ; lintRole g Nominal r
       ; return (t1, t2) }

lintCoercion :: OutCoercion -> LintM (LintedKind, LintedKind, LintedType, LintedType, Role)
-- Check the kind of a coercion term, returning the kind
-- Post-condition: the returned OutTypes are lint-free
--
-- If   lintCoercion co = (k1, k2, s1, s2, r)
-- then co :: s1 ~r s2
--      s1 :: k2
--      s2 :: k2

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoercion (Refl r ty)
  = do { k <- lintType ty
       ; return (k, k, ty, ty, r) }

lintCoercion co@(TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [co1,co2] <- cos
  = do { (k1,k'1,s1,t1,r1) <- lintCoercion co1
       ; (k2,k'2,s2,t2,r2) <- lintCoercion co2
       ; k <- lintArrow (text "coercion" <+> quotes (ppr co)) k1 k2
       ; k' <- lintArrow (text "coercion" <+> quotes (ppr co)) k'1 k'2
       ; lintRole co1 r r1
       ; lintRole co2 r r2
       ; return (k, k', mkFunTy s1 s2, mkFunTy t1 t2, r) }

  | Just {} <- synTyConDefn_maybe tc
  = failWithL (text "Synonym in TyConAppCo:" <+> ppr co)

  | otherwise
  = do { checkTyCon tc
       ; (k's, ks, ss, ts, rs) <- mapAndUnzip5M lintCoercion cos
       ; k' <- lint_co_app co (tyConKind tc) (ss `zip` k's)
       ; k <- lint_co_app co (tyConKind tc) (ts `zip` ks)
       ; _ <- zipWith3M lintRole cos (tyConRolesX r tc) rs
       ; return (k', k, mkTyConApp tc ss, mkTyConApp tc ts, r) }

lintCoercion co@(AppCo co1 co2)
  | TyConAppCo {} <- co1
  = failWithL (text "TyConAppCo to the left of AppCo:" <+> ppr co)
  | Refl _ (TyConApp {}) <- co1
  = failWithL (text "Refl (TyConApp ...) to the left of AppCo:" <+> ppr co)
  | otherwise
  = do { (k1,  k2,  s1, s2, r1) <- lintCoercion co1
       ; (k'1, k'2, t1, t2, r2) <- lintCoercion co2
       ; k3 <- lint_co_app co k1 [(t1,k'1)]
       ; k4 <- lint_co_app co k2 [(t2,k'2)]
       ; if r1 == Phantom
         then lintL (r2 == Phantom || r2 == Nominal)
                     (text "Second argument in AppCo cannot be R:" $$
                      ppr co)
         else lintRole co Nominal r2
       ; return (k3, k4, mkAppTy s1 t1, mkAppTy s2 t2, r1) }

----------
lintCoercion (ForAllCo tv1 kind_co co)
  = do { (_, k2) <- lintStarCoercion kind_co
       ; let tv2 = setTyVarKind tv1 k2
       ; addInScopeVar tv1 $
    do {
       ; (k3, k4, t1, t2, r) <- lintCoercion co
       ; in_scope <- getInScope
       ; let tyl = mkInvForAllTy tv1 t1
             subst = mkTvSubst in_scope $
                     -- We need both the free vars of the `t2` and the
                     -- free vars of the range of the substitution in
                     -- scope. All the free vars of `t2` and `kind_co` should
                     -- already be in `in_scope`, because they've been
                     -- linted and `tv2` has the same unique as `tv1`.
                     -- See Note [The substitution invariant]
                     unitVarEnv tv1 (TyVarTy tv2 `mkCastTy` mkSymCo kind_co)
             tyr = mkInvForAllTy tv2 $
                   substTy subst t2
       ; return (k3, k4, tyl, tyr, r) } }

lintCoercion (CoVarCo cv)
  | not (isCoVar cv)
  = failWithL (hang (text "Bad CoVarCo:" <+> ppr cv)
                  2 (text "With offending type:" <+> ppr (varType cv)))
  | otherwise
  = do { lintTyCoVarInScope cv
       ; cv' <- lookupIdInScope cv
       ; lintUnliftedCoVar cv
       ; return $ coVarKindsTypesRole cv' }

-- See Note [Bad unsafe coercion]
lintCoercion co@(UnivCo prov r ty1 ty2)
  = do { k1 <- lintType ty1
       ; k2 <- lintType ty2
       ; case prov of
           UnsafeCoerceProv -> return ()  -- no extra checks

           PhantomProv kco    -> do { lintRole co Phantom r
                                    ; check_kinds kco k1 k2 }

           ProofIrrelProv kco -> do { lintL (isCoercionTy ty1) $
                                          mkBadProofIrrelMsg ty1 co
                                    ; lintL (isCoercionTy ty2) $
                                          mkBadProofIrrelMsg ty2 co
                                    ; check_kinds kco k1 k2 }

           PluginProv _     -> return ()  -- no extra checks
           HoleProv h       -> addErrL $
                               text "Unfilled coercion hole:" <+> ppr h

       ; when (r /= Phantom && classifiesTypeWithValues k1
                            && classifiesTypeWithValues k2)
              (checkTypes ty1 ty2)
       ; return (k1, k2, ty1, ty2, r) }
   where
     report s = hang (text $ "Unsafe coercion between " ++ s)
                     2 (vcat [ text "From:" <+> ppr ty1
                             , text "  To:" <+> ppr ty2])
     isUnBoxed :: PrimRep -> Bool
     isUnBoxed PtrRep = False
     isUnBoxed _      = True
     checkTypes t1 t2
       = case (repType t1, repType t2) of
           (UnaryRep _, UnaryRep _) ->
              validateCoercion (typePrimRep t1) (typePrimRep t2)
           (MultiRep rep1, MultiRep rep2) ->
              checkWarnL (rep1 == rep2) (report "multi values with different reps")
           _  -> addWarnL (report "multi rep and unary rep")
     validateCoercion :: PrimRep -> PrimRep -> LintM ()
     validateCoercion rep1 rep2
       = do { dflags <- getDynFlags
            ; checkWarnL (isUnBoxed rep1 == isUnBoxed rep2)
                         (report "unboxed and boxed value")
            ; checkWarnL (TyCon.primRepSizeW dflags rep1
                           == TyCon.primRepSizeW dflags rep2)
                         (report "unboxed values of different size")
            ; let fl = liftM2 (==) (TyCon.primRepIsFloat rep1)
                                   (TyCon.primRepIsFloat rep2)
            ; case fl of
                Nothing    -> addWarnL (report "vector types")
                Just False -> addWarnL (report "float and integral values")
                _          -> return ()
            }

     check_kinds kco k1 k2 = do { (k1', k2') <- lintStarCoercion kco
                                ; ensureEqTys k1 k1' (mkBadUnivCoMsg CLeft  co)
                                ; ensureEqTys k2 k2' (mkBadUnivCoMsg CRight co) }


lintCoercion (SymCo co)
  = do { (k1, k2, ty1, ty2, r) <- lintCoercion co
       ; return (k2, k1, ty2, ty1, r) }

lintCoercion co@(TransCo co1 co2)
  = do { (k1a, _k1b, ty1a, ty1b, r1) <- lintCoercion co1
       ; (_k2a, k2b, ty2a, ty2b, r2) <- lintCoercion co2
       ; ensureEqTys ty1b ty2a
               (hang (text "Trans coercion mis-match:" <+> ppr co)
                   2 (vcat [ppr ty1a, ppr ty1b, ppr ty2a, ppr ty2b]))
       ; lintRole co r1 r2
       ; return (k1a, k2b, ty1a, ty2b, r1) }

lintCoercion the_co@(NthCo n co)
  = do { (_, _, s, t, r) <- lintCoercion co
       ; case (splitForAllTy_maybe s, splitForAllTy_maybe t) of
         { (Just (tv_s, _ty_s), Just (tv_t, _ty_t))
             |  n == 0
             -> return (ks, kt, ts, tt, Nominal)
             where
               ts = tyVarKind tv_s
               tt = tyVarKind tv_t
               ks = typeKind ts
               kt = typeKind tt

         ; _ -> case (splitTyConApp_maybe s, splitTyConApp_maybe t) of
         { (Just (tc_s, tys_s), Just (tc_t, tys_t))
             | tc_s == tc_t
             , isInjectiveTyCon tc_s r
                 -- see Note [NthCo and newtypes] in TyCoRep
             , tys_s `equalLength` tys_t
             , n < length tys_s
             -> return (ks, kt, ts, tt, tr)
             where
               ts = getNth tys_s n
               tt = getNth tys_t n
               tr = nthRole r tc_s n
               ks = typeKind ts
               kt = typeKind tt

         ; _ -> failWithL (hang (text "Bad getNth:")
                              2 (ppr the_co $$ ppr s $$ ppr t)) }}}

lintCoercion the_co@(LRCo lr co)
  = do { (_,_,s,t,r) <- lintCoercion co
       ; lintRole co Nominal r
       ; case (splitAppTy_maybe s, splitAppTy_maybe t) of
           (Just s_pr, Just t_pr)
             -> return (ks_pick, kt_pick, s_pick, t_pick, Nominal)
             where
               s_pick  = pickLR lr s_pr
               t_pick  = pickLR lr t_pr
               ks_pick = typeKind s_pick
               kt_pick = typeKind t_pick

           _ -> failWithL (hang (text "Bad LRCo:")
                              2 (ppr the_co $$ ppr s $$ ppr t)) }

lintCoercion (InstCo co arg)
  = do { (k3, k4, t1',t2', r) <- lintCoercion co
       ; (k1',k2',s1,s2, r') <- lintCoercion arg
       ; lintRole arg Nominal r'
       ; case (splitForAllTy_maybe t1', splitForAllTy_maybe t2') of
          (Just (tv1,t1), Just (tv2,t2))
            | k1' `eqType` tyVarKind tv1
            , k2' `eqType` tyVarKind tv2
            -> return (k3, k4,
                       substTyWith [tv1] [s1] t1,
                       substTyWith [tv2] [s2] t2, r)
            | otherwise
            -> failWithL (text "Kind mis-match in inst coercion")
          _ -> failWithL (text "Bad argument of inst") }

lintCoercion co@(AxiomInstCo con ind cos)
  = do { unless (0 <= ind && ind < numBranches (coAxiomBranches con))
                (bad_ax (text "index out of range"))
       ; let CoAxBranch { cab_tvs   = ktvs
                        , cab_cvs   = cvs
                        , cab_roles = roles
                        , cab_lhs   = lhs
                        , cab_rhs   = rhs } = coAxiomNthBranch con ind
       ; unless (length ktvs + length cvs == length cos) $
           bad_ax (text "lengths")
       ; subst <- getTCvSubst
       ; let empty_subst = zapTCvSubst subst
       ; (subst_l, subst_r) <- foldlM check_ki
                                      (empty_subst, empty_subst)
                                      (zip3 (ktvs ++ cvs) roles cos)
       ; let lhs' = substTys subst_l lhs
             rhs' = substTy  subst_r rhs
       ; case checkAxInstCo co of
           Just bad_branch -> bad_ax $ text "inconsistent with" <+>
                                       pprCoAxBranch con bad_branch
           Nothing -> return ()
       ; let s2 = mkTyConApp (coAxiomTyCon con) lhs'
       ; return (typeKind s2, typeKind rhs', s2, rhs', coAxiomRole con) }
  where
    bad_ax what = addErrL (hang (text  "Bad axiom application" <+> parens what)
                        2 (ppr co))

    check_ki (subst_l, subst_r) (ktv, role, arg)
      = do { (k', k'', s', t', r) <- lintCoercion arg
           ; lintRole arg role r
           ; let ktv_kind_l = substTy subst_l (tyVarKind ktv)
                 ktv_kind_r = substTy subst_r (tyVarKind ktv)
           ; unless (k' `eqType` ktv_kind_l)
                    (bad_ax (text "check_ki1" <+> vcat [ ppr co, ppr k', ppr ktv, ppr ktv_kind_l ] ))
           ; unless (k'' `eqType` ktv_kind_r)
                    (bad_ax (text "check_ki2" <+> vcat [ ppr co, ppr k'', ppr ktv, ppr ktv_kind_r ] ))
           ; return (extendTCvSubst subst_l ktv s',
                     extendTCvSubst subst_r ktv t') }

lintCoercion (CoherenceCo co1 co2)
  = do { (_, k2, t1, t2, r) <- lintCoercion co1
       ; let lhsty = mkCastTy t1 co2
       ; k1' <- lintType lhsty
       ; return (k1', k2, lhsty, t2, r) }

lintCoercion (KindCo co)
  = do { (k1, k2, _, _, _) <- lintCoercion co
       ; return (liftedTypeKind, liftedTypeKind, k1, k2, Nominal) }

lintCoercion (SubCo co')
  = do { (k1,k2,s,t,r) <- lintCoercion co'
       ; lintRole co' Nominal r
       ; return (k1,k2,s,t,Representational) }

lintCoercion this@(AxiomRuleCo co cs)
  = do { eqs <- mapM lintCoercion cs
       ; lintRoles 0 (coaxrAsmpRoles co) eqs
       ; case coaxrProves co [ Pair l r | (_,_,l,r,_) <- eqs ] of
           Nothing -> err "Malformed use of AxiomRuleCo" [ ppr this ]
           Just (Pair l r) ->
             return (typeKind l, typeKind r, l, r, coaxrRole co) }
  where
  err m xs  = failWithL $
                hang (text m) 2 $ vcat (text "Rule:" <+> ppr (coaxrName co) : xs)

  lintRoles n (e : es) ((_,_,_,_,r) : rs)
    | e == r    = lintRoles (n+1) es rs
    | otherwise = err "Argument roles mismatch"
                      [ text "In argument:" <+> int (n+1)
                      , text "Expected:" <+> ppr e
                      , text "Found:" <+> ppr r ]
  lintRoles _ [] []  = return ()
  lintRoles n [] rs  = err "Too many coercion arguments"
                          [ text "Expected:" <+> int n
                          , text "Provided:" <+> int (n + length rs) ]

  lintRoles n es []  = err "Not enough coercion arguments"
                          [ text "Expected:" <+> int (n + length es)
                          , text "Provided:" <+> int n ]

----------
lintUnliftedCoVar :: CoVar -> LintM ()
lintUnliftedCoVar cv
  = when (not (isUnliftedType (coVarKind cv))) $
    failWithL (text "Bad lifted equality:" <+> ppr cv
                 <+> dcolon <+> ppr (coVarKind cv))

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
       , le_subst :: TCvSubst        -- Current type substitution; we also use this
                                     -- to keep track of all the variables in scope,
                                     -- both Ids and TyVars
       , le_dynflags :: DynFlags     -- DynamicFlags
       }

data LintFlags
  = LF { lf_check_global_ids           :: Bool -- See Note [Checking for global Ids]
       , lf_check_inline_loop_breakers :: Bool -- See Note [Checking for INLINE loop breakers]
       , lf_check_static_ptrs          :: Bool -- See Note [Checking StaticPtrs]
    }

defaultLintFlags :: LintFlags
defaultLintFlags = LF { lf_check_global_ids = False
                      , lf_check_inline_loop_breakers = True
                      , lf_check_static_ptrs = False
                      }

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

Note [Checking StaticPtrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See SimplCore Note [Grand plan for static forms] for an overview.

Every occurrence of the data constructor @StaticPtr@ should be moved
to the top level by the FloatOut pass.  It's vital that we don't have
nested StaticPtr uses after CorePrep, because we populate the Static
Pointer Table from the top-level bindings. See SimplCore Note [Grand
plan for static forms].

The linter checks that no occurrence is left behind, nested within an
expression. The check is enabled only:

* After the FloatOut, CorePrep, and CoreTidy passes.
  We could check more often, but the condition doesn't hold until
  after the first FloatOut pass.

* When the module uses the StaticPointers language extension. This is
  a little hack.  This optimization arose from the need to compile
  GHC.StaticPtr, which otherwise would be rejected because of the
  following binding for the StaticPtr data constructor itself:

    StaticPtr = \a b1 b2 b3 b4 -> StaticPtr a b1 b2 b3 b4

  which contains an application of `StaticPtr` nested within the
  lambda abstractions.  This binding is injected by CorePrep.

  Note that GHC.StaticPtr is itself compiled without -XStaticPointers.

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
      pure x = LintM $ \ _ errs -> (Just x, errs)
      (<*>) = ap

instance Monad LintM where
  fail err = failWithL (text err)
  m >>= k  = LintM (\ env errs ->
                       let (res, errs') = unLintM m env errs in
                         case res of
                           Just r -> unLintM (k r) env errs'
                           Nothing -> (Nothing, errs'))

#if __GLASGOW_HASKELL__ > 710
instance MonadFail.MonadFail LintM where
    fail err = failWithL (text err)
#endif

instance HasDynFlags LintM where
  getDynFlags = LintM (\ e errs -> (Just (le_dynflags e), errs))

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

initL :: DynFlags -> LintFlags -> LintM a -> WarnsAndErrs    -- Errors and warnings
initL dflags flags m
  = case unLintM m env (emptyBag, emptyBag) of
      (_, errs) -> errs
  where
    env = LE { le_flags = flags, le_subst = emptyTCvSubst, le_loc = [], le_dynflags = dflags }

getLintFlags :: LintM LintFlags
getLintFlags = LintM $ \ env errs -> (Just (le_flags env), errs)

checkL :: Bool -> MsgDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = failWithL msg

-- like checkL, but relevant to type checking
lintL :: Bool -> MsgDoc -> LintM ()
lintL = checkL

checkWarnL :: Bool -> MsgDoc -> LintM ()
checkWarnL True   _  = return ()
checkWarnL False msg = addWarnL msg

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
                                      text "Substitution:" <+> ppr (le_subst env)
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
    unLintM m (env { le_subst = extendTCvInScopeList (le_subst env) vars })
              errs

addInScopeVarSet :: VarSet -> LintM a -> LintM a
addInScopeVarSet vars m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = extendTCvInScopeSet (le_subst env) vars })
              errs

addInScopeVar :: Var -> LintM a -> LintM a
addInScopeVar var m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = extendTCvInScope (le_subst env) var }) errs

extendSubstL :: TyVar -> Type -> LintM a -> LintM a
extendSubstL tv ty m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = Type.extendTvSubst (le_subst env) tv ty }) errs

updateTCvSubst :: TCvSubst -> LintM a -> LintM a
updateTCvSubst subst' m
  = LintM $ \ env errs -> unLintM m (env { le_subst = subst' }) errs

getTCvSubst :: LintM TCvSubst
getTCvSubst = LintM (\ env errs -> (Just (le_subst env), errs))

getInScope :: LintM InScopeSet
getInScope = LintM (\ env errs -> (Just (getTCvInScope $ le_subst env), errs))

applySubstTy :: InType -> LintM OutType
applySubstTy ty = do { subst <- getTCvSubst; return (substTy subst ty) }

applySubstCo :: InCoercion -> LintM OutCoercion
applySubstCo co = do { subst <- getTCvSubst; return (substCo subst co) }

lookupIdInScope :: Id -> LintM Id
lookupIdInScope id
  | not (mustHaveLocalBinding id)
  = return id   -- An imported Id
  | otherwise
  = do  { subst <- getTCvSubst
        ; case lookupInScope (getTCvInScope subst) id of
                Just v  -> return v
                Nothing -> do { addErrL out_of_scope
                              ; return id } }
  where
    out_of_scope = pprBndr LetBind id <+> text "is out of scope"

lintTyCoVarInScope :: Var -> LintM ()
lintTyCoVarInScope v = lintInScope (text "is out of scope") v

lintInScope :: SDoc -> Var -> LintM ()
lintInScope loc_msg var =
 do { subst <- getTCvSubst
    ; lintL (not (mustHaveLocalBinding var) || (var `isInScope` subst))
             (hsep [pprBndr LetBind var, loc_msg]) }

ensureEqTys :: OutType -> OutType -> MsgDoc -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have alrady had the substitution applied
ensureEqTys ty1 ty2 msg = lintL (ty1 `eqType` ty2) msg

lintRole :: Outputable thing
          => thing     -- where the role appeared
          -> Role      -- expected
          -> Role      -- actual
          -> LintM ()
lintRole co r1 r2
  = lintL (r1 == r2)
          (text "Role incompatibility: expected" <+> ppr r1 <> comma <+>
           text "got" <+> ppr r2 $$
           text "in" <+> ppr co)

{-
************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

dumpLoc :: LintLocInfo -> (SrcLoc, SDoc)

dumpLoc (RhsOf v)
  = (getSrcLoc v, brackets (text "RHS of" <+> pp_binders [v]))

dumpLoc (LambdaBodyOf b)
  = (getSrcLoc b, brackets (text "in body of lambda with binder" <+> pp_binder b))

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, brackets (text "In body of a letrec with no binders"))

dumpLoc (BodyOfLetRec bs@(_:_))
  = ( getSrcLoc (head bs), brackets (text "in body of letrec with binders" <+> pp_binders bs))

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, _))
  = (noSrcLoc, text "In a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (CasePat (con, args, _))
  = (noSrcLoc, text "In the pattern of a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (ImportedUnfolding locn)
  = (locn, brackets (text "in an imported unfolding"))
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
         4 (vcat [ text "Actual type:" <+> ppr ty1,
                   text "Annotation on case:" <+> ppr ty2,
                   text "Alt Rhs:" <+> ppr e ])

mkScrutMsg :: Id -> Type -> Type -> TCvSubst -> MsgDoc
mkScrutMsg var var_ty scrut_ty subst
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
          text "Result binder type:" <+> ppr var_ty,--(idType var),
          text "Scrutinee type:" <+> ppr scrut_ty,
     hsep [text "Current TCv subst", ppr subst]]

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
  = vcat [text "Argument value doesn't match argument type:",
              hang (text "Fun type:") 4 (ppr fun_ty),
              hang (text "Arg type:") 4 (ppr arg_ty),
              hang (text "Arg:") 4 (ppr arg)]

mkNonFunAppMsg :: Type -> Type -> CoreExpr -> MsgDoc
mkNonFunAppMsg fun_ty arg_ty arg
  = vcat [text "Non-function type in function position",
              hang (text "Fun type:") 4 (ppr fun_ty),
              hang (text "Arg type:") 4 (ppr arg_ty),
              hang (text "Arg:") 4 (ppr arg)]

mkLetErr :: TyVar -> CoreExpr -> MsgDoc
mkLetErr bndr rhs
  = vcat [text "Bad `let' binding:",
          hang (text "Variable:")
                 4 (ppr bndr <+> dcolon <+> ppr (varType bndr)),
          hang (text "Rhs:")
                 4 (ppr rhs)]

mkTyAppMsg :: Type -> Type -> MsgDoc
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
              hang (text "Exp type:")
                 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
              hang (text "Arg type:")
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkRhsMsg :: Id -> SDoc -> Type -> MsgDoc
mkRhsMsg binder what ty
  = vcat
    [hsep [text "The type of this binder doesn't match the type of its" <+> what <> colon,
            ppr binder],
     hsep [text "Binder's type:", ppr (idType binder)],
     hsep [text "Rhs type:", ppr ty]]

mkLetAppMsg :: CoreExpr -> MsgDoc
mkLetAppMsg e
  = hang (text "This argument does not satisfy the let/app invariant:")
       2 (ppr e)

mkRhsPrimMsg :: Id -> CoreExpr -> MsgDoc
mkRhsPrimMsg binder _rhs
  = vcat [hsep [text "The type of this binder is primitive:",
                     ppr binder],
              hsep [text "Binder's type:", ppr (idType binder)]
             ]

mkStrictMsg :: Id -> MsgDoc
mkStrictMsg binder
  = vcat [hsep [text "Recursive or top-level binder has strict demand info:",
                     ppr binder],
              hsep [text "Binder's demand info:", ppr (idDemandInfo binder)]
             ]

mkNonTopExportedMsg :: Id -> MsgDoc
mkNonTopExportedMsg binder
  = hsep [text "Non-top-level binder is marked as exported:", ppr binder]

mkNonTopExternalNameMsg :: Id -> MsgDoc
mkNonTopExternalNameMsg binder
  = hsep [text "Non-top-level binder has an external name:", ppr binder]

mkKindErrMsg :: TyVar -> Type -> MsgDoc
mkKindErrMsg tyvar arg_ty
  = vcat [text "Kinds don't match in type application:",
          hang (text "Type variable:")
                 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
          hang (text "Arg type:")
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

{- Not needed now
mkArityMsg :: Id -> MsgDoc
mkArityMsg binder
  = vcat [hsep [text "Demand type has",
                ppr (dmdTypeDepth dmd_ty),
                text "arguments, rhs has",
                ppr (idArity binder),
                text "arguments,",
                ppr binder],
              hsep [text "Binder's strictness signature:", ppr dmd_ty]

         ]
           where (StrictSig dmd_ty) = idStrictness binder
-}
mkCastErr :: Outputable casted => casted -> Coercion -> Type -> Type -> MsgDoc
mkCastErr expr co from_ty expr_ty
  = vcat [text "From-type of Cast differs from type of enclosed expression",
          text "From-type:" <+> ppr from_ty,
          text "Type of enclosed expr:" <+> ppr expr_ty,
          text "Actual enclosed expr:" <+> ppr expr,
          text "Coercion used in cast:" <+> ppr co
         ]

mkBadUnivCoMsg :: LeftOrRight -> Coercion -> SDoc
mkBadUnivCoMsg lr co
  = text "Kind mismatch on the" <+> pprLeftOrRight lr <+>
    text "side of a UnivCo:" <+> ppr co

mkBadProofIrrelMsg :: Type -> Coercion -> SDoc
mkBadProofIrrelMsg ty co
  = hang (text "Found a non-coercion in a proof-irrelevance UnivCo:")
       2 (vcat [ text "type:" <+> ppr ty
               , text "co:" <+> ppr co ])

mkBadTyVarMsg :: Var -> SDoc
mkBadTyVarMsg tv
  = text "Non-tyvar used in TyVarTy:"
      <+> ppr tv <+> dcolon <+> ppr (varType tv)

pprLeftOrRight :: LeftOrRight -> MsgDoc
pprLeftOrRight CLeft  = text "left"
pprLeftOrRight CRight = text "right"

dupVars :: [[Var]] -> MsgDoc
dupVars vars
  = hang (text "Duplicate variables brought into scope")
       2 (ppr vars)

dupExtVars :: [[Name]] -> MsgDoc
dupExtVars vars
  = hang (text "Duplicate top-level variables with the same qualified name")
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
-- set the debugLevel setting to 0 in the environment as well as all
-- annotations from incoming modules.
withoutAnnots :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
withoutAnnots pass guts = do
  -- Remove debug flag from environment.
  dflags <- getDynFlags
  let removeFlag env = env{ hsc_dflags = dflags{ debugLevel = 0} }
      withoutFlag corem =
        liftIO =<< runCoreM <$> fmap removeFlag getHscEnv <*> getRuleBase <*>
                                getUniqueSupplyM <*> getModule <*>
                                getVisibleOrphanMods <*>
                                getPrintUnqualified <*> getSrcSpanM <*>
                                pure corem
  -- Nuke existing ticks in module.
  -- TODO: Ticks in unfoldings. Maybe change unfolding so it removes
  -- them in absence of debugLevel > 0.
  let nukeTicks = stripTicksE (not . tickishIsCode)
      nukeAnnotsBind :: CoreBind -> CoreBind
      nukeAnnotsBind bind = case bind of
        Rec bs     -> Rec $ map (\(b,e) -> (b, nukeTicks e)) bs
        NonRec b e -> NonRec b $ nukeTicks e
      nukeAnnotsMod mg@ModGuts{mg_binds=binds}
        = mg{mg_binds = map nukeAnnotsBind binds}
  -- Perform pass with all changes applied
  fmap fst $ withoutFlag $ pass (nukeAnnotsMod guts)
