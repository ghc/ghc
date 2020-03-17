{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


A ``lint'' pass to check for Core correctness.
See Note [Core Lint guarantee].
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}

module GHC.Core.Lint (
    lintCoreBindings, lintUnfolding,
    lintPassResult, lintInteractiveExpr, lintExpr,
    lintAnnots, lintTypes,

    -- ** Debug output
    endPass, endPassIO,
    dumpPassResult,
    GHC.Core.Lint.dumpIfSet,
 ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.Stats ( coreBindsStats )
import GHC.Core.Op.Monad
import Bag
import GHC.Types.Literal
import GHC.Core.DataCon
import TysPrim
import TcType ( isFloatingTy )
import GHC.Types.Var as Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.Set( nonDetEltsUniqSet )
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.Ppr
import ErrUtils
import GHC.Core.Coercion
import GHC.Types.SrcLoc
import GHC.Core.Type as Type
import GHC.Types.RepType
import GHC.Core.TyCo.Rep   -- checks validity of types/coercions
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr ( pprTyVar )
import GHC.Core.TyCon as TyCon
import GHC.Core.Coercion.Axiom
import GHC.Types.Basic
import ErrUtils as Err
import ListSetOps
import PrelNames
import Outputable
import FastString
import Util
import GHC.Core.InstEnv      ( instanceDFunId )
import GHC.Core.Coercion.Opt ( checkAxInstCo )
import GHC.Core.Arity        ( typeArity )
import GHC.Types.Demand ( splitStrictSig, isBotDiv )

import GHC.Driver.Types
import GHC.Driver.Session
import Control.Monad
import MonadUtils
import Data.Foldable      ( toList )
import Data.List.NonEmpty ( NonEmpty )
import Data.List          ( partition )
import Data.Maybe
import Pair
import qualified GHC.LanguageExtensions as LangExt

{-
Note [Core Lint guarantee]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Core Lint is the type-checker for Core. Using it, we get the following guarantee:

If all of:
1. Core Lint passes,
2. there are no unsafe coercions (i.e. unsafeEqualityProof),
3. all plugin-supplied coercions (i.e. PluginProv) are valid, and
4. all case-matches are complete
then running the compiled program will not seg-fault, assuming no bugs downstream
(e.g. in the code generator). This guarantee is quite powerful, in that it allows us
to decouple the safety of the resulting program from the type inference algorithm.

However, do note point (4) above. Core Lint does not check for incomplete case-matches;
see Note [Case expression invariants] in GHC.Core, invariant (4). As explained there,
an incomplete case-match might slip by Core Lint and cause trouble at runtime.

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


Note [Linting function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Representation of function types], all saturated
applications of funTyCon are represented with the FunTy constructor. We check
this invariant in lintType.

Note [Linting type lets]
~~~~~~~~~~~~~~~~~~~~~~~~
In the desugarer, it's very very convenient to be able to say (in effect)
        let a = Type Bool in
        let x::a = True in <body>
That is, use a type let.   See Note [Type let] in CoreSyn.
One place it is used is in mkWwArgs; see Note [Join points and beta-redexes]
in GHC.Core.Op.WorkWrap.Lib.  (Maybe there are other "clients" of this feature; I'm not sure).

* Hence when linting <body> we need to remember that a=Int, else we
  might reject a correct program.  So we carry a type substitution (in
  this example [a -> Bool]) and apply this substitution before
  comparing types. In effect, in Lint, type equality is always
  equality-moduolo-le-subst.  This is in the le_subst field of
  LintEnv.  But nota bene:

  (SI1) The le_subst substitution is applied to types and coercions only

  (SI2) The result of that substitution is used only to check for type
        equality, to check well-typed-ness, /but is then discarded/.
        The result of substittion does not outlive the CoreLint pass.

  (SI3) The InScopeSet of le_subst includes only TyVar and CoVar binders.

* The function
        lintInTy :: Type -> LintM (Type, Kind)
  returns a substituted type.

* When we encounter a binder (like x::a) we must apply the substitution
  to the type of the binding variable.  lintBinders does this.

* Clearly we need to clone tyvar binders as we go.

* But take care (#17590)! We must also clone CoVar binders:
    let a = TYPE (ty |> cv)
    in \cv -> blah
  blindly substituting for `a` might capture `cv`.

* Alas, when cloning a coercion variable we might choose a unique
  that happens to clash with an inner Id, thus
      \cv_66 -> let wild_X7 = blah in blah
  We decide to clone `cv_66` becuase it's already in scope.  Fine,
  choose a new unique.  Aha, X7 looks good.  So we check the lambda
  body with le_subst of [cv_66 :-> cv_X7]

  This is all fine, even though we use the same unique as wild_X7.
  As (SI2) says, we do /not/ return a new lambda
     (\cv_X7 -> let wild_X7 = blah in ...)
  We simply use the le_subst subsitution in types/coercions only, when
  checking for equality.

* We still need to check that Id occurrences are bound by some
  enclosing binding.  We do /not/ use the InScopeSet for the le_subst
  for this purpose -- it contains only TyCoVars.  Instead we have a separate
  le_ids for the in-scope Id binders.

Sigh.  We might want to explore getting rid of type-let!

Note [Bad unsafe coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For discussion see https://gitlab.haskell.org/ghc/ghc/wikis/bad-unsafe-coercions
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

Note [Join points]
~~~~~~~~~~~~~~~~~~
We check the rules listed in Note [Invariants on join points] in GHC.Core. The
only one that causes any difficulty is the first: All occurrences must be tail
calls. To this end, along with the in-scope set, we remember in le_joins the
subset of in-scope Ids that are valid join ids. For example:

  join j x = ... in
  case e of
    A -> jump j y -- good
    B -> case (jump j z) of -- BAD
           C -> join h = jump j w in ... -- good
           D -> let x = jump j v in ... -- BAD

A join point remains valid in case branches, so when checking the A
branch, j is still valid. When we check the scrutinee of the inner
case, however, we set le_joins to empty, and catch the
error. Similarly, join points can occur free in RHSes of other join
points but not the RHSes of value bindings (thunks and functions).

************************************************************************
*                                                                      *
                 Beginning and ending passes
*                                                                      *
************************************************************************

These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a convenient place for them.  They print out stuff
before and after core passes, and do Core Lint when necessary.
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
  = do { forM_ mb_flag $ \flag -> do
           let sty = mkDumpStyle dflags unqual
           dumpAction dflags sty (dumpOptionsFromFlag flag)
              (showSDoc dflags hdr) FormatCore dump_doc

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
coreDumpFlag CoreDoExitify            = Just Opt_D_dump_exitify
coreDumpFlag CoreDoDemand             = Just Opt_D_dump_stranal
coreDumpFlag CoreDoCpr                = Just Opt_D_dump_cpranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds_preopt
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep
coreDumpFlag CoreOccurAnal            = Just Opt_D_dump_occur_anal

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
  = do { putLogMsg dflags NoReason Err.SevDump noSrcSpan
           (defaultDumpStyle dflags)
           (vcat [ lint_banner "errors" (ppr pass), Err.pprMessageBag errs
                 , text "*** Offending Program ***"
                 , pprCoreBindings binds
                 , text "*** End of Offense ***" ])
       ; Err.ghcExit dflags 1 }

  | not (isEmptyBag warns)
  , not (hasNoDebugOutput dflags)
  , showLintWarnings pass
  -- If the Core linter encounters an error, output to stderr instead of
  -- stdout (#13342)
  = putLogMsg dflags NoReason Err.SevInfo noSrcSpan
        (defaultDumpStyle dflags)
        (lint_banner "warnings" (ppr pass) $$ Err.pprMessageBag (mapBag ($$ blankLine) warns))

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
      = do { putLogMsg dflags NoReason Err.SevDump
               noSrcSpan (defaultDumpStyle dflags)
               (vcat [ lint_banner "errors" (text what)
                     , err
                     , text "*** Offending Program ***"
                     , pprCoreExpr expr
                     , text "*** End of Offense ***" ])
           ; Err.ghcExit dflags 1 }

interactiveInScope :: HscEnv -> [Var]
-- In GHCi we may lint expressions, or bindings arising from 'deriving'
-- clauses, that mention variables bound in the interactive context.
-- These are Local things (see Note [Interactively-bound Ids in GHCi] in GHC.Driver.Types).
-- So we have to tell Lint about them, lest it reports them as out of scope.
--
-- We do this by find local-named things that may appear free in interactive
-- context.  This function is pretty revolting and quite possibly not quite right.
-- When we are not in GHCi, the interactive context (hsc_IC hsc_env) is empty
-- so this is a (cheap) no-op.
--
-- See #8215 for an example
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

-- | Type-check a 'CoreProgram'. See Note [Core Lint guarantee].
lintCoreBindings :: DynFlags -> CoreToDo -> [Var] -> CoreProgram -> (Bag MsgDoc, Bag MsgDoc)
--   Returns (warnings, errors)
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreBindings dflags pass local_in_scope binds
  = initL dflags flags local_in_scope $
    addLoc TopLevelBindings           $
    do { checkL (null dups) (dupVars dups)
       ; checkL (null ext_dups) (dupExtVars ext_dups)
       ; lintRecBindings TopLevel all_pairs $
         return () }
  where
    all_pairs = flattenBinds binds
     -- Put all the top-level binders in scope at the start
     -- This is because transformation rules can bring something
     -- into use 'unexpectedly'; see Note [Glomming] in OccurAnal
    binders = map fst all_pairs

    flags = defaultLintFlags
               { lf_check_global_ids = check_globals
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
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = case pass of
                          CoreDoFloatOutwards _ -> AllowAtTopLevel
                          CoreTidy              -> RejectEverywhere
                          CorePrep              -> AllowAtTopLevel
                          _                     -> AllowAnywhere

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

{-
************************************************************************
*                                                                      *
\subsection[lintUnfolding]{lintUnfolding}
*                                                                      *
************************************************************************

Note [Linting Unfoldings from Interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We use this to check all top-level unfoldings that come in from interfaces
(it is very painful to catch errors otherwise).

We do not need to call lintUnfolding on unfoldings that are nested within
top-level unfoldings; they are linted when we lint the top-level unfolding;
hence the `TopLevelFlag` on `tcPragExpr` in GHC.IfaceToCore.

-}

lintUnfolding :: Bool           -- True <=> is a compulsory unfolding
              -> DynFlags
              -> SrcLoc
              -> VarSet         -- Treat these as in scope
              -> CoreExpr
              -> Maybe MsgDoc   -- Nothing => OK

lintUnfolding is_compulsory dflags locn var_set expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    vars = nonDetEltsUniqSet var_set
    (_warns, errs) = initL dflags defaultLintFlags vars $
                     if is_compulsory
                       -- See Note [Checking for levity polymorphism]
                     then noLPChecks linter
                     else linter
    linter = addLoc (ImportedUnfolding locn) $
             lintCoreExpr expr

lintExpr :: DynFlags
         -> [Var]               -- Treat these as in scope
         -> CoreExpr
         -> Maybe MsgDoc        -- Nothing => OK

lintExpr dflags vars expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL dflags defaultLintFlags vars linter
    linter = addLoc TopLevelBindings $
             lintCoreExpr expr

{-
************************************************************************
*                                                                      *
\subsection[lintCoreBinding]{lintCoreBinding}
*                                                                      *
************************************************************************

Check a core binding, returning the list of variables bound.
-}

lintRecBindings :: TopLevelFlag -> [(Id, CoreExpr)]
                -> LintM a -> LintM a
lintRecBindings top_lvl pairs thing_inside
  = lintIdBndrs top_lvl bndrs $ \ bndrs' ->
    do { zipWithM_ lint_pair bndrs' rhss
       ; thing_inside }
  where
    (bndrs, rhss) = unzip pairs
    lint_pair bndr' rhs
      = addLoc (RhsOf bndr') $
        do { rhs_ty <- lintRhs bndr' rhs         -- Check the rhs
           ; lintLetBind top_lvl Recursive bndr' rhs rhs_ty }

lintLetBind :: TopLevelFlag -> RecFlag -> LintedId
              -> CoreExpr -> LintedType -> LintM ()
-- Binder's type, and the RHS, have already been linted
-- This function checks other invariants
lintLetBind top_lvl rec_flag binder rhs rhs_ty
  = do { let binder_ty = idType binder
       ; ensureEqTys binder_ty rhs_ty (mkRhsMsg binder (text "RHS") rhs_ty)

       -- If the binding is for a CoVar, the RHS should be (Coercion co)
       -- See Note [Core type and coercion invariant] in GHC.Core
       ; checkL (not (isCoVar binder) || isCoArg rhs)
                (mkLetErr binder rhs)

        -- Check the let/app invariant
        -- See Note [Core let/app invariant] in GHC.Core
       ; checkL ( isJoinId binder
               || not (isUnliftedType binder_ty)
               || (isNonRec rec_flag && exprOkForSpeculation rhs)
               || exprIsTickedString rhs)
           (badBndrTyMsg binder (text "unlifted"))

        -- Check that if the binder is top-level or recursive, it's not
        -- demanded. Primitive string literals are exempt as there is no
        -- computation to perform, see Note [Core top-level string literals].
       ; checkL (not (isStrictId binder)
            || (isNonRec rec_flag && not (isTopLevel top_lvl))
            || exprIsTickedString rhs)
           (mkStrictMsg binder)

        -- Check that if the binder is at the top level and has type Addr#,
        -- that it is a string literal, see
        -- Note [Core top-level string literals].
       ; checkL (not (isTopLevel top_lvl && binder_ty `eqType` addrPrimTy)
                 || exprIsTickedString rhs)
           (mkTopNonLitStrMsg binder)

       ; flags <- getLintFlags

         -- Check that a join-point binder has a valid type
         -- NB: lintIdBinder has checked that it is not top-level bound
       ; case isJoinId_maybe binder of
            Nothing    -> return ()
            Just arity ->  checkL (isValidJoinPointType arity binder_ty)
                                  (mkInvalidJoinPointMsg binder binder_ty)

       ; when (lf_check_inline_loop_breakers flags
               && isStableUnfolding (realIdUnfolding binder)
               && isStrongLoopBreaker (idOccInfo binder)
               && isInlinePragma (idInlinePragma binder))
              (addWarnL (text "INLINE binder is (non-rule) loop breaker:" <+> ppr binder))
              -- Only non-rule loop breakers inhibit inlining

       -- We used to check that the dmdTypeDepth of a demand signature never
       -- exceeds idArity, but that is an unnecessary complication, see
       -- Note [idArity varies independently of dmdTypeDepth] in GHC.Core.Op.DmdAnal

       -- Check that the binder's arity is within the bounds imposed by
       -- the type and the strictness signature. See Note [exprArity invariant]
       -- and Note [Trimming arity]
       ; checkL (typeArity (idType binder) `lengthAtLeast` idArity binder)
           (text "idArity" <+> ppr (idArity binder) <+>
           text "exceeds typeArity" <+>
           ppr (length (typeArity (idType binder))) <> colon <+>
           ppr binder)

       ; case splitStrictSig (idStrictness binder) of
           (demands, result_info) | isBotDiv result_info ->
             checkL (demands `lengthAtLeast` idArity binder)
               (text "idArity" <+> ppr (idArity binder) <+>
               text "exceeds arity imposed by the strictness signature" <+>
               ppr (idStrictness binder) <> colon <+>
               ppr binder)
           _ -> return ()

       ; mapM_ (lintCoreRule binder binder_ty) (idCoreRules binder)

       ; addLoc (UnfoldingOf binder) $
         lintIdUnfolding binder binder_ty (idUnfolding binder) }

        -- We should check the unfolding, if any, but this is tricky because
        -- the unfolding is a SimplifiableCoreExpr. Give up for now.

-- | Checks the RHS of bindings. It only differs from 'lintCoreExpr'
-- in that it doesn't reject occurrences of the function 'makeStatic' when they
-- appear at the top level and @lf_check_static_ptrs == AllowAtTopLevel@, and
-- for join points, it skips the outer lambdas that take arguments to the
-- join point.
--
-- See Note [Checking StaticPtrs].
lintRhs :: Id -> CoreExpr -> LintM LintedType
-- NB: the Id can be Linted or not -- it's only used for
--     its OccInfo and join-pointer-hood
lintRhs bndr rhs
    | Just arity <- isJoinId_maybe bndr
    = lint_join_lams arity arity True rhs
    | AlwaysTailCalled arity <- tailCallInfo (idOccInfo bndr)
    = lint_join_lams arity arity False rhs
  where
    lint_join_lams 0 _ _ rhs
      = lintCoreExpr rhs

    lint_join_lams n tot enforce (Lam var expr)
      = lintLambda var $ lint_join_lams (n-1) tot enforce expr

    lint_join_lams n tot True _other
      = failWithL $ mkBadJoinArityMsg bndr tot (tot-n) rhs
    lint_join_lams _ _ False rhs
      = markAllJoinsBad $ lintCoreExpr rhs
          -- Future join point, not yet eta-expanded
          -- Body is not a tail position

-- Allow applications of the data constructor @StaticPtr@ at the top
-- but produce errors otherwise.
lintRhs _bndr rhs = fmap lf_check_static_ptrs getLintFlags >>= go
  where
    -- Allow occurrences of 'makeStatic' at the top-level but produce errors
    -- otherwise.
    go AllowAtTopLevel
      | (binders0, rhs') <- collectTyBinders rhs
      , Just (fun, t, info, e) <- collectMakeStaticArgs rhs'
      = markAllJoinsBad $
        foldr
        -- imitate @lintCoreExpr (Lam ...)@
        lintLambda
        -- imitate @lintCoreExpr (App ...)@
        (do fun_ty <- lintCoreExpr fun
            lintCoreArgs fun_ty [Type t, info, e]
        )
        binders0
    go _ = markAllJoinsBad $ lintCoreExpr rhs

lintIdUnfolding :: Id -> Type -> Unfolding -> LintM ()
lintIdUnfolding bndr bndr_ty uf
  | isStableUnfolding uf
  , Just rhs <- maybeUnfoldingTemplate uf
  = do { ty <- if isCompulsoryUnfolding uf
               then noLPChecks $ lintRhs bndr rhs
                     -- See Note [Checking for levity polymorphism]
               else lintRhs bndr rhs
       ; ensureEqTys bndr_ty ty (mkRhsMsg bndr (text "unfolding") ty) }
lintIdUnfolding  _ _ _
  = return ()       -- Do not Lint unstable unfoldings, because that leads
                    -- to exponential behaviour; c.f. GHC.Core.FVs.idUnfoldingVars

{-
Note [Checking for INLINE loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very suspicious if a strong loop breaker is marked INLINE.

However, the desugarer generates instance methods with INLINE pragmas
that form a mutually recursive group.  Only after a round of
simplification are they unravelled.  So we suppress the test for
the desugarer.

Note [Checking for levity polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We ordinarily want to check for bad levity polymorphism. See
Note [Levity polymorphism invariants] in GHC.Core. However, we do *not*
want to do this in a compulsory unfolding. Compulsory unfoldings arise
only internally, for things like newtype wrappers, dictionaries, and
(notably) unsafeCoerce#. These might legitimately be levity-polymorphic;
indeed levity-polyorphic unfoldings are a primary reason for the
very existence of compulsory unfoldings (we can't compile code for
the original, levity-poly, binding).

It is vitally important that we do levity-polymorphism checks *after*
performing the unfolding, but not beforehand. This is all safe because
we will check any unfolding after it has been unfolded; checking the
unfolding beforehand is merely an optimization, and one that actively
hurts us here.

************************************************************************
*                                                                      *
\subsection[lintCoreExpr]{lintCoreExpr}
*                                                                      *
************************************************************************
-}

-- Linted things: substitution applied, and type is linted
type LintedType     = Type
type LintedKind     = Kind
type LintedCoercion = Coercion
type LintedTyCoVar  = TyCoVar
type LintedId       = Id

lintCoreExpr :: CoreExpr -> LintM LintedType
-- The returned type has the substitution from the monad
-- already applied to it:
--      lintCoreExpr e subst = exprType (subst e)
--
-- The returned "type" can be a kind, if the expression is (Type ty)

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]

lintCoreExpr (Var var)
  = lintIdOcc var 0

lintCoreExpr (Lit lit)
  = return (literalType lit)

lintCoreExpr (Cast expr co)
  = do { expr_ty <- markAllJoinsBad $ lintCoreExpr expr
       ; co' <- lintCoercion co
       ; let (Pair from_ty to_ty, role) = coercionKindRole co'
       ; checkValueType to_ty $
         text "target of cast" <+> quotes (ppr co')
       ; lintRole co' Representational role
       ; ensureEqTys from_ty expr_ty (mkCastErr expr co' from_ty expr_ty)
       ; return to_ty }

lintCoreExpr (Tick tickish expr)
  = do case tickish of
         Breakpoint _ ids -> forM_ ids $ \id -> do
                               checkDeadIdOcc id
                               lookupIdInScope id
         _                -> return ()
       markAllJoinsBadIf block_joins $ lintCoreExpr expr
  where
    block_joins = not (tickish `tickishScopesLike` SoftScope)
      -- TODO Consider whether this is the correct rule. It is consistent with
      -- the simplifier's behaviour - cost-centre-scoped ticks become part of
      -- the continuation, and thus they behave like part of an evaluation
      -- context, but soft-scoped and non-scoped ticks simply wrap the result
      -- (see Simplify.simplTick).

lintCoreExpr (Let (NonRec tv (Type ty)) body)
  | isTyVar tv
  =     -- See Note [Linting type lets]
    do  { ty' <- lintType ty
        ; lintTyBndr tv              $ \ tv' ->
    do  { addLoc (RhsOf tv) $ lintTyKind tv' ty'
                -- Now extend the substitution so we
                -- take advantage of it in the body
        ; extendTvSubstL tv ty'        $
          addLoc (BodyOfLetRec [tv]) $
          lintCoreExpr body } }

lintCoreExpr (Let (NonRec bndr rhs) body)
  | isId bndr
  = do { -- First Lint the RHS, before bringing the binder into scope
         rhs_ty <- lintRhs bndr rhs

         -- Now lint the binder
       ; lintBinder LetBind bndr $ \bndr' ->
    do { lintLetBind NotTopLevel NonRecursive bndr' rhs rhs_ty
       ; addLoc (BodyOfLetRec [bndr]) (lintCoreExpr body) } }

  | otherwise
  = failWithL (mkLetErr bndr rhs)       -- Not quite accurate

lintCoreExpr e@(Let (Rec pairs) body)
  = do  { -- Check that the list of pairs is non-empty
          checkL (not (null pairs)) (emptyRec e)

          -- Check that there are no duplicated binders
        ; let (_, dups) = removeDups compare bndrs
        ; checkL (null dups) (dupVars dups)

          -- Check that either all the binders are joins, or none
        ; checkL (all isJoinId bndrs || all (not . isJoinId) bndrs) $
          mkInconsistentRecMsg bndrs

        ; lintRecBindings NotTopLevel pairs $
          addLoc (BodyOfLetRec bndrs)       $
          lintCoreExpr body }
  where
    bndrs = map fst pairs

lintCoreExpr e@(App _ _)
  = do { fun_ty <- lintCoreFun fun (length args)
       ; lintCoreArgs fun_ty args }
  where
    (fun, args) = collectArgs e

lintCoreExpr (Lam var expr)
  = markAllJoinsBad $
    lintLambda var $ lintCoreExpr expr

lintCoreExpr (Case scrut var alt_ty alts)
  = lintCaseExpr scrut var alt_ty alts

-- This case can't happen; linting types in expressions gets routed through
-- lintCoreArgs
lintCoreExpr (Type ty)
  = failWithL (text "Type found as expression" <+> ppr ty)

lintCoreExpr (Coercion co)
  = do { co' <- addLoc (InCo co) $
                lintCoercion co
       ; return (coercionType co') }

----------------------
lintIdOcc :: Var -> Int -- Number of arguments (type or value) being passed
           -> LintM LintedType -- returns type of the *variable*
lintIdOcc var nargs
  = addLoc (OccOf var) $
    do  { checkL (isNonCoVarId var)
                 (text "Non term variable" <+> ppr var)
                 -- See GHC.Core Note [Variable occurrences in Core]

        -- Check that the type of the occurrence is the same
        -- as the type of the binding site.  The inScopeIds are
        -- /un-substituted/, so this checks that the occurrence type
        -- is identical to the binder type.
        -- This makes things much easier for things like:
        --    /\a. \(x::Maybe a). /\a. ...(x::Maybe a)...
        -- The "::Maybe a" on the occurrence is referring to the /outer/ a.
        -- If we compared /substituted/ types we'd risk comparing
        -- (Maybe a) from the binding site with bogus (Maybe a1) from
        -- the occurrence site.  Comparing un-substituted types finesses
        -- this altogether
        ; (bndr, linted_bndr_ty) <- lookupIdInScope var
        ; let occ_ty  = idType var
              bndr_ty = idType bndr
        ; ensureEqTys occ_ty bndr_ty $
          mkBndrOccTypeMismatchMsg bndr var bndr_ty occ_ty

          -- Check for a nested occurrence of the StaticPtr constructor.
          -- See Note [Checking StaticPtrs].
        ; lf <- getLintFlags
        ; when (nargs /= 0 && lf_check_static_ptrs lf /= AllowAnywhere) $
            checkL (idName var /= makeStaticName) $
              text "Found makeStatic nested in an expression"

        ; checkDeadIdOcc var
        ; checkJoinOcc var nargs

        ; return linted_bndr_ty }

lintCoreFun :: CoreExpr
            -> Int              -- Number of arguments (type or val) being passed
            -> LintM LintedType -- Returns type of the *function*
lintCoreFun (Var var) nargs
  = lintIdOcc var nargs

lintCoreFun (Lam var body) nargs
  -- Act like lintCoreExpr of Lam, but *don't* call markAllJoinsBad; see
  -- Note [Beta redexes]
  | nargs /= 0
  = lintLambda var $ lintCoreFun body (nargs - 1)

lintCoreFun expr nargs
  = markAllJoinsBadIf (nargs /= 0) $
      -- See Note [Join points are less general than the paper]
    lintCoreExpr expr
------------------
lintLambda :: Var -> LintM Type -> LintM Type
lintLambda var lintBody =
    addLoc (LambdaBodyOf var) $
    lintBinder LambdaBind var $ \ var' ->
      do { body_ty <- lintBody
         ; return (mkLamType var' body_ty) }
------------------
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

------------------
checkJoinOcc :: Id -> JoinArity -> LintM ()
-- Check that if the occurrence is a JoinId, then so is the
-- binding site, and it's a valid join Id
checkJoinOcc var n_args
  | Just join_arity_occ <- isJoinId_maybe var
  = do { mb_join_arity_bndr <- lookupJoinId var
       ; case mb_join_arity_bndr of {
           Nothing -> -- Binder is not a join point
                      do { join_set <- getValidJoins
                         ; addErrL (text "join set " <+> ppr join_set $$
                                    invalidJoinOcc var) } ;

           Just join_arity_bndr ->

    do { checkL (join_arity_bndr == join_arity_occ) $
           -- Arity differs at binding site and occurrence
         mkJoinBndrOccMismatchMsg var join_arity_bndr join_arity_occ

       ; checkL (n_args == join_arity_occ) $
           -- Arity doesn't match #args
         mkBadJumpMsg var join_arity_occ n_args } } }

  | otherwise
  = return ()

{-
Note [No alternatives lint check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions with no alternatives are odd beasts, and it would seem
like they would worth be looking at in the linter (cf #10180). We
used to check two things:

* exprIsHNF is false: it would *seem* to be terribly wrong if
  the scrutinee was already in head normal form.

* exprIsBottom is true: we should be able to see why GHC believes the
  scrutinee is diverging for sure.

It was already known that the second test was not entirely reliable.
Unfortunately (#13990), the first test turned out not to be reliable
either. Getting the checks right turns out to be somewhat complicated.

For example, suppose we have (comment 8)

  data T a where
    TInt :: T Int

  absurdTBool :: T Bool -> a
  absurdTBool v = case v of

  data Foo = Foo !(T Bool)

  absurdFoo :: Foo -> a
  absurdFoo (Foo x) = absurdTBool x

GHC initially accepts the empty case because of the GADT conditions. But then
we inline absurdTBool, getting

  absurdFoo (Foo x) = case x of

x is in normal form (because the Foo constructor is strict) but the
case is empty. To avoid this problem, GHC would have to recognize
that matching on Foo x is already absurd, which is not so easy.

More generally, we don't really know all the ways that GHC can
lose track of why an expression is bottom, so we shouldn't make too
much fuss when that happens.


Note [Beta redexes]
~~~~~~~~~~~~~~~~~~~
Consider:

  join j @x y z = ... in
  (\@x y z -> jump j @x y z) @t e1 e2

This is clearly ill-typed, since the jump is inside both an application and a
lambda, either of which is enough to disqualify it as a tail call (see Note
[Invariants on join points] in GHC.Core). However, strictly from a
lambda-calculus perspective, the term doesn't go wrong---after the two beta
reductions, the jump *is* a tail call and everything is fine.

Why would we want to allow this when we have let? One reason is that a compound
beta redex (that is, one with more than one argument) has different scoping
rules: naively reducing the above example using lets will capture any free
occurrence of y in e2. More fundamentally, type lets are tricky; many passes,
such as Float Out, tacitly assume that the incoming program's type lets have
all been dealt with by the simplifier. Thus we don't want to let-bind any types
in, say, GHC.Core.Subst.simpleOptPgm, which in some circumstances can run immediately
before Float Out.

All that said, currently GHC.Core.Subst.simpleOptPgm is the only thing using this
loophole, doing so to avoid re-traversing large functions (beta-reducing a type
lambda without introducing a type let requires a substitution). TODO: Improve
simpleOptPgm so that we can forget all this ever happened.

************************************************************************
*                                                                      *
\subsection[lintCoreArgs]{lintCoreArgs}
*                                                                      *
************************************************************************

The basic version of these functions checks that the argument is a
subtype of the required type, as one would expect.
-}


lintCoreArgs  :: LintedType -> [CoreArg] -> LintM LintedType
lintCoreArgs fun_ty args = foldM lintCoreArg fun_ty args

lintCoreArg  :: LintedType -> CoreArg -> LintM LintedType
lintCoreArg fun_ty (Type arg_ty)
  = do { checkL (not (isCoercionTy arg_ty))
                (text "Unnecessary coercion-to-type injection:"
                  <+> ppr arg_ty)
       ; arg_ty' <- lintType arg_ty
       ; lintTyApp fun_ty arg_ty' }

lintCoreArg fun_ty arg
  = do { arg_ty <- markAllJoinsBad $ lintCoreExpr arg
           -- See Note [Levity polymorphism invariants] in GHC.Core
       ; flags <- getLintFlags
       ; lintL (not (lf_check_levity_poly flags) || not (isTypeLevPoly arg_ty))
           (text "Levity-polymorphic argument:" <+>
             (ppr arg <+> dcolon <+> parens (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))))
          -- check for levity polymorphism first, because otherwise isUnliftedType panics

       ; checkL (not (isUnliftedType arg_ty) || exprOkForSpeculation arg)
                (mkLetAppMsg arg)

       ; lintValApp arg fun_ty arg_ty }

-----------------
lintAltBinders :: LintedType     -- Scrutinee type
               -> LintedType     -- Constructor type
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
lintTyApp :: LintedType -> LintedType -> LintM LintedType
lintTyApp fun_ty arg_ty
  | Just (tv,body_ty) <- splitForAllTy_maybe fun_ty
  = do  { lintTyKind tv arg_ty
        ; in_scope <- getInScope
        -- substTy needs the set of tyvars in scope to avoid generating
        -- uniques that are already in scope.
        -- See Note [The substitution invariant] in GHC.Core.TyCo.Subst
        ; return (substTyWithInScope in_scope [tv] [arg_ty] body_ty) }

  | otherwise
  = failWithL (mkTyAppMsg fun_ty arg_ty)

-----------------
lintValApp :: CoreExpr -> LintedType -> LintedType -> LintM LintedType
lintValApp arg fun_ty arg_ty
  | Just (arg,res) <- splitFunTy_maybe fun_ty
  = do { ensureEqTys arg arg_ty err1
       ; return res }
  | otherwise
  = failWithL err2
  where
    err1 = mkAppMsg       fun_ty arg_ty arg
    err2 = mkNonFunAppMsg fun_ty arg_ty arg

lintTyKind :: OutTyVar -> LintedType -> LintM ()
-- Both args have had substitution applied

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintTyKind tyvar arg_ty
  = unless (arg_kind `eqType` tyvar_kind) $
    addErrL (mkKindErrMsg tyvar arg_ty $$ (text "Linted Arg kind:" <+> ppr arg_kind))
  where
    tyvar_kind = tyVarKind tyvar
    arg_kind = typeKind arg_ty

{-
************************************************************************
*                                                                      *
\subsection[lintCoreAlts]{lintCoreAlts}
*                                                                      *
************************************************************************
-}

lintCaseExpr :: CoreExpr -> Id -> Type -> [CoreAlt] -> LintM LintedType
lintCaseExpr scrut var alt_ty alts =
  do { let e = Case scrut var alt_ty alts   -- Just for error messages

     -- Check the scrutinee
     ; scrut_ty <- markAllJoinsBad $ lintCoreExpr scrut
          -- See Note [Join points are less general than the paper]
          -- in GHC.Core

     ; alt_ty <- addLoc (CaseTy scrut) $
                 lintValueType alt_ty
     ; var_ty <- addLoc (IdTy var) $
                 lintValueType (idType var)

     -- We used to try to check whether a case expression with no
     -- alternatives was legitimate, but this didn't work.
     -- See Note [No alternatives lint check] for details.

     -- Check that the scrutinee is not a floating-point type
     -- if there are any literal alternatives
     -- See GHC.Core Note [Case expression invariants] item (5)
     -- See Note [Rules for floating-point comparisons] in GHC.Core.Op.ConstantFold
     ; let isLitPat (LitAlt _, _ , _) = True
           isLitPat _                 = False
     ; checkL (not $ isFloatingTy scrut_ty && any isLitPat alts)
         (ptext (sLit $ "Lint warning: Scrutinising floating-point " ++
                        "expression with literal pattern in case " ++
                        "analysis (see #9238).")
          $$ text "scrut" <+> ppr scrut)

     ; case tyConAppTyCon_maybe (idType var) of
         Just tycon
              | debugIsOn
              , isAlgTyCon tycon
              , not (isAbstractTyCon tycon)
              , null (tyConDataCons tycon)
              , not (exprIsBottom scrut)
              -> pprTrace "Lint warning: case binder's type has no constructors" (ppr var <+> ppr (idType var))
                        -- This can legitimately happen for type families
                      $ return ()
         _otherwise -> return ()

        -- Don't use lintIdBndr on var, because unboxed tuple is legitimate

     ; subst <- getTCvSubst
     ; ensureEqTys var_ty scrut_ty (mkScrutMsg var var_ty scrut_ty subst)
       -- See GHC.Core Note [Case expression invariants] item (7)

     ; lintBinder CaseBind var $ \_ ->
       do { -- Check the alternatives
            mapM_ (lintCoreAlt scrut_ty alt_ty) alts
          ; checkCaseAlts e scrut_ty alts
          ; return alt_ty } }

checkCaseAlts :: CoreExpr -> LintedType -> [CoreAlt] -> LintM ()
-- a) Check that the alts are non-empty
-- b1) Check that the DEFAULT comes first, if it exists
-- b2) Check that the others are in increasing order
-- c) Check that there's a default for infinite types
-- NB: Algebraic cases are not necessarily exhaustive, because
--     the simplifier correctly eliminates case that can't
--     possibly match.

checkCaseAlts e ty alts =
  do { checkL (all non_deflt con_alts) (mkNonDefltMsg e)
         -- See GHC.Core Note [Case expression invariants] item (2)

     ; checkL (increasing_tag con_alts) (mkNonIncreasingAltsMsg e)
         -- See GHC.Core Note [Case expression invariants] item (3)

          -- For types Int#, Word# with an infinite (well, large!) number of
          -- possible values, there should usually be a DEFAULT case
          -- But (see Note [Empty case alternatives] in GHC.Core) it's ok to
          -- have *no* case alternatives.
          -- In effect, this is a kind of partial test. I suppose it's possible
          -- that we might *know* that 'x' was 1 or 2, in which case
          --   case x of { 1 -> e1; 2 -> e2 }
          -- would be fine.
     ; checkL (isJust maybe_deflt || not is_infinite_ty || null alts)
              (nonExhaustiveAltsMsg e) }
  where
    (con_alts, maybe_deflt) = findDefault alts

        -- Check that successive alternatives have strictly increasing tags
    increasing_tag (alt1 : rest@( alt2 : _)) = alt1 `ltAlt` alt2 && increasing_tag rest
    increasing_tag _                         = True

    non_deflt (DEFAULT, _, _) = False
    non_deflt _               = True

    is_infinite_ty = case tyConAppTyCon_maybe ty of
                        Nothing    -> False
                        Just tycon -> isPrimTyCon tycon

lintAltExpr :: CoreExpr -> LintedType -> LintM ()
lintAltExpr expr ann_ty
  = do { actual_ty <- lintCoreExpr expr
       ; ensureEqTys actual_ty ann_ty (mkCaseAltMsg expr actual_ty ann_ty) }
         -- See GHC.Core Note [Case expression invariants] item (6)

lintCoreAlt :: LintedType          -- Type of scrutinee
            -> LintedType          -- Type of the alternative
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
    ; lintBinders CasePatBind args $ \ args' -> do
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
lintBinders :: BindingSite -> [Var] -> ([Var] -> LintM a) -> LintM a
lintBinders _    []         linterF = linterF []
lintBinders site (var:vars) linterF = lintBinder site var $ \var' ->
                                      lintBinders site vars $ \ vars' ->
                                      linterF (var':vars')

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintBinder :: BindingSite -> Var -> (Var -> LintM a) -> LintM a
lintBinder site var linterF
  | isTyCoVar var = lintTyCoBndr var linterF
  | otherwise     = lintIdBndr NotTopLevel site var linterF

lintTyBndr :: TyVar -> (LintedTyCoVar -> LintM a) -> LintM a
lintTyBndr = lintTyCoBndr  -- We could specialise it, I guess

-- lintCoBndr :: CoVar -> (LintedTyCoVar -> LintM a) -> LintM a
-- lintCoBndr = lintTyCoBndr  -- We could specialise it, I guess

lintTyCoBndr :: TyCoVar -> (LintedTyCoVar -> LintM a) -> LintM a
lintTyCoBndr tcv thing_inside
  = do { subst <- getTCvSubst
       ; kind' <- lintType (varType tcv)
       ; let tcv' = uniqAway (getTCvInScope subst) $
                    setVarType tcv kind'
             subst' = extendTCvSubstWithClone subst tcv tcv'
       ; when (isCoVar tcv) $
         lintL (isCoVarType kind')
               (text "CoVar with non-coercion type:" <+> pprTyVar tcv)
       ; updateTCvSubst subst' (thing_inside tcv') }

lintIdBndrs :: forall a. TopLevelFlag -> [Id] -> ([LintedId] -> LintM a) -> LintM a
lintIdBndrs top_lvl ids thing_inside
  = go ids thing_inside
  where
    go :: [Id] -> ([Id] -> LintM a) -> LintM a
    go []       thing_inside = thing_inside []
    go (id:ids) thing_inside = lintIdBndr top_lvl LetBind id  $ \id' ->
                               go ids                         $ \ids' ->
                               thing_inside (id' : ids')

lintIdBndr :: TopLevelFlag -> BindingSite
           -> InVar -> (OutVar -> LintM a) -> LintM a
-- Do substitution on the type of a binder and add the var with this
-- new type to the in-scope set of the second argument
-- ToDo: lint its rules
lintIdBndr top_lvl bind_site id thing_inside
  = ASSERT2( isId id, ppr id )
    do { flags <- getLintFlags
       ; checkL (not (lf_check_global_ids flags) || isLocalId id)
                (text "Non-local Id binder" <+> ppr id)
                -- See Note [Checking for global Ids]

       -- Check that if the binder is nested, it is not marked as exported
       ; checkL (not (isExportedId id) || is_top_lvl)
           (mkNonTopExportedMsg id)

       -- Check that if the binder is nested, it does not have an external name
       ; checkL (not (isExternalName (Var.varName id)) || is_top_lvl)
           (mkNonTopExternalNameMsg id)

          -- See Note [Levity polymorphism invariants] in GHC.Core
       ; lintL (isJoinId id || not (lf_check_levity_poly flags)
                || not (isTypeLevPoly id_ty)) $
         text "Levity-polymorphic binder:" <+> ppr id <+> dcolon <+>
            parens (ppr id_ty <+> dcolon <+> ppr (typeKind id_ty))

       -- Check that a join-id is a not-top-level let-binding
       ; when (isJoinId id) $
         checkL (not is_top_lvl && is_let_bind) $
         mkBadJoinBindMsg id

       -- Check that the Id does not have type (t1 ~# t2) or (t1 ~R# t2);
       -- if so, it should be a CoVar, and checked by lintCoVarBndr
       ; lintL (not (isCoVarType id_ty))
               (text "Non-CoVar has coercion type" <+> ppr id <+> dcolon <+> ppr id_ty)

       ; linted_ty <- addLoc (IdTy id) (lintValueType id_ty)

       ; addInScopeId id linted_ty $
         thing_inside (setIdType id linted_ty) }
  where
    id_ty = idType id

    is_top_lvl = isTopLevel top_lvl
    is_let_bind = case bind_site of
                    LetBind -> True
                    _       -> False

{-
%************************************************************************
%*                                                                      *
             Types
%*                                                                      *
%************************************************************************
-}

lintTypes :: DynFlags
          -> [TyCoVar]   -- Treat these as in scope
          -> [Type]
          -> Maybe MsgDoc -- Nothing => OK
lintTypes dflags vars tys
  | isEmptyBag errs = Nothing
  | otherwise       = Just (pprMessageBag errs)
  where
    (_warns, errs) = initL dflags defaultLintFlags vars linter
    linter = lintBinders LambdaBind vars $ \_ ->
             mapM_ lintType tys

lintValueType :: Type -> LintM LintedType
-- Types only, not kinds
-- Check the type, and apply the substitution to it
-- See Note [Linting type lets]
lintValueType ty
  = addLoc (InType ty) $
    do  { ty' <- lintType ty
        ; let sk = typeKind ty'
        ; lintL (classifiesTypeWithValues sk) $
          hang (text "Ill-kinded type:" <+> ppr ty)
             2 (text "has kind:" <+> ppr sk)
        ; return ty' }

checkTyCon :: TyCon -> LintM ()
checkTyCon tc
  = checkL (not (isTcTyCon tc)) (text "Found TcTyCon:" <+> ppr tc)

-------------------
lintType :: LintedType -> LintM LintedType

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintType (TyVarTy tv)
  | not (isTyVar tv)
  = failWithL (mkBadTyVarMsg tv)

  | otherwise
  = do { subst <- getTCvSubst
       ; case lookupTyVar subst tv of
           Just linted_ty -> return linted_ty

           -- In GHCi we may lint an expression with a free
           -- type variable.  Then it won't be in the
           -- substitution, but it should be in scope
           Nothing | tv `isInScope` subst
                   -> return (TyVarTy tv)
                   | otherwise
                   -> failWithL $
                      hang (text "The type variable" <+> pprBndr LetBind tv)
                         2 (text "is out of scope")
     }

lintType ty@(AppTy t1 t2)
  | TyConApp {} <- t1
  = failWithL $ text "TyConApp to the left of AppTy:" <+> ppr ty
  | otherwise
  = do { t1' <- lintType t1
       ; t2' <- lintType t2
       ; lint_ty_app ty (typeKind t1') [t2']
       ; return (AppTy t1' t2') }

lintType ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = do { report_unsat <- lf_report_unsat_syns <$> getLintFlags
       ; lintTySynFamApp report_unsat ty tc tys }

  | isFunTyCon tc
  , tys `lengthIs` 4
    -- We should never see a saturated application of funTyCon; such
    -- applications should be represented with the FunTy constructor.
    -- See Note [Linting function types] and
    -- Note [Representation of function types].
  = failWithL (hang (text "Saturated application of (->)") 2 (ppr ty))

  | otherwise  -- Data types, data families, primitive types
  = do { checkTyCon tc
       ; tys' <- mapM lintType tys
       ; lint_ty_app ty (tyConKind tc) tys'
       ; return (TyConApp tc tys') }

-- arrows can related *unlifted* kinds, so this has to be separate from
-- a dependent forall.
lintType ty@(FunTy af t1 t2)
  = do { t1' <- lintType t1
       ; t2' <- lintType t2
       ; lintArrow (text "type or kind" <+> quotes (ppr ty)) t1' t2'
       ; return (FunTy af t1' t2') }

lintType ty@(ForAllTy (Bndr tcv vis) body_ty)
  | not (isTyCoVar tcv)
  = failWithL (text "Non-Tyvar or Non-Covar bound in type:" <+> ppr ty)
  | otherwise
  = lintTyCoBndr tcv $ \tcv' ->
    do { body_ty' <- lintType body_ty
       ; lintForAllBody tcv' body_ty'

       ; when (isCoVar tcv) $
         lintL (tcv `elemVarSet` tyCoVarsOfType body_ty) $
         text "Covar does not occur in the body:" <+> (ppr tcv $$ ppr body_ty)
         -- See GHC.Core.TyCo.Rep Note [Unused coercion variable in ForAllTy]
         -- and cf GHC.Core.Coercion Note [Unused coercion variable in ForAllCo]

       ; return (ForAllTy (Bndr tcv' vis) body_ty') }

lintType ty@(LitTy l)
  = do { lintTyLit l; return ty }

lintType (CastTy ty co)
  = do { ty' <- lintType ty
       ; co' <- lintStarCoercion co
       ; let tyk = typeKind ty'
             cok = coercionLKind co'
       ; ensureEqTys tyk cok (mkCastTyErr ty co tyk cok)
       ; return (CastTy ty' co') }

lintType (CoercionTy co)
  = do { co' <- lintCoercion co
       ; return (CoercionTy co') }

-----------------
lintForAllBody :: LintedTyCoVar -> LintedType -> LintM ()
-- Do the checks for the body of a forall-type
lintForAllBody tcv body_ty
  = do { checkValueType body_ty (text "the body of forall:" <+> ppr body_ty)

         -- For type variables, check for skolem escape
         -- See Note [Phantom type variables in kinds] in GHC.Core.Type
         -- The kind of (forall cv. th) is liftedTypeKind, so no
         -- need to check for skolem-escape in the CoVar case
       ; let body_kind = typeKind body_ty
       ; when (isTyVar tcv) $
         case occCheckExpand [tcv] body_kind of
           Just {} -> return ()
           Nothing -> failWithL $
                      hang (text "Variable escape in forall:")
                         2 (vcat [ text "tyvar:" <+> ppr tcv
                                 , text "type:" <+> ppr body_ty
                                 , text "kind:" <+> ppr body_kind ])
    }

-----------------
lintTySynFamApp :: Bool -> InType -> TyCon -> [InType] -> LintM LintedType
-- The TyCon is a type synonym or a type family (not a data family)
-- See Note [Linting type synonym applications]
-- c.f. TcValidity.check_syn_tc_app
lintTySynFamApp report_unsat ty tc tys
  | report_unsat   -- Report unsaturated only if report_unsat is on
  , tys `lengthLessThan` tyConArity tc
  = failWithL (hang (text "Un-saturated type application") 2 (ppr ty))

  -- Deal with type synonyms
  | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  , let expanded_ty = mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys'
  = do { -- Kind-check the argument types, but without reporting
         -- un-saturated type families/synonyms
         tys' <- setReportUnsat False (mapM lintType tys)

       ; when report_unsat $
         do { _ <- lintType expanded_ty
            ; return () }

       ; lint_ty_app ty (tyConKind tc) tys'
       ; return (TyConApp tc tys') }

  -- Otherwise this must be a type family
  | otherwise
  = do { tys' <- mapM lintType tys
       ; lint_ty_app ty (tyConKind tc) tys'
       ; return (TyConApp tc tys') }

-----------------
-- Confirms that a type is really *, #, Constraint etc
checkValueType :: LintedType -> SDoc -> LintM ()
checkValueType ty doc
  = lintL (classifiesTypeWithValues kind)
          (text "Non-*-like kind when *-like expected:" <+> ppr kind $$
           text "when checking" <+> doc)
  where
    kind = typeKind ty

-----------------
lintArrow :: SDoc -> LintedType -> LintedType -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintArrow what t1 t2   -- Eg lintArrow "type or kind `blah'" k1 k2
                       -- or lintArrow "coercion `blah'" k1 k2
  = do { unless (classifiesTypeWithValues k1) (addErrL (msg (text "argument") k1))
       ; unless (classifiesTypeWithValues k2) (addErrL (msg (text "result")   k2)) }
  where
    k1 = typeKind t1
    k2 = typeKind t2
    msg ar k
      = vcat [ hang (text "Ill-kinded" <+> ar)
                  2 (text "in" <+> what)
             , what <+> text "kind:" <+> ppr k ]

-----------------
lint_ty_app :: Type -> LintedKind -> [LintedType] -> LintM ()
lint_ty_app ty k tys
  = lint_app (text "type" <+> quotes (ppr ty)) k tys

----------------
lint_co_app :: Coercion -> LintedKind -> [LintedType] -> LintM ()
lint_co_app ty k tys
  = lint_app (text "coercion" <+> quotes (ppr ty)) k tys

----------------
lintTyLit :: TyLit -> LintM ()
lintTyLit (NumTyLit n)
  | n >= 0    = return ()
  | otherwise = failWithL msg
    where msg = text "Negative type literal:" <+> integer n
lintTyLit (StrTyLit _) = return ()

lint_app :: SDoc -> LintedKind -> [LintedType] -> LintM ()
-- (lint_app d fun_kind arg_tys)
--    We have an application (f arg_ty1 .. arg_tyn),
--    where f :: fun_kind

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lint_app doc kfn arg_tys
    = do { in_scope <- getInScope
         -- We need the in_scope set to satisfy the invariant in
         -- Note [The substitution invariant] in GHC.Core.TyCo.Subst
         ; _ <- foldlM (go_app in_scope) kfn arg_tys
         ; return () }
  where
    fail_msg extra = vcat [ hang (text "Kind application error in") 2 doc
                          , nest 2 (text "Function kind =" <+> ppr kfn)
                          , nest 2 (text "Arg types =" <+> ppr arg_tys)
                          , extra ]

    go_app in_scope kfn ta
      | Just kfn' <- coreView kfn
      = go_app in_scope kfn' ta

    go_app _ fun_kind@(FunTy _ kfa kfb) ta
      = do { let ka = typeKind ta
           ; unless (ka `eqType` kfa) $
             addErrL (fail_msg (text "Fun:" <+> (ppr fun_kind $$ ppr ta <+> dcolon <+> ppr ka)))
           ; return kfb }

    go_app in_scope (ForAllTy (Bndr kv _vis) kfn) ta
      = do { let kv_kind = varType kv
                 ka      = typeKind ta
           ; unless (ka `eqType` kv_kind) $
             addErrL (fail_msg (text "Forall:" <+> (ppr kv $$ ppr kv_kind $$
                                                    ppr ta <+> dcolon <+> ppr ka)))
           ; return $ substTy (extendTCvSubst (mkEmptyTCvSubst in_scope) kv ta) kfn }

    go_app _ kfn ta
       = failWithL (fail_msg (text "Not a fun:" <+> (ppr kfn $$ ppr ta)))

{- *********************************************************************
*                                                                      *
        Linting rules
*                                                                      *
********************************************************************* -}

lintCoreRule :: OutVar -> LintedType -> CoreRule -> LintM ()
lintCoreRule _ _ (BuiltinRule {})
  = return ()  -- Don't bother

lintCoreRule fun fun_ty rule@(Rule { ru_name = name, ru_bndrs = bndrs
                                   , ru_args = args, ru_rhs = rhs })
  = lintBinders LambdaBind bndrs $ \ _ ->
    do { lhs_ty <- lintCoreArgs fun_ty args
       ; rhs_ty <- case isJoinId_maybe fun of
                     Just join_arity
                       -> do { checkL (args `lengthIs` join_arity) $
                                mkBadJoinPointRuleMsg fun join_arity rule
                               -- See Note [Rules for join points]
                             ; lintCoreExpr rhs }
                     _ -> markAllJoinsBad $ lintCoreExpr rhs
       ; ensureEqTys lhs_ty rhs_ty $
         (rule_doc <+> vcat [ text "lhs type:" <+> ppr lhs_ty
                            , text "rhs type:" <+> ppr rhs_ty
                            , text "fun_ty:" <+> ppr fun_ty ])
       ; let bad_bndrs = filter is_bad_bndr bndrs

       ; checkL (null bad_bndrs)
                (rule_doc <+> text "unbound" <+> ppr bad_bndrs)
            -- See Note [Linting rules]
    }
  where
    rule_doc = text "Rule" <+> doubleQuotes (ftext name) <> colon

    lhs_fvs = exprsFreeVars args
    rhs_fvs = exprFreeVars rhs

    is_bad_bndr :: Var -> Bool
    -- See Note [Unbound RULE binders] in GHC.Core.Rules
    is_bad_bndr bndr = not (bndr `elemVarSet` lhs_fvs)
                    && bndr `elemVarSet` rhs_fvs
                    && isNothing (isReflCoVar_maybe bndr)


{- Note [Linting rules]
~~~~~~~~~~~~~~~~~~~~~~~
It's very bad if simplifying a rule means that one of the template
variables (ru_bndrs) that /is/ mentioned on the RHS becomes
not-mentioned in the LHS (ru_args).  How can that happen?  Well, in
#10602, SpecConstr stupidly constructed a rule like

  forall x,c1,c2.
     f (x |> c1 |> c2) = ....

But simplExpr collapses those coercions into one.  (Indeed in
#10602, it collapsed to the identity and was removed altogether.)

We don't have a great story for what to do here, but at least
this check will nail it.

NB (#11643): it's possible that a variable listed in the
binders becomes not-mentioned on both LHS and RHS.  Here's a silly
example:
   RULE forall x y. f (g x y) = g (x+1) (y-1)
And suppose worker/wrapper decides that 'x' is Absent.  Then
we'll end up with
   RULE forall x y. f ($gw y) = $gw (x+1)
This seems sufficiently obscure that there isn't enough payoff to
try to trim the forall'd binder list.

Note [Rules for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A join point cannot be partially applied. However, the left-hand side of a rule
for a join point is effectively a *pattern*, not a piece of code, so there's an
argument to be made for allowing a situation like this:

  join $sj :: Int -> Int -> String
       $sj n m = ...
       j :: forall a. Eq a => a -> a -> String
       {-# RULES "SPEC j" jump j @ Int $dEq = jump $sj #-}
       j @a $dEq x y = ...

Applying this rule can't turn a well-typed program into an ill-typed one, so
conceivably we could allow it. But we can always eta-expand such an
"undersaturated" rule (see 'GHC.Core.Arity.etaExpandToJoinPointRule'), and in fact
the simplifier would have to in order to deal with the RHS. So we take a
conservative view and don't allow undersaturated rules for join points. See
Note [Rules and join points] in OccurAnal for further discussion.
-}

{-
************************************************************************
*                                                                      *
         Linting coercions
*                                                                      *
************************************************************************
-}

{- Note [Asymptotic efficiency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When linting coercions (and types actually) we return a linted
(substituted) coercion.  Then we often have to take the coercionKind of
that returned coercion. If we get long chains, that can be asymptotically
inefficient, notably in
* TransCo
* InstCo
* NthCo (cf #9233)
* LRCo

But the code is simple.  And this is only Lint.  Let's wait to see if
the bad perf bites us in practice.

A solution would be to return the kind and role of the coercion,
as well as the linted coercion.  Or perhaps even *only* the kind and role,
which is what used to happen.   But that proved tricky and error prone
(#17923), so now we return the coercion.
-}


-- lints a coercion, confirming that its lh kind and its rh kind are both *
-- also ensures that the role is Nominal
lintStarCoercion :: InCoercion -> LintM LintedCoercion
lintStarCoercion g
  = do { g' <- lintCoercion g
       ; let Pair t1 t2 = coercionKind g'
       ; checkValueType t1 (text "the kind of the left type in" <+> ppr g)
       ; checkValueType t2 (text "the kind of the right type in" <+> ppr g)
       ; lintRole g Nominal (coercionRole g)
       ; return g' }

lintCoercion :: InCoercion -> LintM LintedCoercion
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]

lintCoercion (CoVarCo cv)
  | not (isCoVar cv)
  = failWithL (hang (text "Bad CoVarCo:" <+> ppr cv)
                  2 (text "With offending type:" <+> ppr (varType cv)))

  | otherwise
  = do { subst <- getTCvSubst
       ; case lookupCoVar subst cv of
           Just linted_co -> return linted_co ;
           Nothing -> -- lintCoBndr always extends the substitition
                      failWithL $
                      hang (text "The coercion variable" <+> pprBndr LetBind cv)
                         2 (text "is out of scope")
     }


lintCoercion (Refl ty)
  = do { ty' <- lintType ty
       ; return (Refl ty') }

lintCoercion (GRefl r ty MRefl)
  = do { ty' <- lintType ty
       ; return (GRefl r ty' MRefl) }

lintCoercion (GRefl r ty (MCo co))
  = do { ty' <- lintType ty
       ; co' <- lintCoercion co
       ; let tk = typeKind ty'
             tl = coercionLKind co'
       ; ensureEqTys tk tl $
         hang (text "GRefl coercion kind mis-match:" <+> ppr co)
            2 (vcat [ppr ty', ppr tk, ppr tl])
       ; lintRole co' Nominal (coercionRole co')
       ; return (GRefl r ty' (MCo co')) }

lintCoercion co@(TyConAppCo r tc cos)
  | tc `hasKey` funTyConKey
  , [_rep1,_rep2,_co1,_co2] <- cos
  = failWithL (text "Saturated TyConAppCo (->):" <+> ppr co)
    -- All saturated TyConAppCos should be FunCos

  | Just {} <- synTyConDefn_maybe tc
  = failWithL (text "Synonym in TyConAppCo:" <+> ppr co)

  | otherwise
  = do { checkTyCon tc
       ; cos' <- mapM lintCoercion cos
       ; let (co_kinds, co_roles) = unzip (map coercionKindRole cos')
       ; lint_co_app co (tyConKind tc) (map pFst co_kinds)
       ; lint_co_app co (tyConKind tc) (map pSnd co_kinds)
       ; zipWithM_ (lintRole co) (tyConRolesX r tc) co_roles
       ; return (TyConAppCo r tc cos') }

lintCoercion co@(AppCo co1 co2)
  | TyConAppCo {} <- co1
  = failWithL (text "TyConAppCo to the left of AppCo:" <+> ppr co)
  | Just (TyConApp {}, _) <- isReflCo_maybe co1
  = failWithL (text "Refl (TyConApp ...) to the left of AppCo:" <+> ppr co)
  | otherwise
  = do { co1' <- lintCoercion co1
       ; co2' <- lintCoercion co2
       ; let (Pair lk1 rk1, r1) = coercionKindRole co1'
             (Pair lk2 rk2, r2) = coercionKindRole co2'
       ; lint_co_app co (typeKind lk1) [lk2]
       ; lint_co_app co (typeKind rk1) [rk2]

       ; if r1 == Phantom
         then lintL (r2 == Phantom || r2 == Nominal)
                     (text "Second argument in AppCo cannot be R:" $$
                      ppr co)
         else lintRole co Nominal r2

       ; return (AppCo co1' co2') }

----------
lintCoercion co@(ForAllCo tcv kind_co body_co)
  | not (isTyCoVar tcv)
  = failWithL (text "Non tyco binder in ForAllCo:" <+> ppr co)
  | otherwise
  = do { kind_co' <- lintStarCoercion kind_co
       ; lintTyCoBndr tcv $ \tcv' ->
    do { body_co' <- lintCoercion body_co
       ; ensureEqTys (varType tcv') (coercionLKind kind_co') $
         text "Kind mis-match in ForallCo" <+> ppr co

       -- Assuming kind_co :: k1 ~ k2
       -- Need to check that
       --    (forall (tcv:k1). lty) and
       --    (forall (tcv:k2). rty[(tcv:k2) |> sym kind_co/tcv])
       -- are both well formed.  Easiest way is to call lintForAllBody
       -- for each; there is actually no need to do the funky substitution
       ; let Pair lty rty = coercionKind body_co'
       ; lintForAllBody tcv' lty
       ; lintForAllBody tcv' rty

       ; when (isCoVar tcv) $
         lintL (almostDevoidCoVarOfCo tcv body_co) $
         text "Covar can only appear in Refl and GRefl: " <+> ppr co
         -- See "last wrinkle" in GHC.Core.Coercion
         -- Note [Unused coercion variable in ForAllCo]
         -- and c.f. GHC.Core.TyCo.Rep Note [Unused coercion variable in ForAllTy]

       ; return (ForAllCo tcv' kind_co' body_co') } }

lintCoercion co@(FunCo r co1 co2)
  = do { co1' <- lintCoercion co1
       ; co2' <- lintCoercion co2
       ; let Pair lt1 rt1 = coercionKind co1
             Pair lt2 rt2 = coercionKind co2
       ; lintArrow (text "coercion" <+> quotes (ppr co)) lt1 lt2
       ; lintArrow (text "coercion" <+> quotes (ppr co)) rt1 rt2
       ; lintRole co1 r (coercionRole co1)
       ; lintRole co2 r (coercionRole co2)
       ; return (FunCo r co1' co2') }

-- See Note [Bad unsafe coercion]
lintCoercion co@(UnivCo prov r ty1 ty2)
  = do { ty1' <- lintType ty1
       ; ty2' <- lintType ty2
       ; let k1 = typeKind ty1'
             k2 = typeKind ty2'
       ; prov' <- lint_prov k1 k2 prov

       ; when (r /= Phantom && classifiesTypeWithValues k1
                            && classifiesTypeWithValues k2)
              (checkTypes ty1 ty2)

       ; return (UnivCo prov' r ty1' ty2') }
   where
     report s = hang (text $ "Unsafe coercion: " ++ s)
                     2 (vcat [ text "From:" <+> ppr ty1
                             , text "  To:" <+> ppr ty2])
     isUnBoxed :: PrimRep -> Bool
     isUnBoxed = not . isGcPtrRep

       -- see #9122 for discussion of these checks
     checkTypes t1 t2
       = do { checkWarnL (not lev_poly1)
                         (report "left-hand type is levity-polymorphic")
            ; checkWarnL (not lev_poly2)
                         (report "right-hand type is levity-polymorphic")
            ; when (not (lev_poly1 || lev_poly2)) $
              do { checkWarnL (reps1 `equalLength` reps2)
                              (report "between values with different # of reps")
                 ; zipWithM_ validateCoercion reps1 reps2 }}
       where
         lev_poly1 = isTypeLevPoly t1
         lev_poly2 = isTypeLevPoly t2

         -- don't look at these unless lev_poly1/2 are False
         -- Otherwise, we get #13458
         reps1 = typePrimRep t1
         reps2 = typePrimRep t2

     validateCoercion :: PrimRep -> PrimRep -> LintM ()
     validateCoercion rep1 rep2
       = do { platform <- targetPlatform <$> getDynFlags
            ; checkWarnL (isUnBoxed rep1 == isUnBoxed rep2)
                         (report "between unboxed and boxed value")
            ; checkWarnL (TyCon.primRepSizeB platform rep1
                           == TyCon.primRepSizeB platform rep2)
                         (report "between unboxed values of different size")
            ; let fl = liftM2 (==) (TyCon.primRepIsFloat rep1)
                                   (TyCon.primRepIsFloat rep2)
            ; case fl of
                Nothing    -> addWarnL (report "between vector types")
                Just False -> addWarnL (report "between float and integral values")
                _          -> return ()
            }

     lint_prov k1 k2 (PhantomProv kco)
       = do { kco' <- lintStarCoercion kco
            ; lintRole co Phantom r
            ; check_kinds kco' k1 k2
            ; return (PhantomProv kco') }

     lint_prov k1 k2 (ProofIrrelProv kco)
       = do { lintL (isCoercionTy ty1) (mkBadProofIrrelMsg ty1 co)
            ; lintL (isCoercionTy ty2) (mkBadProofIrrelMsg ty2 co)
            ; kco' <- lintStarCoercion kco
            ; check_kinds kco k1 k2
            ; return (ProofIrrelProv kco') }

     lint_prov _ _ prov@(PluginProv _) = return prov

     check_kinds kco k1 k2
       = do { let Pair k1' k2' = coercionKind kco
            ; ensureEqTys k1 k1' (mkBadUnivCoMsg CLeft  co)
            ; ensureEqTys k2 k2' (mkBadUnivCoMsg CRight co) }


lintCoercion (SymCo co)
  = do { co' <- lintCoercion co
       ; return (SymCo co') }

lintCoercion co@(TransCo co1 co2)
  = do { co1' <- lintCoercion co1
       ; co2' <- lintCoercion co2
       ; let ty1b = coercionRKind co1'
             ty2a = coercionLKind co2'
       ; ensureEqTys ty1b ty2a
               (hang (text "Trans coercion mis-match:" <+> ppr co)
                   2 (vcat [ppr (coercionKind co1'), ppr (coercionKind co2')]))
       ; lintRole co (coercionRole co1) (coercionRole co2)
       ; return (TransCo co1' co2') }

lintCoercion the_co@(NthCo r0 n co)
  = do { co' <- lintCoercion co
       ; let (Pair s t, r) = coercionKindRole co'
       ; case (splitForAllTy_maybe s, splitForAllTy_maybe t) of
         { (Just _, Just _)
             -- works for both tyvar and covar
             | n == 0
             ,  (isForAllTy_ty s && isForAllTy_ty t)
             || (isForAllTy_co s && isForAllTy_co t)
             -> do { lintRole the_co Nominal r0
                   ; return (NthCo r0 n co') }

         ; _ -> case (splitTyConApp_maybe s, splitTyConApp_maybe t) of
         { (Just (tc_s, tys_s), Just (tc_t, tys_t))
             | tc_s == tc_t
             , isInjectiveTyCon tc_s r
                 -- see Note [NthCo and newtypes] in GHC.Core.TyCo.Rep
             , tys_s `equalLength` tys_t
             , tys_s `lengthExceeds` n
             -> do { lintRole the_co tr r0
                   ; return (NthCo r0 n co') }
                where
                  tr = nthRole r tc_s n

         ; _ -> failWithL (hang (text "Bad getNth:")
                              2 (ppr the_co $$ ppr s $$ ppr t)) }}}

lintCoercion the_co@(LRCo lr co)
  = do { co' <- lintCoercion co
       ; let Pair s t = coercionKind co'
             r        = coercionRole co'
       ; lintRole co Nominal r
       ; case (splitAppTy_maybe s, splitAppTy_maybe t) of
           (Just _, Just _) -> return (LRCo lr co')
           _ -> failWithL (hang (text "Bad LRCo:")
                              2 (ppr the_co $$ ppr s $$ ppr t)) }

lintCoercion (InstCo co arg)
  = do { co'  <- lintCoercion co
       ; arg' <- lintCoercion arg
       ; let Pair t1' t2' = coercionKind co'
             Pair s1  s2  = coercionKind arg

       ; lintRole arg Nominal (coercionRole arg')

      ; case (splitForAllTy_ty_maybe t1', splitForAllTy_ty_maybe t2') of
         -- forall over tvar
         { (Just (tv1,_), Just (tv2,_))
             | typeKind s1 `eqType` tyVarKind tv1
             , typeKind s2 `eqType` tyVarKind tv2
             -> return (InstCo co' arg')
             | otherwise
             -> failWithL (text "Kind mis-match in inst coercion")

         ; _ -> case (splitForAllTy_co_maybe t1', splitForAllTy_co_maybe t2') of
         -- forall over covar
         { (Just (cv1, _), Just (cv2, _))
             | typeKind s1 `eqType` varType cv1
             , typeKind s2 `eqType` varType cv2
             , CoercionTy _ <- s1
             , CoercionTy _ <- s2
             -> return (InstCo co' arg')
             | otherwise
             -> failWithL (text "Kind mis-match in inst coercion")

         ; _ -> failWithL (text "Bad argument of inst") }}}

lintCoercion co@(AxiomInstCo con ind cos)
  = do { unless (0 <= ind && ind < numBranches (coAxiomBranches con))
                (bad_ax (text "index out of range"))
       ; let CoAxBranch { cab_tvs   = ktvs
                        , cab_cvs   = cvs
                        , cab_roles = roles } = coAxiomNthBranch con ind
       ; unless (cos `equalLength` (ktvs ++ cvs)) $
           bad_ax (text "lengths")
       ; cos' <- mapM lintCoercion cos
       ; subst <- getTCvSubst
       ; let empty_subst = zapTCvSubst subst
       ; _ <- foldlM check_ki (empty_subst, empty_subst)
                              (zip3 (ktvs ++ cvs) roles cos')
       ; let fam_tc = coAxiomTyCon con
       ; case checkAxInstCo co of
           Just bad_branch -> bad_ax $ text "inconsistent with" <+>
                                       pprCoAxBranch fam_tc bad_branch
           Nothing -> return ()
       ; return (AxiomInstCo con ind cos') }
  where
    bad_ax what = addErrL (hang (text  "Bad axiom application" <+> parens what)
                        2 (ppr co))

    check_ki (subst_l, subst_r) (ktv, role, arg')
      = do { let Pair s' t' = coercionKind arg'
                 sk' = typeKind s'
                 tk' = typeKind t'
           ; lintRole arg' role (coercionRole arg')
           ; let ktv_kind_l = substTy subst_l (tyVarKind ktv)
                 ktv_kind_r = substTy subst_r (tyVarKind ktv)
           ; unless (sk' `eqType` ktv_kind_l)
                    (bad_ax (text "check_ki1" <+> vcat [ ppr co, ppr sk', ppr ktv, ppr ktv_kind_l ] ))
           ; unless (tk' `eqType` ktv_kind_r)
                    (bad_ax (text "check_ki2" <+> vcat [ ppr co, ppr tk', ppr ktv, ppr ktv_kind_r ] ))
           ; return (extendTCvSubst subst_l ktv s',
                     extendTCvSubst subst_r ktv t') }

lintCoercion (KindCo co)
  = do { co' <- lintCoercion co
       ; return (KindCo co') }

lintCoercion (SubCo co')
  = do { co' <- lintCoercion co'
       ; lintRole co' Nominal (coercionRole co')
       ; return (SubCo co') }

lintCoercion this@(AxiomRuleCo ax cos)
  = do { cos' <- mapM lintCoercion cos
       ; lint_roles 0 (coaxrAsmpRoles ax) cos'
       ; case coaxrProves ax (map coercionKind cos') of
           Nothing -> err "Malformed use of AxiomRuleCo" [ ppr this ]
           Just _  -> return (AxiomRuleCo ax cos') }
  where
  err m xs  = failWithL $
              hang (text m) 2 $ vcat (text "Rule:" <+> ppr (coaxrName ax) : xs)

  lint_roles n (e : es) (co : cos)
    | e == coercionRole co = lint_roles (n+1) es cos
    | otherwise = err "Argument roles mismatch"
                      [ text "In argument:" <+> int (n+1)
                      , text "Expected:" <+> ppr e
                      , text "Found:" <+> ppr (coercionRole co) ]
  lint_roles _ [] []  = return ()
  lint_roles n [] rs  = err "Too many coercion arguments"
                          [ text "Expected:" <+> int n
                          , text "Provided:" <+> int (n + length rs) ]

  lint_roles n es []  = err "Not enough coercion arguments"
                          [ text "Expected:" <+> int (n + length es)
                          , text "Provided:" <+> int n ]

lintCoercion (HoleCo h)
  = do { addErrL $ text "Unfilled coercion hole:" <+> ppr h
       ; lintCoercion (CoVarCo (coHoleCoVar h)) }


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

       , le_subst :: TCvSubst  -- Current TyCo substitution
                               --    See Note [Linting type lets]
            -- /Only/ substitutes for type variables;
            --        but might clone CoVars
            -- We also use le_subst to keep track of
            -- in-scope TyVars and CoVars (but not Ids)
            -- Range of the TCvSubst is LintedType/LintedCo

       , le_ids   :: VarEnv (Id, LintedType)    -- In-scope Ids
            -- Used to check that occurrences have an enclosing binder.
            -- The Id is /pre-substitution/, used to check that
            -- the occurrence has an identical type to the binder
            -- The LintedType is used to return the type of the occurrence,
            -- without having to lint it again.

       , le_joins :: IdSet     -- Join points in scope that are valid
                               -- A subset of the InScopeSet in le_subst
                               -- See Note [Join points]

       , le_dynflags :: DynFlags     -- DynamicFlags
       }

data LintFlags
  = LF { lf_check_global_ids           :: Bool -- See Note [Checking for global Ids]
       , lf_check_inline_loop_breakers :: Bool -- See Note [Checking for INLINE loop breakers]
       , lf_check_static_ptrs :: StaticPtrCheck -- ^ See Note [Checking StaticPtrs]
       , lf_report_unsat_syns :: Bool -- ^ See Note [Linting type synonym applications]
       , lf_check_levity_poly :: Bool -- See Note [Checking for levity polymorphism]
    }

-- See Note [Checking StaticPtrs]
data StaticPtrCheck
    = AllowAnywhere
        -- ^ Allow 'makeStatic' to occur anywhere.
    | AllowAtTopLevel
        -- ^ Allow 'makeStatic' calls at the top-level only.
    | RejectEverywhere
        -- ^ Reject any 'makeStatic' occurrence.
  deriving Eq

defaultLintFlags :: LintFlags
defaultLintFlags = LF { lf_check_global_ids = False
                      , lf_check_inline_loop_breakers = True
                      , lf_check_static_ptrs = AllowAnywhere
                      , lf_report_unsat_syns = True
                      , lf_check_levity_poly = True
                      }

newtype LintM a =
   LintM { unLintM ::
            LintEnv ->
            WarnsAndErrs ->           -- Warning and error messages so far
            (Maybe a, WarnsAndErrs) } -- Result and messages (if any)
   deriving (Functor)

type WarnsAndErrs = (Bag MsgDoc, Bag MsgDoc)

{- Note [Checking for global Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before CoreTidy, all locally-bound Ids must be LocalIds, even
top-level ones. See Note [Exported LocalIds] and #9857.

Note [Checking StaticPtrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Grand plan for static forms] in StaticPtrTable for an overview.

Every occurrence of the function 'makeStatic' should be moved to the
top level by the FloatOut pass.  It's vital that we don't have nested
'makeStatic' occurrences after CorePrep, because we populate the Static
Pointer Table from the top-level bindings. See SimplCore Note [Grand
plan for static forms].

The linter checks that no occurrence is left behind, nested within an
expression. The check is enabled only after the FloatOut, CorePrep,
and CoreTidy passes and only if the module uses the StaticPointers
language extension. Checking more often doesn't help since the condition
doesn't hold until after the first FloatOut pass.

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

Note [Linting type synonym applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When linting a type-synonym, or type-family, application
  S ty1 .. tyn
we behave as follows (#15057, #T15664):

* If lf_report_unsat_syns = True, and S has arity < n,
  complain about an unsaturated type synonym or type family

* Switch off lf_report_unsat_syns, and lint ty1 .. tyn.

  Reason: catch out of scope variables or other ill-kinded gubbins,
  even if S discards that argument entirely. E.g. (#15012):
     type FakeOut a = Int
     type family TF a
     type instance TF Int = FakeOut a
  Here 'a' is out of scope; but if we expand FakeOut, we conceal
  that out-of-scope error.

  Reason for switching off lf_report_unsat_syns: with
  LiberalTypeSynonyms, GHC allows unsaturated synonyms provided they
  are saturated when the type is expanded. Example
     type T f = f Int
     type S a = a -> a
     type Z = T S
  In Z's RHS, S appears unsaturated, but it is saturated when T is expanded.

* If lf_report_unsat_syns is on, expand the synonym application and
  lint the result.  Reason: want to check that synonyms are saturated
  when the type is expanded.
-}

instance Applicative LintM where
      pure x = LintM $ \ _ errs -> (Just x, errs)
      (<*>) = ap

instance Monad LintM where
  m >>= k  = LintM (\ env errs ->
                       let (res, errs') = unLintM m env errs in
                         case res of
                           Just r -> unLintM (k r) env errs'
                           Nothing -> (Nothing, errs'))

instance MonadFail LintM where
    fail err = failWithL (text err)

instance HasDynFlags LintM where
  getDynFlags = LintM (\ e errs -> (Just (le_dynflags e), errs))

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | OccOf Id            -- Occurrence of id
  | LambdaBodyOf Id     -- The lambda-binder
  | UnfoldingOf Id      -- Unfolding of a binder
  | BodyOfLetRec [Id]   -- One of the binders
  | CaseAlt CoreAlt     -- Case alternative
  | CasePat CoreAlt     -- The *pattern* of the case alternative
  | CaseTy CoreExpr     -- The type field of a case expression
                        -- with this scrutinee
  | IdTy Id             -- The type field of an Id binder
  | AnExpr CoreExpr     -- Some expression
  | ImportedUnfolding SrcLoc -- Some imported unfolding (ToDo: say which)
  | TopLevelBindings
  | InType Type         -- Inside a type
  | InCo   Coercion     -- Inside a coercion

initL :: DynFlags -> LintFlags -> [Var]
       -> LintM a -> WarnsAndErrs    -- Warnings and errors
initL dflags flags vars m
  = case unLintM m env (emptyBag, emptyBag) of
      (Just _, errs) -> errs
      (Nothing, errs@(_, e)) | not (isEmptyBag e) -> errs
                             | otherwise -> pprPanic ("Bug in Lint: a failure occurred " ++
                                                      "without reporting an error message") empty
  where
    (tcvs, ids) = partition isTyCoVar vars
    env = LE { le_flags = flags
             , le_subst = mkEmptyTCvSubst (mkInScopeSet (mkVarSet tcvs))
             , le_ids   = mkVarEnv [(id, (id,idType id)) | id <- ids]
             , le_joins = emptyVarSet
             , le_loc = []
             , le_dynflags = dflags }

setReportUnsat :: Bool -> LintM a -> LintM a
-- Switch off lf_report_unsat_syns
setReportUnsat ru thing_inside
  = LintM $ \ env errs ->
    let env' = env { le_flags = (le_flags env) { lf_report_unsat_syns = ru } }
    in unLintM thing_inside env' errs

-- See Note [Checking for levity polymorphism]
noLPChecks :: LintM a -> LintM a
noLPChecks thing_inside
  = LintM $ \env errs ->
    let env' = env { le_flags = (le_flags env) { lf_check_levity_poly = False } }
    in unLintM thing_inside env' errs

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
                (Nothing, (warns, addMsg True env errs msg))

addErrL :: MsgDoc -> LintM ()
addErrL msg = LintM $ \ env (warns,errs) ->
              (Just (), (warns, addMsg True env errs msg))

addWarnL :: MsgDoc -> LintM ()
addWarnL msg = LintM $ \ env (warns,errs) ->
              (Just (), (addMsg False env warns msg, errs))

addMsg :: Bool -> LintEnv ->  Bag MsgDoc -> MsgDoc -> Bag MsgDoc
addMsg is_error env msgs msg
  = ASSERT2( notNull loc_msgs, msg )
    msgs `snocBag` mk_msg msg
  where
   loc_msgs :: [(SrcLoc, SDoc)]  -- Innermost first
   loc_msgs = map dumpLoc (le_loc env)

   cxt_doc = vcat [ vcat $ reverse $ map snd loc_msgs
                  , text "Substitution:" <+> ppr (le_subst env) ]
   context | is_error  = cxt_doc
           | otherwise = whenPprDebug cxt_doc
     -- Print voluminous info for Lint errors
     -- but not for warnings

   msg_span = case [ span | (loc,_) <- loc_msgs
                          , let span = srcLocSpan loc
                          , isGoodSrcSpan span ] of
               []    -> noSrcSpan
               (s:_) -> s
   mk_msg msg = mkLocMessage SevWarning msg_span
                             (msg $$ context)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m
  = LintM $ \ env errs ->
    unLintM m (env { le_loc = extra_loc : le_loc env }) errs

inCasePat :: LintM Bool         -- A slight hack; see the unique call site
inCasePat = LintM $ \ env errs -> (Just (is_case_pat env), errs)
  where
    is_case_pat (LE { le_loc = CasePat {} : _ }) = True
    is_case_pat _other                           = False

addInScopeId :: Id -> LintedType -> LintM a -> LintM a
addInScopeId id linted_ty m
  = LintM $ \ env@(LE { le_ids = id_set, le_joins = join_set }) errs ->
    unLintM m (env { le_ids   = extendVarEnv id_set id (id, linted_ty)
                   , le_joins = add_joins join_set }) errs
  where
    add_joins join_set
      | isJoinId id = extendVarSet join_set id -- Overwrite with new arity
      | otherwise   = delVarSet    join_set id -- Remove any existing binding

getInScopeIds :: LintM (VarEnv (Id,LintedType))
getInScopeIds = LintM (\env errs -> (Just (le_ids env), errs))

extendTvSubstL :: TyVar -> Type -> LintM a -> LintM a
extendTvSubstL tv ty m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = Type.extendTvSubst (le_subst env) tv ty }) errs

updateTCvSubst :: TCvSubst -> LintM a -> LintM a
updateTCvSubst subst' m
  = LintM $ \ env errs -> unLintM m (env { le_subst = subst' }) errs

markAllJoinsBad :: LintM a -> LintM a
markAllJoinsBad m
  = LintM $ \ env errs -> unLintM m (env { le_joins = emptyVarSet }) errs

markAllJoinsBadIf :: Bool -> LintM a -> LintM a
markAllJoinsBadIf True  m = markAllJoinsBad m
markAllJoinsBadIf False m = m

getValidJoins :: LintM IdSet
getValidJoins = LintM (\ env errs -> (Just (le_joins env), errs))

getTCvSubst :: LintM TCvSubst
getTCvSubst = LintM (\ env errs -> (Just (le_subst env), errs))

getInScope :: LintM InScopeSet
getInScope = LintM (\ env errs -> (Just (getTCvInScope $ le_subst env), errs))

lookupIdInScope :: Id -> LintM (Id, LintedType)
lookupIdInScope id_occ
  = do { in_scope_ids <- getInScopeIds
       ; case lookupVarEnv in_scope_ids id_occ of
           Just (id_bndr, linted_ty)
             -> do { checkL (not (bad_global id_bndr)) global_in_scope
                   ; return (id_bndr, linted_ty) }
           Nothing -> do { checkL (not is_local) local_out_of_scope
                         ; return (id_occ, idType id_occ) } }
                      -- We don't bother to lint the type
                      -- of global (i.e. imported) Ids
  where
    is_local = mustHaveLocalBinding id_occ
    local_out_of_scope = text "Out of scope:" <+> pprBndr LetBind id_occ
    global_in_scope    = hang (text "Occurrence is GlobalId, but binding is LocalId")
                            2 (pprBndr LetBind id_occ)
    bad_global id_bnd = isGlobalId id_occ
                     && isLocalId id_bnd
                     && not (isWiredIn id_occ)
       -- 'bad_global' checks for the case where an /occurrence/ is
       -- a GlobalId, but there is an enclosing binding fora a LocalId.
       -- NB: the in-scope variables are mostly LocalIds, checked by lintIdBndr,
       --     but GHCi adds GlobalIds from the interactive context.  These
       --     are fine; hence the test (isLocalId id == isLocalId v)
       -- NB: when compiling Control.Exception.Base, things like absentError
       --     are defined locally, but appear in expressions as (global)
       --     wired-in Ids after worker/wrapper
       --     So we simply disable the test in this case

lookupJoinId :: Id -> LintM (Maybe JoinArity)
-- Look up an Id which should be a join point, valid here
-- If so, return its arity, if not return Nothing
lookupJoinId id
  = do { join_set <- getValidJoins
       ; case lookupVarSet join_set id of
            Just id' -> return (isJoinId_maybe id')
            Nothing  -> return Nothing }

ensureEqTys :: LintedType -> LintedType -> MsgDoc -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have already had the substitution applied
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
  = (getSrcLoc v, text "In the RHS of" <+> pp_binders [v])

dumpLoc (OccOf v)
  = (getSrcLoc v, text "In an occurrence of" <+> pp_binder v)

dumpLoc (LambdaBodyOf b)
  = (getSrcLoc b, text "In the body of lambda with binder" <+> pp_binder b)

dumpLoc (UnfoldingOf b)
  = (getSrcLoc b, text "In the unfolding of" <+> pp_binder b)

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, text "In body of a letrec with no binders")

dumpLoc (BodyOfLetRec bs@(_:_))
  = ( getSrcLoc (head bs), text "In the body of letrec with binders" <+> pp_binders bs)

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (con, args, _))
  = (noSrcLoc, text "In a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (CasePat (con, args, _))
  = (noSrcLoc, text "In the pattern of a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (CaseTy scrut)
  = (noSrcLoc, hang (text "In the result-type of a case with scrutinee:")
                  2 (ppr scrut))

dumpLoc (IdTy b)
  = (getSrcLoc b, text "In the type of a binder:" <+> ppr b)

dumpLoc (ImportedUnfolding locn)
  = (locn, text "In an imported unfolding")
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
  = hang (text "Case expression with DEFAULT not at the beginning") 4 (ppr e)
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

emptyRec :: CoreExpr -> MsgDoc
emptyRec e = hang (text "Empty Rec binding:") 2 (ppr e)

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

badBndrTyMsg :: Id -> SDoc -> MsgDoc
badBndrTyMsg binder what
  = vcat [ text "The type of this binder is" <+> what <> colon <+> ppr binder
         , text "Binder's type:" <+> ppr (idType binder) ]

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

mkTopNonLitStrMsg :: Id -> MsgDoc
mkTopNonLitStrMsg binder
  = hsep [text "Top-level Addr# binder has a non-literal rhs:", ppr binder]

mkKindErrMsg :: TyVar -> Type -> MsgDoc
mkKindErrMsg tyvar arg_ty
  = vcat [text "Kinds don't match in type application:",
          hang (text "Type variable:")
                 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
          hang (text "Arg type:")
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkCastErr :: CoreExpr -> Coercion -> Type -> Type -> MsgDoc
mkCastErr expr = mk_cast_err "expression" "type" (ppr expr)

mkCastTyErr :: Type -> Coercion -> Kind -> Kind -> MsgDoc
mkCastTyErr ty = mk_cast_err "type" "kind" (ppr ty)

mk_cast_err :: String -- ^ What sort of casted thing this is
                      --   (\"expression\" or \"type\").
            -> String -- ^ What sort of coercion is being used
                      --   (\"type\" or \"kind\").
            -> SDoc   -- ^ The thing being casted.
            -> Coercion -> Type -> Type -> MsgDoc
mk_cast_err thing_str co_str pp_thing co from_ty thing_ty
  = vcat [from_msg <+> text "of Cast differs from" <+> co_msg
            <+> text "of" <+> enclosed_msg,
          from_msg <> colon <+> ppr from_ty,
          text (capitalise co_str) <+> text "of" <+> enclosed_msg <> colon
            <+> ppr thing_ty,
          text "Actual" <+> enclosed_msg <> colon <+> pp_thing,
          text "Coercion used in cast:" <+> ppr co
         ]
  where
    co_msg, from_msg, enclosed_msg :: SDoc
    co_msg       = text co_str
    from_msg     = text "From-" <> co_msg
    enclosed_msg = text "enclosed" <+> text thing_str

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

mkBadJoinBindMsg :: Var -> SDoc
mkBadJoinBindMsg var
  = vcat [ text "Bad join point binding:" <+> ppr var
         , text "Join points can be bound only by a non-top-level let" ]

mkInvalidJoinPointMsg :: Var -> Type -> SDoc
mkInvalidJoinPointMsg var ty
  = hang (text "Join point has invalid type:")
        2 (ppr var <+> dcolon <+> ppr ty)

mkBadJoinArityMsg :: Var -> Int -> Int -> CoreExpr -> SDoc
mkBadJoinArityMsg var ar nlams rhs
  = vcat [ text "Join point has too few lambdas",
           text "Join var:" <+> ppr var,
           text "Join arity:" <+> ppr ar,
           text "Number of lambdas:" <+> ppr nlams,
           text "Rhs = " <+> ppr rhs
           ]

invalidJoinOcc :: Var -> SDoc
invalidJoinOcc var
  = vcat [ text "Invalid occurrence of a join variable:" <+> ppr var
         , text "The binder is either not a join point, or not valid here" ]

mkBadJumpMsg :: Var -> Int -> Int -> SDoc
mkBadJumpMsg var ar nargs
  = vcat [ text "Join point invoked with wrong number of arguments",
           text "Join var:" <+> ppr var,
           text "Join arity:" <+> ppr ar,
           text "Number of arguments:" <+> int nargs ]

mkInconsistentRecMsg :: [Var] -> SDoc
mkInconsistentRecMsg bndrs
  = vcat [ text "Recursive let binders mix values and join points",
           text "Binders:" <+> hsep (map ppr_with_details bndrs) ]
  where
    ppr_with_details bndr = ppr bndr <> ppr (idDetails bndr)

mkJoinBndrOccMismatchMsg :: Var -> JoinArity -> JoinArity -> SDoc
mkJoinBndrOccMismatchMsg bndr join_arity_bndr join_arity_occ
  = vcat [ text "Mismatch in join point arity between binder and occurrence"
         , text "Var:" <+> ppr bndr
         , text "Arity at binding site:" <+> ppr join_arity_bndr
         , text "Arity at occurrence:  " <+> ppr join_arity_occ ]

mkBndrOccTypeMismatchMsg :: Var -> Var -> LintedType -> LintedType -> SDoc
mkBndrOccTypeMismatchMsg bndr var bndr_ty var_ty
  = vcat [ text "Mismatch in type between binder and occurrence"
         , text "Binder:" <+> ppr bndr <+> dcolon <+> ppr bndr_ty
         , text "Occurrence:" <+> ppr var <+> dcolon <+> ppr var_ty
         , text "  Before subst:" <+> ppr (idType var) ]

mkBadJoinPointRuleMsg :: JoinId -> JoinArity -> CoreRule -> SDoc
mkBadJoinPointRuleMsg bndr join_arity rule
  = vcat [ text "Join point has rule with wrong number of arguments"
         , text "Var:" <+> ppr bndr
         , text "Join arity:" <+> ppr join_arity
         , text "Rule:" <+> ppr rule ]

pprLeftOrRight :: LeftOrRight -> MsgDoc
pprLeftOrRight CLeft  = text "left"
pprLeftOrRight CRight = text "right"

dupVars :: [NonEmpty Var] -> MsgDoc
dupVars vars
  = hang (text "Duplicate variables brought into scope")
       2 (ppr (map toList vars))

dupExtVars :: [NonEmpty Name] -> MsgDoc
dupExtVars vars
  = hang (text "Duplicate top-level variables with the same qualified name")
       2 (ppr (map toList vars))

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
    when (not (null diffs)) $ GHC.Core.Op.Monad.putMsg $ vcat
      [ lint_banner "warning" pname
      , text "Core changes with annotations:"
      , withPprStyle (defaultDumpStyle dflags) $ nest 2 $ vcat diffs
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
          -- TODO: supply tag here as well ?
        liftIO =<< runCoreM <$> fmap removeFlag getHscEnv <*> getRuleBase <*>
                                getUniqMask <*> getModule <*>
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
