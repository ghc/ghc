{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


A ``lint'' pass to check for Core correctness.
See Note [Core Lint guarantee].
-}

module GHC.Core.Lint (
    LintPassResultConfig (..),
    LintFlags (..),
    StaticPtrCheck (..),
    LintConfig (..),
    WarnsAndErrs,

    lintCoreBindings', lintUnfolding,
    lintPassResult, lintExpr,
    lintAnnots, lintAxioms,

    -- ** Debug output
    EndPassConfig (..),
    endPassIO,
    displayLintResults, dumpPassResult
 ) where

import GHC.Prelude

import GHC.Driver.DynFlags

import GHC.Tc.Utils.TcType
  ( ConcreteTvOrigin(..), ConcreteTyVars
  , isFloatingPrimTy, isTyFamFree )
import GHC.Tc.Types.Origin
  ( FixedRuntimeRepOrigin(..) )
import GHC.Unit.Module.ModGuts
import GHC.Platform

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.Stats ( coreBindsStats )
import GHC.Core.DataCon
import GHC.Core.Ppr
import GHC.Core.Coercion
import GHC.Core.Type as Type
import GHC.Core.Predicate( isCoVarType )
import GHC.Core.Multiplicity
import GHC.Core.UsageEnv
import GHC.Core.TyCo.Rep   -- checks validity of types/coercions
import GHC.Core.TyCo.Compare ( eqType, eqTypes, eqTypeIgnoringMultiplicity, eqForAllVis )
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCon as TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.FamInstEnv( compatibleBranches )
import GHC.Core.Unify
import GHC.Core.Opt.Arity    ( typeArity, exprIsDeadEnd )

import GHC.Core.Opt.Monad

import GHC.Types.Literal
import GHC.Types.Var as Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.SrcLoc
import GHC.Types.Tickish
import GHC.Types.Unique.FM ( isNullUFM, sizeUFM )
import GHC.Types.RepType
import GHC.Types.Basic
import GHC.Types.Demand      ( splitDmdSig, isDeadEndDiv )

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim

import GHC.Data.Bag
import GHC.Data.List.SetOps

import GHC.Utils.Monad
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Error
import qualified GHC.Utils.Error as Err
import GHC.Utils.Logger

import GHC.Data.Pair
import GHC.Base (oneShot)
import GHC.Data.Unboxed

import Control.Monad
import Data.Foldable      ( for_, toList )
import Data.List.NonEmpty ( NonEmpty(..), groupWith, nonEmpty )
import Data.Maybe
import Data.IntMap.Strict ( IntMap )
import qualified Data.IntMap.Strict as IntMap ( lookup, keys, empty, fromList )

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

Note [Linting function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All saturated applications of funTyCon are represented with the FunTy constructor.
See Note [Function type constructors and FunTy] in GHC.Builtin.Types.Prim

 We check this invariant in lintType.

Note [Linting type lets]
~~~~~~~~~~~~~~~~~~~~~~~~
In the desugarer, it's very very convenient to be able to say (in effect)
        let a = Type Bool in
        let x::a = True in <body>
That is, use a type let.  See Note [Core type and coercion invariant] in "GHC.Core".
One place it is used is in mkWwBodies; see Note [Join points and beta-redexes]
in GHC.Core.Opt.WorkWrap.Utils.  (Maybe there are other "clients" of this feature; I'm not sure).

* Hence when linting <body> we need to remember that a=Int, else we
  might reject a correct program.  So we carry a type substitution (in
  this example [a -> Bool]) and apply this substitution before
  comparing types. In effect, in Lint, type equality is always
  equality-modulo-le-subst.  This is in the le_subst field of
  LintEnv.  But nota bene:

  (SI1) The le_subst substitution is applied to types and coercions only

  (SI2) The result of that substitution is used only to check for type
        equality, to check well-typed-ness, /but is then discarded/.
        The result of substitution does not outlive the CoreLint pass.

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
  We decide to clone `cv_66` because it's already in scope.  Fine,
  choose a new unique.  Aha, X7 looks good.  So we check the lambda
  body with le_subst of [cv_66 :-> cv_X7]

  This is all fine, even though we use the same unique as wild_X7.
  As (SI2) says, we do /not/ return a new lambda
     (\cv_X7 -> let wild_X7 = blah in ...)
  We simply use the le_subst substitution in types/coercions only, when
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

Note [Avoiding compiler perf traps when constructing error messages.]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's quite common to put error messages into a where clause when it might
be triggered by multiple branches. E.g.

  checkThing x y z =
    case x of
      X -> unless (correctX x) $ failWithL errMsg
      Y -> unless (correctY y) $ failWithL errMsg
    where
      errMsg = text "My error involving:" $$ ppr x <+> ppr y

However ghc will compile this to:

  checkThink x y z =
    let errMsg = text "My error involving:" $$ ppr x <+> ppr y
    in case x of
      X -> unless (correctX x) $ failWithL errMsg
      Y -> unless (correctY y) $ failWithL errMsg

Putting the allocation of errMsg into the common non-error path.
One way to work around this is to turn errMsg into a function:

  checkThink x y z =
    case x of
      X -> unless (correctX x) $ failWithL (errMsg x y)
      Y -> unless (correctY y) $ failWithL (errMsg x y)
    where
      errMsg x y = text "My error involving:" $$ ppr x <+> ppr y

This way `errMsg` is a static function and it being defined in the common
path does not result in allocation in the hot path. This can be surprisingly
impactful. Changing `lint_app` reduced allocations for one test program I was
looking at by ~4%.

Note [MCInfo for Lint]
~~~~~~~~~~~~~~~~~~~~~~
When printing a Lint message, use the MCInfo severity so that the
message is printed on stderr rather than stdout (#13342).

************************************************************************
*                                                                      *
                 Beginning and ending passes
*                                                                      *
************************************************************************
-}

-- | Configuration for boilerplate operations at the end of a
-- compilation pass producing Core.
data EndPassConfig = EndPassConfig
  { ep_dumpCoreSizes :: !Bool
  -- ^ Whether core bindings should be dumped with the size of what they
  -- are binding (i.e. the size of the RHS of the binding).

  , ep_lintPassResult :: !(Maybe LintPassResultConfig)
  -- ^ Whether we should lint the result of this pass.

  , ep_namePprCtx :: !NamePprCtx

  , ep_dumpFlag :: !(Maybe DumpFlag)

  , ep_prettyPass :: !SDoc

  , ep_passDetails :: !SDoc
  }

endPassIO :: Logger
          -> EndPassConfig
          -> CoreProgram -> [CoreRule]
          -> IO ()
-- Used by the IO-is CorePrep too
endPassIO logger cfg binds rules
  = do { dumpPassResult logger (ep_dumpCoreSizes cfg) (ep_namePprCtx cfg) mb_flag
                        (renderWithContext defaultSDocContext (ep_prettyPass cfg))
                        (ep_passDetails cfg) binds rules
       ; for_ (ep_lintPassResult cfg) $ \lp_cfg ->
           lintPassResult logger lp_cfg binds
       }
  where
    mb_flag = case ep_dumpFlag cfg of
                Just flag | logHasDumpFlag logger flag                    -> Just flag
                          | logHasDumpFlag logger Opt_D_verbose_core2core -> Just flag
                _ -> Nothing

dumpPassResult :: Logger
               -> Bool                  -- dump core sizes?
               -> NamePprCtx
               -> Maybe DumpFlag        -- Just df => show details in a file whose
                                        --            name is specified by df
               -> String                -- Header
               -> SDoc                  -- Extra info to appear after header
               -> CoreProgram -> [CoreRule]
               -> IO ()
dumpPassResult logger dump_core_sizes name_ppr_ctx mb_flag hdr extra_info binds rules
  = do { forM_ mb_flag $ \flag -> do
           logDumpFile logger (mkDumpStyle name_ppr_ctx) flag hdr FormatCore dump_doc

         -- Report result size
         -- This has the side effect of forcing the intermediate to be evaluated
         -- if it's not already forced by a -ddump flag.
       ; Err.debugTraceMsg logger 2 size_doc
       }

  where
    size_doc = sep [text "Result size of" <+> text hdr, nest 2 (equals <+> ppr (coreBindsStats binds))]

    dump_doc  = vcat [ nest 2 extra_info
                     , size_doc
                     , blankLine
                     , if dump_core_sizes
                        then pprCoreBindingsWithSize binds
                        else pprCoreBindings         binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]

{-
************************************************************************
*                                                                      *
                 Top-level interfaces
*                                                                      *
************************************************************************
-}

data LintPassResultConfig = LintPassResultConfig
  { lpr_diagOpts         :: !DiagOpts
  , lpr_platform         :: !Platform
  , lpr_makeLintFlags    :: !LintFlags
  , lpr_showLintWarnings :: !Bool
  , lpr_passPpr          :: !SDoc
  , lpr_localsInScope    :: ![Var]
  }

lintPassResult :: Logger -> LintPassResultConfig
               -> CoreProgram -> IO ()
lintPassResult logger cfg binds
  = do { let warns_and_errs = lintCoreBindings'
               (LintConfig
                { l_diagOpts = lpr_diagOpts cfg
                , l_platform = lpr_platform cfg
                , l_flags    = lpr_makeLintFlags cfg
                , l_vars     = lpr_localsInScope cfg
                })
               binds
       ; Err.showPass logger $
           "Core Linted result of " ++
           renderWithContext defaultSDocContext (lpr_passPpr cfg)
       ; displayLintResults logger
                            (lpr_showLintWarnings cfg) (lpr_passPpr cfg)
                            (pprCoreBindings binds) warns_and_errs
       }

displayLintResults :: Logger
                   -> Bool -- ^ If 'True', display linter warnings.
                           --   If 'False', ignore linter warnings.
                   -> SDoc -- ^ The source of the linted program
                   -> SDoc -- ^ The linted program, pretty-printed
                   -> WarnsAndErrs
                   -> IO ()
displayLintResults logger display_warnings pp_what pp_pgm (warns, errs)
  | not (isEmptyBag errs)
  = do { logMsg logger Err.MCInfo noSrcSpan  -- See Note [MCInfo for Lint]
           $ withPprStyle defaultDumpStyle
           (vcat [ lint_banner "errors" pp_what, Err.pprMessageBag errs
                 , text "*** Offending Program ***"
                 , pp_pgm
                 , text "*** End of Offense ***" ])
       ; Err.ghcExit logger 1 }

  | not (isEmptyBag warns)
  , log_enable_debug (logFlags logger)
  , display_warnings
  = logMsg logger Err.MCInfo noSrcSpan  -- See Note [MCInfo for Lint]
      $ withPprStyle defaultDumpStyle
        (lint_banner "warnings" pp_what $$ Err.pprMessageBag (mapBag ($$ blankLine) warns))

  | otherwise = return ()

lint_banner :: String -> SDoc -> SDoc
lint_banner string pass = text "*** Core Lint"      <+> text string
                          <+> text ": in result of" <+> pass
                          <+> text "***"

-- | Type-check a 'CoreProgram'. See Note [Core Lint guarantee].
lintCoreBindings' :: LintConfig -> CoreProgram -> WarnsAndErrs
--   Returns (warnings, errors)
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreBindings' cfg binds
  = initL cfg $
    addLoc TopLevelBindings           $
    do { -- Check that all top-level binders are distinct
         -- We do not allow  [NonRec x=1, NonRec y=x, NonRec x=2]
         -- because of glomming; see Note [Glomming] in GHC.Core.Opt.OccurAnal
         checkL (null dups) (dupVars dups)

         -- Check for External top level binders with the same M.n name
       ; checkL (null ext_dups) (dupExtVars ext_dups)

         -- Typecheck the bindings
       ; lintRecBindings TopLevel all_pairs $ \_ ->
         return () }
  where
    all_pairs = flattenBinds binds
     -- Put all the top-level binders in scope at the start
     -- This is because rewrite rules can bring something
     -- into use 'unexpectedly'; see Note [Glomming] in "GHC.Core.Opt.OccurAnal"
    binders = map fst all_pairs

    (_, dups) = removeDups compare binders

    -- ext_dups checks for names with different uniques
    -- but the same External name M.n.  We don't
    -- allow this at top level:
    --    M.n{r3}  = ...
    --    M.n{r29} = ...
    -- because they both get the same linker symbol
    ext_dups = snd $ removeDupsOn ord_ext $
               filter isExternalName $ map Var.varName binders
    ord_ext n = (nameModule n, nameOccName n)

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

lintUnfolding :: Bool             -- ^ True <=> is a compulsory unfolding
              -> LintConfig
              -> SrcLoc
              -> CoreExpr
              -> Maybe (Bag SDoc) -- Nothing => OK

lintUnfolding is_compulsory cfg locn expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just errs
  where
    (_warns, errs) = initL cfg $
                     if is_compulsory
                       -- See Note [Checking for representation polymorphism]
                     then noFixedRuntimeRepChecks linter
                     else linter
    linter = addLoc (ImportedUnfolding locn) $
             lintCoreExpr expr

lintExpr :: LintConfig
         -> CoreExpr
         -> Maybe (Bag SDoc)  -- Nothing => OK

lintExpr cfg expr
  | isEmptyBag errs = Nothing
  | otherwise       = Just errs
  where
    (_warns, errs) = initL cfg linter
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

-- Returns a UsageEnv because this function is called in lintCoreExpr for
-- Let

lintRecBindings :: TopLevelFlag -> [(Id, CoreExpr)]
                -> ([OutId] -> LintM a) -> LintM (a, [UsageEnv])
lintRecBindings top_lvl pairs thing_inside
  = lintIdBndrs top_lvl bndrs $ \ bndrs' ->
    do { ues <- zipWithM lint_pair bndrs' rhss
       ; a <- thing_inside bndrs'
       ; return (a, ues) }
  where
    (bndrs, rhss) = unzip pairs
    lint_pair bndr' rhs
      = addLoc (RhsOf bndr') $
        do { (rhs_ty, ue) <- lintRhs bndr' rhs         -- Check the rhs
           ; lintLetBind top_lvl Recursive bndr' rhs rhs_ty
           ; return ue }

lintLetBody :: LintLocInfo -> [OutId] -> CoreExpr -> LintM (OutType, UsageEnv)
lintLetBody loc bndrs body
  = do { (body_ty, body_ue) <- addLoc loc (lintCoreExpr body)
       ; mapM_ (lintJoinBndrType body_ty) bndrs
       ; return (body_ty, body_ue) }

lintLetBind :: TopLevelFlag -> RecFlag -> OutId
              -> CoreExpr -> OutType -> LintM ()
-- Binder's type, and the RHS, have already been linted
-- This function checks other invariants
lintLetBind top_lvl rec_flag binder rhs rhs_ty
  = do { let binder_ty = idType binder
       ; ensureEqTys binder_ty rhs_ty (mkRhsMsg binder (text "RHS") rhs_ty)

       -- If the binding is for a CoVar, the RHS should be (Coercion co)
       -- See Note [Core type and coercion invariant] in GHC.Core
       ; checkL (not (isCoVar binder) || isCoArg rhs)
                (mkLetErr binder rhs)

        -- Check the let-can-float invariant
        -- See Note [Core let-can-float invariant] in GHC.Core
       ; checkL ( isJoinId binder
               || mightBeLiftedType binder_ty
               || (isNonRec rec_flag && exprOkForSpeculation rhs)
               || isDataConWorkId binder || isDataConWrapId binder -- until #17521 is fixed
               || exprIsTickedString rhs)
           (badBndrTyMsg binder (text "unlifted"))

        -- Check that if the binder is at the top level and has type Addr#,
        -- that it is a string literal.
        -- See Note [Core top-level string literals].
       ; checkL (not (isTopLevel top_lvl && binder_ty `eqType` addrPrimTy)
                 || exprIsTickedString rhs)
           (mkTopNonLitStrMsg binder)

       ; flags <- getLintFlags

         -- Check that a join-point binder has a valid type
         -- NB: lintIdBinder has checked that it is not top-level bound
       ; case idJoinPointHood binder of
            NotJoinPoint    -> return ()
            JoinPoint arity ->  checkL (isValidJoinPointType arity binder_ty)
                                       (mkInvalidJoinPointMsg binder binder_ty)

       ; when (lf_check_inline_loop_breakers flags
               && isStableUnfolding (realIdUnfolding binder)
               && isStrongLoopBreaker (idOccInfo binder)
               && isInlinePragma (idInlinePragma binder))
              (addWarnL (text "INLINE binder is (non-rule) loop breaker:" <+> ppr binder))
              -- Only non-rule loop breakers inhibit inlining

       -- We used to check that the dmdTypeDepth of a demand signature never
       -- exceeds idArity, but that is an unnecessary complication, see
       -- Note [idArity varies independently of dmdTypeDepth] in GHC.Core.Opt.DmdAnal

       -- Check that the binder's arity is within the bounds imposed by the type
       -- and the strictness signature. See Note [Arity invariants for bindings]
       -- and Note [Trimming arity]

       ; checkL (typeArity (idType binder) >= idArity binder)
           (text "idArity" <+> ppr (idArity binder) <+>
           text "exceeds typeArity" <+>
           ppr (typeArity (idType binder)) <> colon <+>
           ppr binder)

       -- See Note [idArity varies independently of dmdTypeDepth]
       --     in GHC.Core.Opt.DmdAnal
       ; case splitDmdSig (idDmdSig binder) of
           (demands, result_info) | isDeadEndDiv result_info ->
              if (demands `lengthAtLeast` idArity binder)
              then return ()
              else pprTrace "Hack alert: lintLetBind #24623"
                       (ppr (idArity binder) $$ ppr (idDmdSig binder)) $
                   return ()
--             checkL (demands `lengthAtLeast` idArity binder)
--               (text "idArity" <+> ppr (idArity binder) <+>
--               text "exceeds arity imposed by the strictness signature" <+>
--               ppr (idDmdSig binder) <> colon <+>
--               ppr binder)

           _ -> return ()

       ; addLoc (RuleOf binder) $ mapM_ (lintCoreRule binder binder_ty) (idCoreRules binder)

       ; addLoc (UnfoldingOf binder) $
         lintIdUnfolding binder binder_ty (idUnfolding binder)
       ; return () }

        -- We should check the unfolding, if any, but this is tricky because
        -- the unfolding is a SimplifiableCoreExpr. Give up for now.

-- | Checks the RHS of bindings. It only differs from 'lintCoreExpr'
-- in that it doesn't reject occurrences of the function 'makeStatic' when they
-- appear at the top level and @lf_check_static_ptrs == AllowAtTopLevel@, and
-- for join points, it skips the outer lambdas that take arguments to the
-- join point.
--
-- See Note [Checking StaticPtrs].
lintRhs :: Id -> CoreExpr -> LintM (OutType, UsageEnv)
-- NB: the Id can be Linted or not -- it's only used for
--     its OccInfo and join-pointer-hood
lintRhs bndr rhs
    | JoinPoint arity <- idJoinPointHood bndr
    = lintJoinLams arity (Just bndr) rhs
    | AlwaysTailCalled arity <- tailCallInfo (idOccInfo bndr)
    = lintJoinLams arity Nothing rhs

-- Allow applications of the data constructor @StaticPtr@ at the top
-- but produce errors otherwise.
lintRhs _bndr rhs = fmap lf_check_static_ptrs getLintFlags >>= go
  where
    -- Allow occurrences of 'makeStatic' at the top-level but produce errors
    -- otherwise.
    go :: StaticPtrCheck -> LintM (OutType, UsageEnv)
    go AllowAtTopLevel
      | (binders0, rhs') <- collectTyBinders rhs
      , Just (fun, t, info, e) <- collectMakeStaticArgs rhs'
      = markAllJoinsBad $
        foldr
        -- imitate @lintCoreExpr (Lam ...)@
        lintLambda
        -- imitate @lintCoreExpr (App ...)@
        (do fun_ty_ue <- lintCoreExpr fun
            lintCoreArgs fun_ty_ue [Type t, info, e]
        )
        binders0
    go _ = markAllJoinsBad $ lintCoreExpr rhs

-- | Lint the RHS of a join point with expected join arity of @n@ (see Note
-- [Join points] in "GHC.Core").
lintJoinLams :: JoinArity -> Maybe Id -> CoreExpr -> LintM (OutType, UsageEnv)
lintJoinLams join_arity enforce rhs
  = go join_arity rhs
  where
    go 0 expr            = lintCoreExpr expr
    go n (Lam var body)  = lintLambda var $ go (n-1) body
    go n expr | Just bndr <- enforce -- Join point with too few RHS lambdas
              = failWithL $ mkBadJoinArityMsg bndr join_arity n rhs
              | otherwise -- Future join point, not yet eta-expanded
              = markAllJoinsBad $ lintCoreExpr expr
                -- Body of lambda is not a tail position

lintIdUnfolding :: Id -> Type -> Unfolding -> LintM ()
lintIdUnfolding bndr bndr_ty uf
  | isStableUnfolding uf
  , Just rhs <- maybeUnfoldingTemplate uf
  = do { ty <- fst <$> (if isCompulsoryUnfolding uf
                        then noFixedRuntimeRepChecks $ lintRhs bndr rhs
            --               ^^^^^^^^^^^^^^^^^^^^^^^
            -- See Note [Checking for representation polymorphism]
                        else lintRhs bndr rhs)
       ; ensureEqTys bndr_ty ty (mkRhsMsg bndr (text "unfolding") ty) }
lintIdUnfolding  _ _ _
  = return ()       -- Do not Lint unstable unfoldings, because that leads
                    -- to exponential behaviour; c.f. GHC.Core.FVs.idUnfoldingVars

{- Note [Checking for INLINE loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very suspicious if a strong loop breaker is marked INLINE.

However, the desugarer generates instance methods with INLINE pragmas
that form a mutually recursive group.  Only after a round of
simplification are they unravelled.  So we suppress the test for
the desugarer.  Here is an example:
  instance Eq T where
    t1 == t2 = blah
    t1 /= t2 = not (t1 == t2)
    {-# INLINE (/=) #-}

This will generate something like
    -- From the class decl for Eq
    data Eq a = EqDict (a->a->Bool) (a->a->Bool)
    eq_sel :: Eq a -> (a->a->Bool)
    eq_sel (EqDict eq _) = eq

    -- From the instance Eq T
    $ceq :: T -> T -> Bool
    $ceq = blah

    Rec { $dfEqT :: Eq T {-# DFunId #-}
          $dfEqT = EqDict $ceq $cnoteq

          $cnoteq :: T -> T -> Bool  {-# INLINE #-}
          $cnoteq x y = not (eq_sel $dfEqT x y) }

Notice that

* `$dfEqT` and `$cnotEq` are mutually recursive.

* We do not want `$dfEqT` to be the loop breaker: it's a DFunId, and
  we want to let it "cancel" with "eq_sel" (see Note [ClassOp/DFun
  selection] in GHC.Tc.TyCl.Instance, which it can't do if it's a loop
  breaker.

So we make `$cnoteq` into the loop breaker. That means it can't
inline, despite the INLINE pragma. That's what gives rise to the
warning, which is perfectly appropriate for, say
   Rec { {-# INLINE f #-}  f = \x -> ...f.... }
We can't inline a recursive function -- it's a loop breaker.

But now we can optimise `eq_sel $dfEqT` to `$ceq`, so we get
  Rec {
    $dfEqT :: Eq T {-# DFunId #-}
    $dfEqT = EqDict $ceq $cnoteq

    $cnoteq :: T -> T -> Bool  {-# INLINE #-}
    $cnoteq x y = not ($ceq x y) }

and now the dependencies of the Rec have gone, and we can split it up to give
    NonRec {  $dfEqT :: Eq T {-# DFunId #-}
              $dfEqT = EqDict $ceq $cnoteq }

    NonRec {  $cnoteq :: T -> T -> Bool  {-# INLINE #-}
              $cnoteq x y = not ($ceq x y) }

Now $cnoteq is not a loop breaker any more, so the INLINE pragma can
take effect -- the warning turned out to be temporary.

To stop excessive warnings, this warning for INLINE loop breakers is
switched off when linting the result of the desugarer.  See
lf_check_inline_loop_breakers in GHC.Core.Lint.


Note [Checking for representation polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We ordinarily want to check for bad representation polymorphism. See
Note [Representation polymorphism invariants] in GHC.Core. However, we do *not*
want to do this in a compulsory unfolding. Compulsory unfoldings arise
only internally, for things like newtype wrappers, dictionaries, and
(notably) unsafeCoerce#. These might legitimately be representation-polymorphic;
indeed representation-polymorphic unfoldings are a primary reason for the
very existence of compulsory unfoldings (we can't compile code for
the original, representation-polymorphic, binding).

It is vitally important that we do representation polymorphism checks *after*
performing the unfolding, but not beforehand. This is all safe because
we will check any unfolding after it has been unfolded; checking the
unfolding beforehand is merely an optimization, and one that actively
hurts us here.

Note [Linting of runRW#]
~~~~~~~~~~~~~~~~~~~~~~~~
runRW# has some very special behavior (see Note [runRW magic] in
GHC.CoreToStg.Prep) which CoreLint must accommodate, by allowing
join points in its argument.  For example, this is fine:

    join j x = ...
    in runRW#  (\s. case v of
                       A -> j 3
                       B -> j 4)

Usually those calls to the join point 'j' would not be valid tail calls,
because they occur in a function argument.  But in the case of runRW#
they are fine, because runRW# (\s.e) behaves operationally just like e.
(runRW# is ultimately inlined in GHC.CoreToStg.Prep.)

In the case that the continuation is /not/ a lambda we simply disable this
special behaviour.  For example, this is /not/ fine:

    join j = ...
    in runRW# @r @ty (jump j)

Note [Coercions in terms]
~~~~~~~~~~~~~~~~~~~~~~~~~
The expression (Type ty) can occur only as the argument of an application,
or the RHS of a non-recursive Let.  But what about (Coercion co)?

Currently it appears in ghc-prim:GHC.Types.coercible_sel, a WiredInId whose
definition is:
   coercible_sel :: Coercible a b => (a ~R# b)
   coercible_sel d = case d of
                         MkCoercibleDict (co :: a ~# b) -> Coercion co

So this function has a (Coercion co) in the alternative of a case.

Richard says (!11908): it shouldn't appear outside of arguments, but we've been
loose about this. coercible_sel is some thin ice. Really we should be unpacking
Coercible using case, not a selector. I recall looking into this a few years
back and coming to the conclusion that the fix was worse than the disease. Don't
remember the details, but could probably recover it if we want to revisit.

So Lint current accepts (Coercion co) in arbitrary places.  There is no harm in
that: it really is a value, albeit a zero-bit value.

************************************************************************
*                                                                      *
\subsection[lintCoreExpr]{lintCoreExpr}
*                                                                      *
************************************************************************
-}

lintCoreExpr :: InExpr -> LintM (OutType, UsageEnv)
-- The returned type has the substitution from the monad
-- already applied to it:
--      lintCoreExpr e subst = exprType (subst e)
--
-- The returned "type" can be a kind, if the expression is (Type ty)

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]

lintCoreExpr (Var var)
  = do {  var_pair@(var_ty, _) <- lintIdOcc var 0
           -- See Note [Linting representation-polymorphic builtins]
       ; checkRepPolyBuiltin (Var var) [] var_ty
           --checkDataToTagPrimOpTyCon (Var var) []
       ; return var_pair }

lintCoreExpr (Lit lit)
  = return (literalType lit, zeroUE)

lintCoreExpr (Cast expr co)
  = do { (expr_ty, ue) <- markAllJoinsBad (lintCoreExpr expr)
            -- markAllJoinsBad: see Note [Join points and casts]

       ; lintCoercion co
       ; lintRole co Representational (coercionRole co)
       ; Pair from_ty to_ty <- substCoKindM co
       ; checkValueType (typeKind to_ty) $
         text "target of cast" <+> quotes (ppr co)
       ; ensureEqTys from_ty expr_ty (mkCastErr expr co from_ty expr_ty)
       ; return (to_ty, ue) }

lintCoreExpr (Tick tickish expr)
  = do { case tickish of
           Breakpoint _ _ ids _ -> forM_ ids $ \id -> lintIdOcc id 0
           _                    -> return ()
       ; markAllJoinsBadIf block_joins $ lintCoreExpr expr }
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
    do  { ty' <- lintTypeAndSubst ty
        ; lintTyCoBndr tv              $ \ tv' ->
    do  { addLoc (RhsOf tv) $ lintTyKind tv' ty'
                -- Now extend the substitution so we
                -- take advantage of it in the body
        ; extendTvSubstL tv ty' $
          addLoc (BodyOfLet tv) $
          lintCoreExpr body } }

lintCoreExpr (Let (NonRec bndr rhs) body)
  | isId bndr
  = do { -- First Lint the RHS, before bringing the binder into scope
         (rhs_ty, let_ue) <- lintRhs bndr rhs

          -- See Note [Multiplicity of let binders] in Var
         -- Now lint the binder
       ; lintBinder LetBind bndr $ \bndr' ->
    do { lintLetBind NotTopLevel NonRecursive bndr' rhs rhs_ty
       ; addAliasUE bndr' let_ue $
         lintLetBody (BodyOfLet bndr') [bndr'] body } }

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

          -- See Note [Multiplicity of let binders] in Var
        ; ((body_type, body_ue), ues) <-
            lintRecBindings NotTopLevel pairs $ \ bndrs' ->
            lintLetBody (BodyOfLetRec bndrs') bndrs' body
        ; return (body_type, body_ue  `addUE` scaleUE ManyTy (foldr1WithDefault zeroUE addUE ues)) }
  where
    bndrs = map fst pairs

lintCoreExpr e@(App _ _)
  | Var fun <- fun
  , fun `hasKey` runRWKey
    -- See Note [Linting of runRW#]
    -- N.B. we may have an over-saturated application of the form:
    --   runRW (\s -> \x -> ...) y
  , ty_arg1 : ty_arg2 : cont_arg : rest <- args
  = do { let lint_rw_cont :: CoreArg -> Mult -> UsageEnv -> LintM (OutType, UsageEnv)
             lint_rw_cont expr@(Lam _ _) mult fun_ue
                = do { (arg_ty, arg_ue) <- lintJoinLams 1 (Just fun) expr
                     ; let app_ue = addUE fun_ue (scaleUE mult arg_ue)
                     ; return (arg_ty, app_ue) }

             lint_rw_cont expr mult ue
                = lintValArg expr mult ue
             -- TODO: Look through ticks?

       ; runrw_pr <- lintApp (text "runRW# expression")
                               lintTyArg lint_rw_cont
                               (idType fun) [ty_arg1,ty_arg2,cont_arg] zeroUE
       ; lintCoreArgs runrw_pr rest }

  | otherwise
  = do { fun_pair <- lintCoreFun fun (length args)
       ; app_pair@(app_ty, _) <- lintCoreArgs fun_pair args

       -- See Note [Linting representation-polymorphic builtins]
       ; checkRepPolyBuiltin fun args app_ty
       ; --checkDataToTagPrimOpTyCon fun args

       ; return app_pair}
  where
    skipTick t = case collectFunSimple e of
      (Var v) -> etaExpansionTick v t
      _ -> tickishFloatable t
    (fun, args, _source_ticks) = collectArgsTicks skipTick e
      -- We must look through source ticks to avoid #21152, for example:
      --
      -- reallyUnsafePtrEquality
      --   = \ @a ->
      --       (src<loc> reallyUnsafePtrEquality#)
      --         @Lifted @a @Lifted @a
      --
      -- To do this, we use `collectArgsTicks tickishFloatable` to match
      -- the eta expansion behaviour, as per Note [Eta expansion and source notes]
      -- in GHC.Core.Opt.Arity.
      -- Sadly this was not quite enough. So we now also accept things that CorePrep will allow.
      -- See Note [Ticks and mandatory eta expansion]

lintCoreExpr (Lam var expr)
  = markAllJoinsBad $
    lintLambda var $ lintCoreExpr expr

lintCoreExpr (Case scrut var alt_ty alts)
  = lintCaseExpr scrut var alt_ty alts

-- This case can't happen; linting types in expressions gets routed through lintTyArg
lintCoreExpr (Type ty)
  = failWithL (text "Type found as expression" <+> ppr ty)

lintCoreExpr (Coercion co)
  -- See Note [Coercions in terms]
  = do { addLoc (InCo co) $ lintCoercion co
       ; ty <- substTyM (coercionType co)
       ; return (ty, zeroUE) }

----------------------
lintIdOcc :: InId -> Int -- Number of arguments (type or value) being passed
          -> LintM (OutType, UsageEnv) -- returns type of the *variable*
lintIdOcc in_id nargs
  = addLoc (OccOf in_id) $
    do  { checkL (isNonCoVarId in_id)
                 (text "Non term variable" <+> ppr in_id)
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
        ; out_ty <- lintVarOcc in_id

          -- Check for a nested occurrence of the StaticPtr constructor.
          -- See Note [Checking StaticPtrs].
        ; lf <- getLintFlags
        ; when (nargs /= 0 && lf_check_static_ptrs lf /= AllowAnywhere) $
            checkL (idName in_id /= makeStaticName) $
              text "Found makeStatic nested in an expression"

        ; checkDeadIdOcc in_id

        ; case isDataConId_maybe in_id of
             Nothing -> return ()
             Just dc -> checkTypeDataConOcc "expression" dc

        ; checkJoinOcc in_id nargs
        ; usage <- varCallSiteUsage in_id

        ; return (out_ty, usage) }



lintCoreFun :: CoreExpr
            -> Int                          -- Number of arguments (type or val) being passed
            -> LintM (OutType, UsageEnv) -- Returns type of the *function*
lintCoreFun (Var var) nargs
  = lintIdOcc var nargs

lintCoreFun (Lam var body) nargs
  -- Act like lintCoreExpr of Lam, but *don't* call markAllJoinsBad;
  -- See Note [Beta redexes]
  | nargs /= 0
  = lintLambda var $ lintCoreFun body (nargs - 1)

lintCoreFun expr nargs
  = markAllJoinsBadIf (nargs /= 0) $
      -- See Note [Join points are less general than the paper]
    lintCoreExpr expr
------------------
lintLambda :: Var -> LintM (Type, UsageEnv) -> LintM (Type, UsageEnv)
lintLambda var lintBody =
    addLoc (LambdaBodyOf var) $
    lintBinder LambdaBind var $ \ var' ->
    do { (body_ty, ue) <- lintBody
       ; ue' <- checkLinearity ue var'
       ; return (mkLamType var' body_ty, ue') }
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
lintJoinBndrType :: OutType -- Type of the body
                 -> OutId   -- Possibly a join Id
                -> LintM ()
-- Checks that the return type of a join Id matches the body
-- E.g. join j x = rhs in body
--      The type of 'rhs' must be the same as the type of 'body'
lintJoinBndrType body_ty bndr
  | JoinPoint arity <- idJoinPointHood bndr
  , let bndr_ty = idType bndr
  , (bndrs, res) <- splitPiTys bndr_ty
  = checkL (length bndrs >= arity
            && body_ty `eqType` mkPiTys (drop arity bndrs) res) $
    hang (text "Join point returns different type than body")
       2 (vcat [ text "Join bndr:" <+> ppr bndr <+> dcolon <+> ppr (idType bndr)
               , text "Join arity:" <+> ppr arity
               , text "Body type:" <+> ppr body_ty ])
  | otherwise
  = return ()

checkJoinOcc :: Id -> JoinArity -> LintM ()
-- Check that if the occurrence is a JoinId, then so is the
-- binding site, and it's a valid join Id
checkJoinOcc var n_args
  | JoinPoint join_arity_occ <- idJoinPointHood var
  = do { mb_join_arity_bndr <- lookupJoinId var
       ; case mb_join_arity_bndr of {
           NotJoinPoint -> do { join_set <- getValidJoins
                              ; addErrL (text "join set " <+> ppr join_set $$
                                invalidJoinOcc var) } ;

           JoinPoint join_arity_bndr ->

    do { checkL (join_arity_bndr == join_arity_occ) $
           -- Arity differs at binding site and occurrence
         mkJoinBndrOccMismatchMsg var join_arity_bndr join_arity_occ

       ; checkL (n_args == join_arity_occ) $
           -- Arity doesn't match #args
         mkBadJumpMsg var join_arity_occ n_args } } }

  | otherwise
  = return ()

checkTypeDataConOcc :: String -> DataCon -> LintM ()
-- Check that the Id is not a data constructor of a `type data` declaration
-- Invariant (I1) of Note [Type data declarations] in GHC.Rename.Module
checkTypeDataConOcc what dc
  = checkL (not (isTypeDataTyCon (dataConTyCon dc))) $
    (text "type data constructor found in a" <+> text what <> colon <+> ppr dc)

{-
-- | Check that a use of a dataToTag# primop satisfies conditions DTT2
-- and DTT3 from Note [DataToTag overview] in GHC.Tc.Instance.Class
--
-- Ignores applications not headed by dataToTag# primops.

-- Commented out because GHC.PrimopWrappers doesn't respect this condition yet.
-- See wrinkle DTW7 in Note [DataToTag overview].
checkDataToTagPrimOpTyCon
  :: CoreExpr   -- ^ the function (head of the application) we are checking
  -> [CoreArg]  -- ^ The arguments to the application
  -> LintM ()
checkDataToTagPrimOpTyCon (Var fun_id) args
  | Just op <- isPrimOpId_maybe fun_id
  , op == DataToTagSmallOp || op == DataToTagLargeOp
  = case args of
      Type _levity : Type dty : _rest
        | Just (tc, _) <- splitTyConApp_maybe dty
        , isValidDTT2TyCon tc
          -> do  platform <- getPlatform
                 let  numConstrs = tyConFamilySize tc
                      isSmallOp = op == DataToTagSmallOp
                 checkL (isSmallFamily platform numConstrs == isSmallOp) $
                   text "dataToTag# primop-size/tycon-family-size mismatch"
        | otherwise -> failWithL $ text "dataToTagLarge# used at non-ADT type:"
                                   <+> ppr dty
      _ -> failWithL $ text "dataToTagLarge# needs two type arguments but has args:"
                       <+> ppr (take 2 args)

checkDataToTagPrimOpTyCon _ _ = pure ()
-}

-- | Check representation-polymorphic invariants in an application of a
-- built-in function or newtype constructor.
--
-- See Note [Linting representation-polymorphic builtins].
checkRepPolyBuiltin :: CoreExpr   -- ^ the function (head of the application) we are checking
                    -> [CoreArg]  -- ^ the arguments to the application
                    -> OutType -- ^ the instantiated type of the overall application
                    -> LintM ()
checkRepPolyBuiltin (Var fun_id) args app_ty
  = do { do_rep_poly_checks <- lf_check_fixed_rep <$> getLintFlags
       ; when (do_rep_poly_checks && hasNoBinding fun_id) $
           if
             -- (2) representation-polymorphic unlifted newtypes
             | Just dc <- isDataConId_maybe fun_id
             , isNewDataCon dc
             -> if tcHasFixedRuntimeRep $ dataConTyCon dc
                then return ()
                else checkRepPolyNewtypeApp dc args app_ty

             -- (1) representation-polymorphic builtins
             | otherwise
             -> checkRepPolyBuiltinApp fun_id args
       }
checkRepPolyBuiltin _ _ _ = return ()

checkRepPolyNewtypeApp :: DataCon -> [CoreArg] -> OutType -> LintM ()
checkRepPolyNewtypeApp nt args app_ty
  -- If the newtype is saturated, we're OK.
  | any isValArg args
  = return ()
  -- Otherwise, check we can eta-expand.
  | otherwise
  = case getRuntimeArgTys app_ty of
      (Scaled _ first_val_arg_ty, _):_
        | not $ typeHasFixedRuntimeRep first_val_arg_ty
        -> failWithL (err_msg first_val_arg_ty)
      _ -> return ()

  where

      err_msg :: Type -> SDoc
      err_msg bad_arg_ty
        = vcat [ text "Cannot eta expand unlifted newtype constructor" <+> quotes (ppr nt) <> dot
               , text "Its argument type does not have a fixed runtime representation:"
               , nest 2 $ ppr_ty_ki bad_arg_ty ]

      ppr_ty_ki :: Type -> SDoc
      ppr_ty_ki ty = bullet <+> ppr ty <+> dcolon <+> ppr (typeKind ty)

checkRepPolyBuiltinApp :: Id -> [CoreArg] -> LintM ()
checkRepPolyBuiltinApp fun_id args = checkL (null not_concs) err_msg
  where

    conc_binder_positions :: IntMap ConcreteTvOrigin
    conc_binder_positions
      = concreteTyVarPositions fun_id
      $ idDetailsConcreteTvs
      $ idDetails fun_id

    max_pos :: Int
    max_pos =
      case nonEmpty $ IntMap.keys conc_binder_positions of
        Nothing -> 0
        Just positions -> maximum positions

    not_concs :: [(SDoc, ConcreteTvOrigin)]
    not_concs =
      mapMaybe is_bad (zip [1..max_pos] (map Just args ++ repeat Nothing))
        -- NB: 1-indexed

    is_bad :: (Int, Maybe CoreArg) -> Maybe (SDoc, ConcreteTvOrigin)
    is_bad (pos, mb_arg)
      | Just conc_reason <- IntMap.lookup pos conc_binder_positions
      , Just bad_ty <- case mb_arg of
          Just (Type ki)
            | isConcreteType ki
            -> Nothing
            | otherwise
            -- Here we handle the situation in which a "must be concrete" TyVar
            -- has been instantiated with a type that is not concrete.
            -> Just $ quotes (ppr ki) <+> text "is not concrete."
          -- We expected a type argument in this position, and got something else: panic!
          Just arg ->
            pprPanic "checkRepPolyBuiltinApp: expected a type in this position" $
              vcat [ text "fun_id:" <+> ppr fun_id <+> dcolon <+> ppr (idType fun_id)
                   , text "pos:" <+> ppr pos
                   , text "arg:" <+> ppr arg ]
          Nothing ->
            -- Here we handle the situation in which a "must be concrete" TyVar
            -- has not been instantiated at all.
            case conc_reason of
              ConcreteFRR frr_orig ->
                let ty = frr_type frr_orig
                in  Just $ ppr ty <+> dcolon <+> ppr (typeKind ty)
      = Just (bad_ty, conc_reason)
      | otherwise
      = Nothing

    err_msg :: SDoc
    err_msg
      = vcat $ map ((bullet <+>) . ppr_not_conc) not_concs

    ppr_not_conc :: (SDoc, ConcreteTvOrigin) -> SDoc
    ppr_not_conc (bad_ty, conc) =
      vcat
       [ ppr_conc_orig conc
       , nest 2 bad_ty ]

    ppr_conc_orig :: ConcreteTvOrigin -> SDoc
    ppr_conc_orig (ConcreteFRR frr_orig) =
      case frr_orig of
        FixedRuntimeRepOrigin { frr_context = ctxt } ->
          hsep [ ppr ctxt, text "does not have a fixed runtime representation:" ]

-- | Compute the 1-indexed positions in the outer forall'd quantified type variables
-- of the type in which the concrete type variables occur.
--
-- See Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete.
concreteTyVarPositions :: Id -> ConcreteTyVars -> IntMap ConcreteTvOrigin
concreteTyVarPositions fun_id conc_tvs
  | isNullUFM conc_tvs
  = IntMap.empty
  | otherwise
  = case splitForAllTyCoVars (idType fun_id) of
    ([], _)  -> IntMap.empty
    (tvs, _) ->
      let positions =
            IntMap.fromList
              [ (pos, conc_orig)
              | (tv, pos) <- zip tvs [1..]
              , conc_orig <- maybeToList $ lookupNameEnv conc_tvs (tyVarName tv)
              ]
         -- Assert that we have as many positions as concrete type variables,
         -- i.e. we are not missing any concreteness information.
      in assertPpr (sizeUFM conc_tvs == length positions)
           (vcat [ text "concreteTyVarPositions: missing concreteness information"
                 , text "fun_id:" <+> ppr fun_id
                 , text "tvs:" <+> ppr tvs
                 , text "Expected # of concrete tvs:" <+> ppr (sizeUFM conc_tvs)
                 , text "  Actual # of concrete tvs:" <+> ppr (length positions) ])
           positions

-- Check that the usage of var is consistent with var itself, and pop the var
-- from the usage environment (this is important because of shadowing).
checkLinearity :: UsageEnv -> OutVar -> LintM UsageEnv
checkLinearity body_ue lam_var =
  case varMultMaybe lam_var of
    Just mult -> do
      let (lhs, body_ue') = popUE body_ue lam_var
          err_msg = vcat [ text "Linearity failure in lambda:" <+> ppr lam_var
                         , ppr lhs <+> text "⊈" <+> ppr mult
                         , ppr body_ue ]
      ensureSubUsage lhs mult err_msg
      return body_ue'
    Nothing    -> return body_ue -- A type variable

{- Note [Join points and casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that this should be OK:
   join j x = rhs
   in (case e of
          A   -> alt1
          B x -> (jump j x) |> co)

You might think that, since the cast is ultimately erased, the jump to
`j` should still be OK as a join point.  But no!  See #21716. Suppose

  newtype Age = MkAge Int   -- axAge :: Age ~ Int
  f :: Int -> ...           -- f strict in it's first argument

and consider the expression

  f (join j :: Bool -> Age
          j x = (rhs1 :: Age)
     in case v of
         Just x  -> (j x |> axAge :: Int)
         Nothing -> rhs2)

Then, if the Simplifier pushes the strict call into the join points
and alternatives we'll get

   join j' x = f (rhs1 :: Age)
   in case v of
      Just x  -> j' x |> axAge
      Nothing -> f rhs2

Utterly bogus.  `f` expects an `Int` and we are giving it an `Age`.
No no no.  Casts destroy the tail-call property.  Henc markAllJoinsBad
in the (Cast expr co) case of lintCoreExpr.

Note [No alternatives lint check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions with no alternatives are odd beasts, and it would seem
like they would worth be looking at in the linter (cf #10180). We
used to check two things:

* exprIsHNF is false: it would *seem* to be terribly wrong if
  the scrutinee was already in head normal form.

* exprIsDeadEnd is true: we should be able to see why GHC believes the
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

-- Takes the functions type and arguments as argument.
-- Returns the *result* of applying the function to arguments.
-- e.g. f :: Int -> Bool -> Int would return `Int` as result type.
lintCoreArgs  :: (OutType, UsageEnv) -> [InExpr] -> LintM (OutType, UsageEnv)
lintCoreArgs (fun_ty, fun_ue) args
  = lintApp (text "expression")
              lintTyArg lintValArg fun_ty args fun_ue

lintTyArg :: InExpr -> LintM OutType

-- Type argument
lintTyArg (Type arg_ty)
  = do { checkL (not (isCoercionTy arg_ty))
                (text "Unnecessary coercion-to-type injection:"
                  <+> ppr arg_ty)
       ; lintTypeAndSubst arg_ty }
lintTyArg arg
  = failWithL (hang (text "Expected type argument but found") 2 (ppr arg))

lintValArg  :: InExpr -> Mult -> UsageEnv -> LintM (OutType, UsageEnv)
lintValArg arg mult fun_ue
  = do { (arg_ty, arg_ue) <- markAllJoinsBad $ lintCoreExpr arg
           -- See Note [Representation polymorphism invariants] in GHC.Core

       ; flags <- getLintFlags
       ; when (lf_check_fixed_rep flags) $
         -- Only check that 'arg_ty' has a fixed RuntimeRep
         -- if 'lf_check_fixed_rep' is on.
         do { checkL (typeHasFixedRuntimeRep arg_ty)
                     (text "Argument does not have a fixed runtime representation"
                      <+> ppr arg <+> dcolon
                      <+> parens (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))) }

       ; let app_ue = addUE fun_ue (scaleUE mult arg_ue)
       ; return (arg_ty, app_ue) }

-----------------
lintAltBinders :: UsageEnv
               -> Var         -- Case binder
               -> OutType     -- Scrutinee type
               -> OutType     -- Constructor type
               -> [(Mult, OutVar)]    -- Binders
               -> LintM UsageEnv
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintAltBinders rhs_ue _case_bndr scrut_ty con_ty []
  = do { ensureEqTys con_ty scrut_ty (mkBadPatMsg con_ty scrut_ty)
       ; return rhs_ue }
lintAltBinders rhs_ue case_bndr scrut_ty con_ty ((var_w, bndr):bndrs)
  | isTyVar bndr
  = do { con_ty' <- lintTyApp con_ty (mkTyVarTy bndr)
       ; lintAltBinders rhs_ue case_bndr scrut_ty con_ty'  bndrs }
  | otherwise
  = do { (con_ty', _) <- lintValApp (Var bndr) con_ty (idType bndr) zeroUE zeroUE
         -- We can pass zeroUE to lintValApp because we ignore its usage
         -- calculation and compute it in the call for checkCaseLinearity below.
       ; rhs_ue' <- checkCaseLinearity rhs_ue case_bndr var_w bndr
       ; lintAltBinders rhs_ue' case_bndr scrut_ty con_ty' bndrs }

-- | Implements the case rules for linearity
checkCaseLinearity :: UsageEnv -> Var -> Mult -> Var -> LintM UsageEnv
checkCaseLinearity ue case_bndr var_w bndr = do
  ensureSubUsage lhs rhs err_msg
  lintLinearBinder (ppr bndr) (case_bndr_w `mkMultMul` var_w) (idMult bndr)
  return $ deleteUE ue bndr
  where
    lhs = bndr_usage `addUsage` (var_w `scaleUsage` case_bndr_usage)
    rhs = case_bndr_w `mkMultMul` var_w
    err_msg  = (text "Linearity failure in variable:" <+> ppr bndr
                $$ ppr lhs <+> text "⊈" <+> ppr rhs
                $$ text "Computed by:"
                <+> text "LHS:" <+> lhs_formula
                <+> text "RHS:" <+> rhs_formula)
    lhs_formula = ppr bndr_usage <+> text "+"
                                 <+> parens (ppr case_bndr_usage <+> text "*" <+> ppr var_w)
    rhs_formula = ppr case_bndr_w <+> text "*" <+> ppr var_w
    case_bndr_w = idMult case_bndr
    case_bndr_usage = lookupUE ue case_bndr
    bndr_usage = lookupUE ue bndr



-----------------
lintTyApp :: OutType -> OutType -> LintM OutType
lintTyApp fun_ty arg_ty
  | Just (tv,body_ty) <- splitForAllTyVar_maybe fun_ty
  = do  { lintTyKind tv arg_ty
        ; in_scope <- getInScope
        -- substTy needs the set of tyvars in scope to avoid generating
        -- uniques that are already in scope.
        -- See Note [The substitution invariant] in GHC.Core.TyCo.Subst
        ; return (substTyWithInScope in_scope [tv] [arg_ty] body_ty) }

  | otherwise
  = failWithL (mkTyAppMsg fun_ty arg_ty)

-----------------

-- | @lintValApp arg fun_ty arg_ty@ lints an application of @fun arg@
-- where @fun :: fun_ty@ and @arg :: arg_ty@, returning the type of the
-- application.
lintValApp :: CoreExpr -> OutType -> OutType -> UsageEnv -> UsageEnv
           -> LintM (OutType, UsageEnv)
lintValApp arg fun_ty arg_ty fun_ue arg_ue
  | Just (_, w, arg_ty', res_ty') <- splitFunTy_maybe fun_ty
  = do { ensureEqTys arg_ty' arg_ty (mkAppMsg arg_ty' arg_ty arg)
       ; let app_ue =  addUE fun_ue (scaleUE w arg_ue)
       ; return (res_ty', app_ue) }
  | otherwise
  = failWithL err2
  where
    err2 = mkNonFunAppMsg fun_ty arg_ty arg

lintTyKind :: OutTyVar -> OutType -> LintM ()
-- Both args have had substitution applied

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintTyKind tyvar arg_ty
  = unless (arg_kind `eqType` tyvar_kind) $
    addErrL (mkKindErrMsg tyvar arg_ty $$ (text "Linted Arg kind:" <+> ppr arg_kind))
  where
    tyvar_kind = tyVarKind tyvar
    arg_kind   = typeKind arg_ty

{-
************************************************************************
*                                                                      *
\subsection[lintCoreAlts]{lintCoreAlts}
*                                                                      *
************************************************************************
-}

lintCaseExpr :: CoreExpr -> InId -> InType -> [CoreAlt] -> LintM (OutType, UsageEnv)
lintCaseExpr scrut case_bndr alt_ty alts
  = do { let e = Case scrut case_bndr alt_ty alts   -- Just for error messages

       -- Check the scrutinee
       ; (scrut_ty', scrut_ue) <- markAllJoinsBad $ lintCoreExpr scrut
            -- See Note [Join points are less general than the paper]
            -- in GHC.Core

       ; alt_ty' <- addLoc (CaseTy scrut) $ lintValueType alt_ty

       ; checkCaseAlts e scrut scrut_ty' alts

       -- Lint the case-binder. Must do this after linting the scrutinee
       -- because the case-binder isn't in scope in the scrutineex
       ; lintBinder CaseBind case_bndr $ \case_bndr' ->
      -- Don't use lintIdBndr on case_bndr, because unboxed tuple is legitimate

    do { let case_bndr_ty' = idType case_bndr'
             scrut_mult    = idMult case_bndr'

       ; ensureEqTys case_bndr_ty' scrut_ty' (mkScrutMsg case_bndr case_bndr_ty' scrut_ty')
         -- See GHC.Core Note [Case expression invariants] item (7)

       ; -- Check the alternatives
       ; alt_ues <- mapM (lintCoreAlt case_bndr' scrut_ty' scrut_mult alt_ty') alts
       ; let case_ue = (scaleUE scrut_mult scrut_ue) `addUE` supUEs alt_ues
       ; return (alt_ty', case_ue) } }

checkCaseAlts :: InExpr -> InExpr -> OutType -> [CoreAlt] -> LintM ()
-- a) Check that the alts are non-empty
-- b1) Check that the DEFAULT comes first, if it exists
-- b2) Check that the others are in increasing order
-- c) Check that there's a default for infinite types
-- d) Check that the scrutinee is not a floating-point type
--    if there are any literal alternatives
-- e) Check if the scrutinee type has no constructors
--
-- We used to try to check whether a case expression with no
-- alternatives was legitimate, but this didn't work.
-- See Note [No alternatives lint check] for details.
--
-- NB: Algebraic cases are not necessarily exhaustive, because
--     the simplifier correctly eliminates case that can't
--     possibly match.
checkCaseAlts e scrut scrut_ty alts
  = do { checkL (all non_deflt con_alts) (mkNonDefltMsg e)
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
                (nonExhaustiveAltsMsg e)

       -- Check that the scrutinee is not a floating-point type
       -- if there are any literal alternatives
       -- See GHC.Core Note [Case expression invariants] item (5)
       -- See Note [Rules for floating-point comparisons] in GHC.Core.Opt.ConstantFold
       ; checkL (not $ isFloatingPrimTy scrut_ty && any is_lit_alt alts)
           (text "Lint warning: Scrutinising floating-point expression with literal pattern in case analysis (see #9238)."
            $$ text "scrut" <+> ppr scrut)

       -- Check if scrutinee type has no constructors
       -- Just a trace message for now
       ; case tyConAppTyCon_maybe scrut_ty of
           Just tycon
                | debugIsOn
                , isAlgTyCon tycon
                , not (isAbstractTyCon tycon)
                , null (tyConDataCons tycon)
                , not (exprIsDeadEnd scrut)
                -> pprTrace "Lint warning: case scrutinee type has no constructors"
                        (ppr scrut_ty)
                          -- This can legitimately happen for type families
                        $ return ()
           _otherwise -> return ()
        }
  where
    (con_alts, maybe_deflt) = findDefault alts

        -- Check that successive alternatives have strictly increasing tags
    increasing_tag (alt1 : rest@( alt2 : _)) = alt1 `ltAlt` alt2 && increasing_tag rest
    increasing_tag _                         = True

    non_deflt (Alt DEFAULT _ _) = False
    non_deflt _                 = True

    is_lit_alt (Alt (LitAlt _) _  _) = True
    is_lit_alt _                     = False

    is_infinite_ty = case tyConAppTyCon_maybe scrut_ty of
                        Nothing    -> False
                        Just tycon -> isPrimTyCon tycon

lintAltExpr :: CoreExpr -> OutType -> LintM UsageEnv
lintAltExpr expr ann_ty
  = do { (actual_ty, ue) <- lintCoreExpr expr
       ; ensureEqTys actual_ty ann_ty (mkCaseAltMsg expr actual_ty ann_ty)
       ; return ue }
         -- See GHC.Core Note [Case expression invariants] item (6)

lintCoreAlt :: OutId         -- Case binder
            -> OutType       -- Type of scrutinee
            -> Mult          -- Multiplicity of scrutinee
            -> OutType       -- Type of the alternative
            -> CoreAlt
            -> LintM UsageEnv
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintCoreAlt case_bndr _ scrut_mult alt_ty (Alt DEFAULT args rhs) =
  do { lintL (null args) (mkDefaultArgsMsg args)
     ; rhs_ue <- lintAltExpr rhs alt_ty
     ; let (case_bndr_usage, rhs_ue') = popUE rhs_ue case_bndr
           err_msg = vcat [ text "Linearity failure in the DEFAULT clause:" <+> ppr case_bndr
                          , ppr case_bndr_usage <+> text "⊈" <+> ppr scrut_mult ]
     ; ensureSubUsage case_bndr_usage scrut_mult err_msg
     ; return rhs_ue' }

lintCoreAlt case_bndr scrut_ty _ alt_ty (Alt (LitAlt lit) args rhs)
  | litIsLifted lit
  = failWithL integerScrutinisedMsg
  | otherwise
  = do { lintL (null args) (mkDefaultArgsMsg args)
       ; ensureEqTys lit_ty scrut_ty (mkBadPatMsg lit_ty scrut_ty)
       ; rhs_ue <- lintAltExpr rhs alt_ty
       ; return (deleteUE rhs_ue case_bndr) -- No need for linearity checks
       }
  where
    lit_ty = literalType lit

lintCoreAlt case_bndr scrut_ty _scrut_mult alt_ty alt@(Alt (DataAlt con) args rhs)
  | isNewTyCon (dataConTyCon con)
  = zeroUE <$ addErrL (mkNewTyDataConAltMsg scrut_ty alt)
  | Just (tycon, tycon_arg_tys) <- splitTyConApp_maybe scrut_ty
  = addLoc (CaseAlt alt) $  do
    { checkTypeDataConOcc "pattern" con
    ; lintL (tycon == dataConTyCon con) (mkBadConMsg tycon con)

      -- Instantiate the universally quantified
      -- type variables of the data constructor
    ; let { con_payload_ty = piResultTys (dataConRepType con) tycon_arg_tys
          ; binderMult (Named _)   = ManyTy
          ; binderMult (Anon st _) = scaledMult st
          -- See Note [Validating multiplicities in a case]
          ; multiplicities = map binderMult $ fst $ splitPiTys con_payload_ty }

        -- And now bring the new binders into scope
    ; lintBinders CasePatBind args $ \ args' -> do
      { rhs_ue <- lintAltExpr rhs alt_ty
      ; rhs_ue' <- addLoc (CasePat alt) $
                   lintAltBinders rhs_ue case_bndr scrut_ty con_payload_ty
                                  (zipEqual multiplicities  args')
      ; return $ deleteUE rhs_ue' case_bndr
      }
   }

  | otherwise   -- Scrut-ty is wrong shape
  = zeroUE <$ addErrL (mkBadAltMsg scrut_ty alt)

{-
Note [Validating multiplicities in a case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose 'MkT :: a %m -> T m a'.
If we are validating 'case (x :: T Many a) of MkT y -> ...',
we have to substitute m := Many in the type of MkT - in particular,
y can be used Many times and that expression would still be linear in x.
We do this by looking at con_payload_ty, which is the type of the datacon
applied to the surrounding arguments.
Testcase: linear/should_compile/MultConstructor

Data constructors containing existential tyvars will then have
Named binders, which are always multiplicity Many.
Testcase: indexed-types/should_compile/GADT1
-}

lintLinearBinder :: SDoc -> Mult -> Mult -> LintM ()
lintLinearBinder doc actual_usage described_usage
  = ensureSubMult actual_usage described_usage err_msg
    where
      err_msg = (text "Multiplicity of variable does not agree with its context"
                $$ doc
                $$ ppr actual_usage
                $$ text "Annotation:" <+> ppr described_usage)

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
lintBinders :: HasDebugCallStack => BindingSite -> [InVar] -> ([OutVar] -> LintM a) -> LintM a
lintBinders _    []         linterF = linterF []
lintBinders site (var:vars) linterF = lintBinder site var $ \var' ->
                                      lintBinders site vars $ \ vars' ->
                                      linterF (var':vars')

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintBinder :: HasDebugCallStack => BindingSite -> InVar -> (OutVar -> LintM a) -> LintM a
lintBinder site var linterF
  | isTyCoVar var = lintTyCoBndr var linterF
  | otherwise     = lintIdBndr NotTopLevel site var linterF

lintTyCoBndr :: HasDebugCallStack => TyCoVar -> (OutTyCoVar -> LintM a) -> LintM a
lintTyCoBndr tcv thing_inside
  = do { tcv_type' <- lintTypeAndSubst (varType tcv)
       ; let tcv_kind' = typeKind tcv_type'

         -- See (FORALL1) and (FORALL2) in GHC.Core.Type
       ; if (isTyVar tcv)
         then -- Check that in (forall (a:ki). blah) we have ki:Type
              lintL (isLiftedTypeKind tcv_kind') $
              hang (text "TyVar whose kind does not have kind Type:")
                 2 (ppr tcv <+> dcolon <+> ppr tcv_type' <+> dcolon <+> ppr tcv_kind')
         else -- Check that in (forall (cv::ty). blah),
              -- then ty looks like (t1 ~# t2)
              lintL (isCoVarType tcv_type') $
              text "CoVar with non-coercion type:" <+> pprTyVar tcv

       ; addInScopeTyCoVar tcv tcv_type' thing_inside }

lintIdBndrs :: forall a. TopLevelFlag -> [InId] -> ([OutId] -> LintM a) -> LintM a
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
  = assertPpr (isId id) (ppr id) $
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

          -- See Note [Representation polymorphism invariants] in GHC.Core
       ; lintL (isJoinId id || not (lf_check_fixed_rep flags)
                || typeHasFixedRuntimeRep id_ty) $
         text "Binder does not have a fixed runtime representation:" <+> ppr id <+> dcolon <+>
            parens (ppr id_ty <+> dcolon <+> ppr (typeKind id_ty))

       -- Check that a join-id is a not-top-level let-binding
       ; when (isJoinId id) $
         checkL (not is_top_lvl && is_let_bind) $
         mkBadJoinBindMsg id

       -- Check that the Id does not have type (t1 ~# t2) or (t1 ~R# t2);
       -- if so, it should be a CoVar, and checked by lintCoVarBndr
       ; lintL (not (isCoVarType id_ty))
               (text "Non-CoVar has coercion type" <+> ppr id <+> dcolon <+> ppr id_ty)

       -- Check that the lambda binder has no value or OtherCon unfolding.
       -- See #21496
       ; lintL (not (bind_site == LambdaBind && isEvaldUnfolding (idUnfolding id)))
                (text "Lambda binder with value or OtherCon unfolding.")

       ; out_ty <- addLoc (IdTy id) (lintValueType id_ty)

       ; addInScopeId id out_ty thing_inside }
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

{- Note [Linting types and coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that
   lintType     :: InType     -> LintM ()
   lintCoercion :: InCoercion -> LintM ()
Neither returns anything.

If you need the kind of the type, then do `typeKind` and then apply
the ambient substitution using `substTyM`.  Note that the substitution
empty unless there is shadowing or type-lets; and if the substitution is
empty, the `substTyM` is a no-op.

It is better to take the kind and then substitute, rather than substitute
and then take the kind, becaues the kind is usually smaller.

Note: you might wonder if we should apply the same logic to expressions.
Why do we have
  lintExpr :: InExpr -> LintM OutType
Partly inertia; but also taking the type of an expresison involve looking
down a deep chain of let's, whereas that is not true of taking the kind
of a type.  It'd be worth an experiment though.

Historical note: in the olden days we had
   lintType :: InType -> LintM OutType
but that burned a huge amount of allocation building an OutType that was
often discarded, or used only to get its kind.

I also experimented with
   lintType :: InType -> LintM OutKind
but that too was slower.  It is also much simpler to return ()!  If we
return the kind we have to duplicate the logic in `typeKind`; and it is
much worse for coercions.
-}

lintValueType :: Type -> LintM OutType
-- Types only, not kinds
-- Check the type, and apply the substitution to it
-- See Note [Linting type lets]
lintValueType ty
  = addLoc (InType ty) $
    do  { ty' <- lintTypeAndSubst ty
        ; let sk = typeKind ty'
        ; lintL (isTYPEorCONSTRAINT sk) $
          hang (text "Ill-kinded type:" <+> ppr ty)
             2 (text "has kind:" <+> ppr sk)
        ; return ty' }

checkTyCon :: TyCon -> LintM ()
checkTyCon tc
  = checkL (not (isTcTyCon tc)) (text "Found TcTyCon:" <+> ppr tc)

-------------------
lintTypeAndSubst :: InType -> LintM OutType
lintTypeAndSubst ty = do { lintType ty; substTyM ty }
           -- In GHCi we may lint an expression with a free
           -- type variable.  Then it won't be in the
           -- substitution, but it should be in scope

lintType :: InType -> LintM ()
-- See Note [Linting types and coercions]
--
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintType (TyVarTy tv)
  | not (isTyVar tv)
  = failWithL (mkBadTyVarMsg tv)

  | otherwise
  = do { _ <- lintVarOcc tv
       ; return () }

lintType ty@(AppTy t1 t2)
  | TyConApp {} <- t1
  = failWithL $ text "TyConApp to the left of AppTy:" <+> ppr ty
  | otherwise
  = do { let (fun_ty, arg_tys) = collect t1 [t2]
       ; lintType fun_ty
       ; fun_kind <- substTyM (typeKind fun_ty)
       ; lint_ty_app ty fun_kind arg_tys }
  where
    collect (AppTy f a) as = collect f (a:as)
    collect fun         as = (fun, as)

lintType ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = do { report_unsat <- lf_report_unsat_syns <$> getLintFlags
       ; lintTySynFamApp report_unsat ty tc tys }

  | Just {} <- tyConAppFunTy_maybe tc tys
    -- We should never see a saturated application of funTyCon; such
    -- applications should be represented with the FunTy constructor.
    -- See Note [Linting function types]
  = failWithL (hang (text "Saturated application of" <+> quotes (ppr tc)) 2 (ppr ty))

  | otherwise  -- Data types, data families, primitive types
  = do { checkTyCon tc
       ; lint_ty_app ty (tyConKind tc) tys }

-- arrows can related *unlifted* kinds, so this has to be separate from
-- a dependent forall.
lintType ty@(FunTy af tw t1 t2)
  = do { lintType t1
       ; lintType t2
       ; lintType tw
       ; lintArrow (text "type or kind" <+> quotes (ppr ty)) af t1 t2 tw }

lintType ty@(ForAllTy {})
  = go [] ty
  where
    go :: [OutTyCoVar] -> InType -> LintM ()
    -- Loop, collecting the forall-binders
    go tcvs ty@(ForAllTy (Bndr tcv _) body_ty)
      | not (isTyCoVar tcv)
      = failWithL (text "Non-TyVar or Non-CoVar bound in type:" <+> ppr ty)

      | otherwise
      = lintTyCoBndr tcv $ \tcv' ->
        do { -- See GHC.Core.TyCo.Rep Note [Unused coercion variable in ForAllTy]
             -- Suspicious because it works on InTyCoVar; c.f. ForAllCo
             when (isCoVar tcv) $
             lintL (anyFreeVarsOfType (== tcv) body_ty) $
             text "Covar does not occur in the body:" <+> (ppr tcv $$ ppr body_ty)

           ; go (tcv' : tcvs) body_ty }

    go tcvs body_ty
      = do { lintType body_ty
           ; lintForAllBody tcvs body_ty }

lintType (CastTy ty co)
  = do { lintType ty
       ; ty_kind <- substTyM (typeKind ty)
       ; co_lk <- lintStarCoercion co
       ; ensureEqTys ty_kind co_lk (mkCastTyErr ty co ty_kind co_lk) }

lintType (LitTy l)       = lintTyLit l
lintType (CoercionTy co) = lintCoercion co

-----------------
lintForAllBody :: [OutTyCoVar] -> InType -> LintM ()
-- Do the checks for the body of a forall-type
lintForAllBody tcvs body_ty
  = do { -- For type variables, check for skolem escape
         -- See Note [Phantom type variables in kinds] in GHC.Core.Type
         -- The kind of (forall cv. th) is liftedTypeKind, so no
         -- need to check for skolem-escape in the CoVar case
         body_kind <- substTyM (typeKind body_ty)
       ; case occCheckExpand tcvs body_kind of
           Just {} -> return ()
           Nothing -> failWithL $
                      hang (text "Variable escape in forall:")
                         2 (vcat [ text "tycovars (reversed):" <+> ppr tcvs
                                 , text "type:" <+> ppr body_ty
                                 , text "kind:" <+> ppr body_kind ])
       ; checkValueType body_kind (text "the body of forall:" <+> ppr body_ty) }

-----------------
lintTySynFamApp :: Bool -> InType -> TyCon -> [InType] -> LintM ()
-- The TyCon is a type synonym or a type family (not a data family)
-- See Note [Linting type synonym applications]
-- c.f. GHC.Tc.Validity.check_syn_tc_app
lintTySynFamApp report_unsat ty tc tys
  | report_unsat   -- Report unsaturated only if report_unsat is on
  , tys `lengthLessThan` tyConArity tc
  = failWithL (hang (text "Un-saturated type application") 2 (ppr ty))

  -- Deal with type synonyms
  | ExpandsSyn tenv rhs tys' <- expandSynTyCon_maybe tc tys
  , let expanded_ty = mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys'
  = do { when report_unsat $ do { _ <- lintType expanded_ty
                                ; return () }

       ; -- Kind-check the argument types, but without reporting
         -- un-saturated type families/synonyms
       ; setReportUnsat False $
         lint_ty_app ty (tyConKind tc) tys }

  -- Otherwise this must be a type family
  | otherwise
  = lint_ty_app ty (tyConKind tc) tys

-----------------
-- Confirms that a kind is really TYPE r or Constraint
checkValueType :: OutKind -> SDoc -> LintM ()
checkValueType kind doc
  = lintL (isTYPEorCONSTRAINT kind)
          (text "Non-Type-like kind when Type-like expected:" <+> ppr kind $$
           text "when checking" <+> doc)

-----------------
lintArrow :: SDoc -> FunTyFlag -> InType -> InType -> InType -> LintM ()
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]
lintArrow what af t1 t2 tw  -- Eg lintArrow "type or kind `blah'" k1 k2 kw
                            -- or lintArrow "coercion `blah'" k1 k2 kw
  = do { k1 <- substTyM (typeKind t1)
       ; k2 <- substTyM (typeKind t2)
       ; kw <- substTyM (typeKind tw)
       ; unless (isTYPEorCONSTRAINT k1) (report (text "argument")     t1 k1)
       ; unless (isTYPEorCONSTRAINT k2) (report (text "result")       t2 k2)
       ; unless (isMultiplicityTy kw)   (report (text "multiplicity") tw kw)

       ; let real_af = chooseFunTyFlag t1 t2
       ; unless (real_af == af) $ addErrL $
         hang (text "Bad FunTyFlag")
            2 (vcat [ text "FunTyFlag =" <+> ppr af
                    , text "Computed FunTyFlag =" <+> ppr real_af
                    , text "in" <+> what ]) }
  where
    report ar t k = addErrL (hang (text "Ill-kinded" <+> ar)
                                2 (vcat [ ppr t <+> dcolon <+> ppr k
                                        , text "in" <+> what ]))

-----------------
lintTyLit :: TyLit -> LintM ()
lintTyLit (NumTyLit n)
  | n >= 0    = return ()
  | otherwise = failWithL msg
    where msg = text "Negative type literal:" <+> integer n
lintTyLit (StrTyLit _) = return ()
lintTyLit (CharTyLit _) = return ()

-----------------
lint_ty_app :: InType -> OutKind -> [InType] -> LintM ()
lint_ty_app ty = lint_tyco_app (text "type" <+> quotes (ppr ty))

lint_co_app :: HasDebugCallStack => Coercion -> OutKind -> [InType] -> LintM ()
lint_co_app co = lint_tyco_app (text "coercion" <+> quotes (ppr co))

lint_tyco_app :: SDoc -> OutKind -> [InType] -> LintM ()
lint_tyco_app msg fun_kind arg_tys
    -- See Note [Avoiding compiler perf traps when constructing error messages.]
  = do { _ <- lintApp msg (\ty     -> do { lintType ty; substTyM ty })
                            (\ty _ _ -> do { lintType ty; ki <- substTyM (typeKind ty); return (ki,()) })
                            fun_kind arg_tys ()
       ; return () }

----------------
lintApp :: forall in_a acc. Outputable in_a =>
             SDoc
          -> (in_a -> LintM OutType)                        -- Lint the thing and return its value
          -> (in_a -> Mult -> acc -> LintM (OutKind, acc))  -- Lint the thing and return its type
          -> OutType
          -> [in_a]                               -- The arguments, always "In" things
          -> acc                                  -- Used (only) for UsageEnv in /term/ applications
          -> LintM (OutType,acc)
-- lintApp is a performance-critical function, which deals with multiple
-- applications such as  (/\a./\b./\c. expr) @ta @tb @tc
-- When returning the type of this expression we want to avoid substituting a:=ta,
-- and /then/ substituting b:=tb, etc.  That's quadratic, and can be a huge
-- perf hole.  So we gather all the arguments [in_a], and then gather the
-- substitution incrementally in the `go` loop.
--
-- lintApp is used:
--    * for term applications (lintCoreArgs)
--    * for type applications (lint_ty_app)
--    * for coercion application (lint_co_app)
-- To deal with these cases `lintApp` has two higher order arguments;
-- but we specialise it for each call site (by inlining)
{-# INLINE lintApp #-}    -- INLINE: very few call sites;
                          -- not recursive; specialised at its call sites

lintApp msg lint_forall_arg lint_arrow_arg !orig_fun_ty all_args acc
    = do { !in_scope <- getInScope
         -- We need the in_scope set to satisfy the invariant in
         -- Note [The substitution invariant] in GHC.Core.TyCo.Subst
         -- Forcing the in scope set eagerly here reduces allocations by up to 4%.

         ; let init_subst = mkEmptySubst in_scope

               go :: Subst -> OutType -> acc -> [in_a] -> LintM (OutType, acc)
                     -- The Subst applies (only) to the fun_ty
                     -- c.f. GHC.Core.Type.piResultTys, which has a similar loop

               go subst fun_ty acc []
                 = return (substTy subst fun_ty, acc)

               go subst (ForAllTy (Bndr tv _vis) body_ty) acc (arg:args)
                 = do { arg' <- lint_forall_arg arg
                      ; let tv_kind = substTy subst (varType tv)
                            karg'   = typeKind arg'
                            subst'  = extendTCvSubst subst tv arg'
                      ; ensureEqTys karg' tv_kind $
                        lint_app_fail_msg msg orig_fun_ty all_args
                            (hang (text "Forall:" <+> (ppr tv $$ ppr tv_kind))
                                2 (ppr arg' <+> dcolon <+> ppr karg'))
                      ; go subst' body_ty acc args }

               go subst fun_ty@(FunTy _ mult exp_arg_ty res_ty) acc (arg:args)
                 = do { (arg_ty, acc') <- lint_arrow_arg arg (substTy subst mult) acc
                      ; ensureEqTys (substTy subst exp_arg_ty) arg_ty $
                        lint_app_fail_msg msg orig_fun_ty all_args
                            (hang (text "Fun:" <+> ppr fun_ty)
                                2 (vcat [ text "exp_arg_ty:" <+> ppr exp_arg_ty
                                        , text "arg:" <+> ppr arg <+> dcolon <+> ppr arg_ty ]))
                      ; go subst res_ty acc' args }

               go subst fun_ty acc args
                 | Just fun_ty' <- coreView fun_ty
                 = go subst fun_ty' acc args

                 | not (isEmptyTCvSubst subst)  -- See Note [Care with kind instantiation]
                 = go init_subst (substTy subst fun_ty) acc args

                 | otherwise
                 = failWithL (lint_app_fail_msg msg orig_fun_ty all_args
                                  (text "Not a fun:" <+> (ppr fun_ty $$ ppr args)))

         ; go init_subst orig_fun_ty acc all_args }

-- This is a top level definition to ensure we pass all variables of the error message
-- explicitly and don't capture them as free variables. Otherwise this binder might
-- become a thunk that get's allocated in the hot code path.
-- See Note [Avoiding compiler perf traps when constructing error messages.]
lint_app_fail_msg :: (Outputable a2) => SDoc -> OutType -> a2 -> SDoc -> SDoc
lint_app_fail_msg msg kfn arg_tys extra
  = vcat [ hang (text "Application error in") 2 msg
         , nest 2 (text "Function type =" <+> ppr kfn)
         , nest 2 (text "Args =" <+> ppr arg_tys)
         , extra ]

{- *********************************************************************
*                                                                      *
        Linting rules
*                                                                      *
********************************************************************* -}

lintCoreRule :: OutVar -> OutType -> CoreRule -> LintM ()
lintCoreRule _ _ (BuiltinRule {})
  = return ()  -- Don't bother

lintCoreRule fun fun_ty rule@(Rule { ru_name = name, ru_bndrs = bndrs
                                   , ru_args = args, ru_rhs = rhs })
  = lintBinders LambdaBind bndrs $ \ _ ->
    do { (lhs_ty, _) <- lintCoreArgs (fun_ty, zeroUE) args
       ; (rhs_ty, _) <- case idJoinPointHood fun of
                     JoinPoint join_arity
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
not-mentioned in the LHS (ru_args).  How can that happen?  Well, in #10602,
SpecConstr stupidly constructed a rule like

  forall x,c1,c2.
     f (x |> c1 |> c2) = ....

But simplExpr collapses those coercions into one.  (Indeed in #10602,
it collapsed to the identity and was removed altogether.)

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
"undersaturated" rule (see 'GHC.Core.Opt.Arity.etaExpandToJoinPointRule'), and in fact
the simplifier would have to in order to deal with the RHS. So we take a
conservative view and don't allow undersaturated rules for join points. See
Note [Join points and unfoldings/rules] in "GHC.Core.Opt.OccurAnal" for further discussion.
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
* SelCo (cf #9233)
* LRCo

But the code is simple.  And this is only Lint.  Let's wait to see if
the bad perf bites us in practice.

A solution would be to return the kind and role of the coercion,
as well as the linted coercion.  Or perhaps even *only* the kind and role,
which is what used to happen.   But that proved tricky and error prone
(#17923), so now we return the coercion.
-}


-- lintStarCoercion lints a coercion, confirming that its lh kind and
-- its rh kind are both *; also ensures that the role is Nominal
-- Returns the lh kind
lintStarCoercion :: InCoercion -> LintM OutType
lintStarCoercion g
  = do { lintCoercion g
       ; Pair t1 t2 <- substCoKindM g
       ; checkValueType (typeKind t1) (text "the kind of the left type in" <+> ppr g)
       ; checkValueType (typeKind t2) (text "the kind of the right type in" <+> ppr g)
       ; lintRole g Nominal (coercionRole g)
       ; return t1 }

substCoKindM :: InCoercion -> LintM (Pair OutType)
substCoKindM co
  = do { let !(Pair lk rk) = coercionKind co
       ; lk' <- substTyM lk
       ; rk' <- substTyM rk
       ; return (Pair lk' rk') }

lintCoercion :: HasDebugCallStack => InCoercion -> LintM ()
-- See Note [Linting types and coercions]
--
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism]

lintCoercion (CoVarCo cv)
  | not (isCoVar cv)
  = failWithL (hang (text "Bad CoVarCo:" <+> ppr cv)
                  2 (text "With offending type:" <+> ppr (varType cv)))

  | otherwise  -- C.f. lintType (TyVarTy tv), which has better docs
  = do { _ <- lintVarOcc cv; return () }

lintCoercion (Refl ty)          = lintType ty
lintCoercion (GRefl _r ty MRefl) = lintType ty

lintCoercion (GRefl _r ty (MCo co))
  = do { lintType ty
       ; lintCoercion co
       ; tk <- substTyM (typeKind ty)
       ; tl <- substTyM (coercionLKind co)
       ; ensureEqTys tk tl $
         hang (text "GRefl coercion kind mis-match:" <+> ppr co)
            2 (vcat [ppr ty, ppr tk, ppr tl])
       ; lintRole co Nominal (coercionRole co) }

lintCoercion co@(TyConAppCo r tc cos)
  | Just {} <- tyConAppFunCo_maybe r tc cos
  = failWithL (hang (text "Saturated application of" <+> quotes (ppr tc))
                  2 (ppr co))
    -- All saturated TyConAppCos should be FunCos

  | Just {} <- synTyConDefn_maybe tc
  = failWithL (text "Synonym in TyConAppCo:" <+> ppr co)

  | otherwise
  = do { checkTyCon tc
       ; mapM_ lintCoercion cos
       ; let tc_kind = tyConKind tc
       ; lint_co_app co tc_kind (map coercionLKind cos)
       ; lint_co_app co tc_kind (map coercionRKind cos)
       ; zipWithM_ (lintRole co) (tyConRoleListX r tc) (map coercionRole cos) }


lintCoercion co@(AppCo co1 co2)
  | TyConAppCo {} <- co1
  = failWithL (text "TyConAppCo to the left of AppCo:" <+> ppr co)
  | Just (TyConApp {}, _) <- isReflCo_maybe co1
  = failWithL (text "Refl (TyConApp ...) to the left of AppCo:" <+> ppr co)
  | otherwise
  = do { lintCoercion co1
       ; lintCoercion co2
       ; let !(Pair lt1 rt1) = coercionKind co1
       ; lk1 <- substTyM (typeKind lt1)
       ; rk1 <- substTyM (typeKind rt1)
       ; lint_co_app co lk1 [coercionLKind co2]
       ; lint_co_app co rk1 [coercionRKind co2]

       ; let r2 = coercionRole co2
       ; if coercionRole co1 == Phantom
         then lintL (r2 == Phantom || r2 == Nominal)
                     (text "Second argument in AppCo cannot be R:" $$
                      ppr co)
         else lintRole co Nominal r2 }

----------
lintCoercion co@(ForAllCo {})
-- See Note [ForAllCo] in GHC.Core.TyCo.Rep for the typing rule for ForAllCo
  = do { _ <- go [] co; return () }
  where
    go :: [OutTyCoVar]   -- Binders in reverse order
       -> InCoercion -> LintM Role
    go tcvs co@(ForAllCo { fco_tcv = tcv, fco_visL = visL, fco_visR = visR
                         , fco_kind = kind_co, fco_body = body_co })
      | not (isTyCoVar tcv)
      = failWithL (text "Non tyco binder in ForAllCo:" <+> ppr co)

      | otherwise
      = do { lk <- lintStarCoercion kind_co
           ; lintTyCoBndr tcv $ \tcv' ->
        do { ensureEqTys (varType tcv') lk $
             text "Kind mis-match in ForallCo" <+> ppr co

           -- I'm not very sure about this part, because it traverses body_co
           -- but at least it's on a cold path (a ForallCo for a CoVar)
           -- Also it works on InTyCoVar and InCoercion, which is suspect
           ; when (isCoVar tcv) $
             do { lintL (visL == coreTyLamForAllTyFlag && visR == coreTyLamForAllTyFlag) $
                  text "Invalid visibility flags in CoVar ForAllCo" <+> ppr co
                  -- See (FC7) in Note [ForAllCo] in GHC.Core.TyCo.Rep
                ; lintL (almostDevoidCoVarOfCo tcv body_co) $
                  text "Covar can only appear in Refl and GRefl: " <+> ppr co }
                  -- See (FC6) in Note [ForAllCo] in GHC.Core.TyCo.Rep

           ; role <- go (tcv':tcvs) body_co

           ; when (role == Nominal) $
             lintL (visL `eqForAllVis` visR) $
             text "Nominal ForAllCo has mismatched visibilities: " <+> ppr co

           ; return role } }

    go tcvs body_co
      = do { lintCoercion body_co

           -- Need to check that
           --    (forall (tcv:k1). lty) and
           --    (forall (tcv:k2). rty[(tcv:k2) |> sym kind_co/tcv])
           -- are both well formed, including the skolem escape check.
           -- Easiest way is to call lintForAllBody for each
           ; let Pair lty rty = coercionKind body_co
           ; lintForAllBody tcvs lty
           ; lintForAllBody tcvs rty

           ; return (coercionRole body_co) }


lintCoercion (FunCo { fco_role = r, fco_afl = afl, fco_afr = afr
                    , fco_mult = cow, fco_arg = co1, fco_res = co2 })
  = do { lintCoercion co1
       ; lintCoercion co2
       ; lintCoercion cow
       ; let Pair lt1 rt1 = coercionKind co1
             Pair lt2 rt2 = coercionKind co2
             Pair ltw rtw = coercionKind cow
       ; lintArrow (bad_co_msg "arrowl") afl lt1 lt2 ltw
       ; lintArrow (bad_co_msg "arrowr") afr rt1 rt2 rtw
       ; lintRole co1 r (coercionRole co1)
       ; lintRole co2 r (coercionRole co2)
       ; let expected_mult_role = case r of
                                    Phantom -> Phantom
                                    _ -> Nominal
       ; lintRole cow expected_mult_role (coercionRole cow) }
  where
    bad_co_msg s = hang (text "Bad coercion" <+> parens (text s))
                      2 (vcat [ text "afl:" <+> ppr afl
                              , text "afr:" <+> ppr afr
                              , text "arg_co:" <+> ppr co1
                              , text "res_co:" <+> ppr co2 ])

-- See Note [Bad unsafe coercion]
lintCoercion co@(UnivCo { uco_role = r, uco_prov = prov
                        , uco_lty = ty1, uco_rty = ty2, uco_deps = deps })
  = do { -- Check the role.  PhantomProv must have Phantom role, otherwise any role is fine
         case prov of
            PhantomProv -> lintRole co Phantom r
            _           -> return ()

       -- Check the to and from types
       ; lintType ty1
       ; lintType ty2
       ; tk1 <- substTyM (typeKind ty1)
       ; tk2 <- substTyM (typeKind ty2)

       ; when (r /= Phantom && isTYPEorCONSTRAINT tk1 && isTYPEorCONSTRAINT tk2)
              (checkTypes ty1 ty2)

       -- Check the coercions on which this UnivCo depends
       ; mapM_ lintCoercion deps }
   where
     report s = hang (text $ "Unsafe coercion: " ++ s)
                     2 (vcat [ text "From:" <+> ppr ty1
                             , text "  To:" <+> ppr ty2])
     isUnBoxed :: PrimRep -> Bool
     isUnBoxed = not . isGcPtrRep

       -- see #9122 for discussion of these checks
     checkTypes t1 t2
       = do { checkWarnL fixed_rep_1
                         (report "left-hand type does not have a fixed runtime representation")
            ; checkWarnL fixed_rep_2
                         (report "right-hand type does not have a fixed runtime representation")
            ; when (fixed_rep_1 && fixed_rep_2) $
              do { checkWarnL (reps1 `equalLength` reps2)
                              (report "between values with different # of reps")
                 ; zipWithM_ validateCoercion reps1 reps2 }}
       where
         fixed_rep_1 = typeHasFixedRuntimeRep t1
         fixed_rep_2 = typeHasFixedRuntimeRep t2

         -- don't look at these unless lev_poly1/2 are False
         -- Otherwise, we get #13458
         reps1 = typePrimRep t1
         reps2 = typePrimRep t2

     validateCoercion :: PrimRep -> PrimRep -> LintM ()
     validateCoercion rep1 rep2
       = do { platform <- getPlatform
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

lintCoercion (SymCo co) = lintCoercion co

lintCoercion co@(TransCo co1 co2)
  = do { lintCoercion co1
       ; lintCoercion co2
       ; rk1 <- substTyM (coercionRKind co1)
       ; lk2 <- substTyM (coercionLKind co2)
       ; ensureEqTys rk1 lk2
               (hang (text "Trans coercion mis-match:" <+> ppr co)
                   2 (vcat [ppr (coercionKind co1), ppr (coercionKind co2)]))
       ; lintRole co (coercionRole co1) (coercionRole co2) }

lintCoercion the_co@(SelCo cs co)
  = do { lintCoercion co
       ; Pair s t <- substCoKindM co

       ; if -- forall (both TyVar and CoVar)
            | Just _ <- splitForAllTyCoVar_maybe s
            , Just _ <- splitForAllTyCoVar_maybe t
            , SelForAll <- cs
            ,   (isForAllTy_ty s && isForAllTy_ty t)
             || (isForAllTy_co s && isForAllTy_co t)
            -> return ()

            -- function
            | isFunTy s
            , isFunTy t
            , SelFun {} <- cs
            -> return ()

            -- TyCon
            | Just (tc_s, tys_s) <- splitTyConApp_maybe s
            , Just (tc_t, tys_t) <- splitTyConApp_maybe t
            , tc_s == tc_t
            , SelTyCon n r0 <- cs
            , let co_role = coercionRole co
            , isInjectiveTyCon tc_s co_role
                -- see Note [SelCo and newtypes] in GHC.Core.TyCo.Rep
            , tys_s `equalLength` tys_t
            , tys_s `lengthExceeds` n
            -> do { lintRole the_co (tyConRole co_role tc_s n) r0
                  ; return () }

            | otherwise
            -> failWithL (hang (text "Bad SelCo:")
                             2 (ppr the_co $$ ppr s $$ ppr t)) }

lintCoercion the_co@(LRCo _lr co)
  = do { lintCoercion co
       ; Pair s t <- substCoKindM co
       ; lintRole co Nominal (coercionRole co)
       ; case (splitAppTy_maybe s, splitAppTy_maybe t) of
           (Just {}, Just {}) -> return ()
           _ -> failWithL (hang (text "Bad LRCo:")
                              2 (ppr the_co $$ ppr s $$ ppr t)) }


lintCoercion orig_co@(InstCo co arg)
  = go co [arg]
  where
    go (InstCo co arg) args = do { lintCoercion arg; go co (arg:args) }
    go co              args = do { lintCoercion co
                                 ; let Pair lty rty = coercionKind co
                                 ; lty' <- substTyM lty
                                 ; rty' <- substTyM rty
                                 ; in_scope <- getInScope
                                 ; let subst = mkEmptySubst in_scope
                                 ; go_args (subst, lty') (subst,rty') args }

    -------------
    go_args :: (Subst, OutType) -> (Subst,OutType) -> [InCoercion]
           -> LintM ()
    go_args _ _ []
      = return ()
    go_args lty rty (arg:args)
      = do { (lty1, rty1)  <- go_arg lty rty arg
           ; go_args lty1 rty1 args }

    -------------
    go_arg :: (Subst, OutType) -> (Subst,OutType) -> InCoercion
           -> LintM ((Subst,OutType), (Subst,OutType))
    go_arg (lsubst,lty) (rsubst,rty) arg
      = do { lintRole arg Nominal (coercionRole arg)
           ; Pair arg_lty arg_rty <- substCoKindM arg

           ; case (splitForAllTyCoVar_maybe lty, splitForAllTyCoVar_maybe rty) of
              -- forall over tvar
                (Just (ltv,lty1), Just (rtv,rty1))
                  | typeKind arg_lty `eqType` substTy lsubst (tyVarKind ltv)
                  , typeKind arg_rty `eqType` substTy rsubst (tyVarKind rtv)
                  -> return ( (extendTCvSubst lsubst ltv arg_lty, lty1)
                            , (extendTCvSubst rsubst rtv arg_rty, rty1) )
                  | otherwise
                  -> failWithL (hang (text "Kind mis-match in inst coercion")
                                   2 (vcat [ text "arg"  <+> ppr arg
                                           , text "lty"  <+> ppr lty <+> dcolon <+> ppr (typeKind lty)
                                           , text "rty"  <+> ppr rty <+> dcolon <+> ppr (typeKind rty)
                                           , text "arg_lty" <+> ppr arg_lty <+> dcolon <+> ppr (typeKind arg_lty)
                                           , text "arg_rty" <+> ppr arg_rty <+> dcolon <+> ppr (typeKind arg_rty)
                                           , text "ltv" <+> ppr ltv <+> dcolon <+> ppr (tyVarKind ltv)
                                           , text "rtv" <+> ppr rtv <+> dcolon <+> ppr (tyVarKind rtv) ]))

                _ -> failWithL (text "Bad argument of inst" <+> ppr orig_co) }

lintCoercion this_co@(AxiomCo ax cos)
  = do { mapM_ lintCoercion cos
       ; lint_roles 0 (coAxiomRuleArgRoles ax) cos
       ; prs <- mapM substCoKindM cos
       ; lint_ax ax prs }

  where
    lint_ax :: CoAxiomRule -> [Pair OutType] -> LintM ()
    lint_ax (BuiltInFamRew  bif) prs
      = checkL (isJust (bifrw_proves bif prs))  bad_bif
    lint_ax (BuiltInFamInj bif) prs
      = checkL (case prs of
                  [pr] -> isJust (bifinj_proves bif pr)
                  _    -> False)
               bad_bif
    lint_ax (UnbranchedAxiom ax) prs
      = lintBranch this_co (coAxiomTyCon ax) (coAxiomSingleBranch ax) prs
    lint_ax (BranchedAxiom ax ind) prs
      = do { checkL (0 <= ind && ind < numBranches (coAxiomBranches ax))
                    (bad_ax this_co (text "index out of range"))
           ; lintBranch this_co (coAxiomTyCon ax) (coAxiomNthBranch ax ind) prs }

    bad_bif = bad_ax this_co (text "Proves returns Nothing")

    err :: forall a. String -> [SDoc] -> LintM a
    err m xs  = failWithL $
                hang (text m) 2 $ vcat (text "Rule:" <+> ppr ax : xs)

    lint_roles n (e : es) (co:cos)
      | e == coercionRole co
      = lint_roles (n+1) es cos
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

lintCoercion (KindCo co) = lintCoercion co

lintCoercion (SubCo co)
  = do { lintCoercion co
       ; lintRole co Nominal (coercionRole co) }

lintCoercion (HoleCo h)
  = failWithL (text "Unfilled coercion hole:" <+> ppr h)

{-
Note [Conflict checking for axiom applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following type family and axiom:

type family Equal (a :: k) (b :: k) :: Bool
type instance where
  Equal a a = True
  Equal a b = False
--
Equal :: forall k::*. k -> k -> Bool
axEqual :: { forall k::*. forall a::k. Equal k a a ~ True
           ; forall k::*. forall a::k. forall b::k. Equal k a b ~ False }

The coercion (axEqual[1] <*> <Int> <Int) is ill-typed, and Lint should reject it.
(Recall that the index is 0-based, so this is the second branch of the axiom.)
The problem is that, on the surface, it seems that

  (axEqual[1] <*> <Int> <Int>) :: (Equal * Int Int ~ False)

and that all is OK. But, all is not OK: we want to use the first branch of the
axiom in this case, not the second. The problem is that the parameters of the
first branch can unify with the supplied coercions, thus meaning that the first
branch should be taken. See also Note [Apartness] in "GHC.Core.FamInstEnv".

For more details, see the section "Branched axiom conflict checking" in
docs/core-spec, which defines the corresponding no_conflict function used by the
Co_AxiomInstCo rule in the section "Coercion typing".
-}

-- | Check to make sure that an axiom application is internally consistent.
-- Returns the conflicting branch, if it exists
-- Note [Conflict checking for axiom applications]
lintBranch :: Coercion -> TyCon-> CoAxBranch -> [Pair Type] -> LintM ()
-- defined here to avoid dependencies in GHC.Core.Coercion
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
lintBranch this_co fam_tc branch arg_kinds
  | CoAxBranch { cab_tvs = ktvs, cab_cvs = cvs } <- branch
  = do { checkL (arg_kinds `equalLength` (ktvs ++ cvs)) $
                (bad_ax this_co (text "lengths"))

       ; subst <- getSubst
       ; let empty_subst = zapSubst subst
       ; _ <- foldlM check_ki (empty_subst, empty_subst)
                              (zip (ktvs ++ cvs) arg_kinds)

       ; case check_no_conflict target incomps of
            Nothing -> return ()
            Just bad_branch -> failWithL $ bad_ax this_co $
                               text "inconsistent with" <+>
                                 pprCoAxBranch fam_tc bad_branch }
  where
    check_ki (subst_l, subst_r) (ktv, Pair s' t')
      = do { let sk' = typeKind s'
                 tk' = typeKind t'
           ; let ktv_kind_l = substTy subst_l (tyVarKind ktv)
                 ktv_kind_r = substTy subst_r (tyVarKind ktv)
           ; checkL (sk' `eqType` ktv_kind_l)
                    (bad_ax this_co (text "check_ki1" <+> vcat [ ppr this_co, ppr sk', ppr ktv, ppr ktv_kind_l ] ))
           ; checkL (tk' `eqType` ktv_kind_r)
                    (bad_ax this_co (text "check_ki2" <+> vcat [ ppr this_co, ppr tk', ppr ktv, ppr ktv_kind_r ] ))
           ; return (extendTCvSubst subst_l ktv s',
                     extendTCvSubst subst_r ktv t') }

    tvs          = coAxBranchTyVars branch
    cvs          = coAxBranchCoVars branch
    incomps      = coAxBranchIncomps branch
    (tys, cotys) = splitAtList tvs (map pFst arg_kinds)
    co_args      = map stripCoercionTy cotys
    subst        = zipTvSubst tvs tys `composeTCvSubst`
                   zipCvSubst cvs co_args
    target   = Type.substTys subst (coAxBranchLHS branch)

    check_no_conflict :: [Type] -> [CoAxBranch] -> Maybe CoAxBranch
    check_no_conflict _    [] = Nothing
    check_no_conflict flat (b@CoAxBranch { cab_lhs = lhs_incomp } : rest)
         -- See Note [Apartness] in GHC.Core.FamInstEnv
      | SurelyApart <- tcUnifyTysFG alwaysBindFam alwaysBindTv flat lhs_incomp
      = check_no_conflict flat rest
      | otherwise
      = Just b

bad_ax :: Coercion -> SDoc -> SDoc
bad_ax this_co what
    = hang (text "Bad axiom application" <+> parens what) 2 (ppr this_co)


{-
************************************************************************
*                                                                      *
              Axioms
*                                                                      *
************************************************************************
-}

lintAxioms :: Logger
           -> LintConfig
           -> SDoc -- ^ The source of the linted axioms
           -> [CoAxiom Branched]
           -> IO ()
lintAxioms logger cfg what axioms =
  displayLintResults logger True what (vcat $ map pprCoAxiom axioms) $
  initL cfg $
  do { mapM_ lint_axiom axioms
     ; let axiom_groups = groupWith coAxiomTyCon axioms
     ; mapM_ lint_axiom_group axiom_groups }

lint_axiom :: CoAxiom Branched -> LintM ()
lint_axiom ax@(CoAxiom { co_ax_tc = tc, co_ax_branches = branches
                       , co_ax_role = ax_role })
  = addLoc (InAxiom ax) $
    do { mapM_ (lint_branch tc) branch_list
       ; extra_checks }
  where
    branch_list = fromBranches branches

    extra_checks
      | isNewTyCon tc
      = do { CoAxBranch { cab_tvs     = ax_tvs
                        , cab_eta_tvs = eta_tvs
                        , cab_cvs     = cvs
                        , cab_roles   = roles
                        , cab_lhs     = lhs_tys }
              <- case branch_list of
               [branch] -> return branch
               _        -> failWithL (text "multi-branch axiom with newtype")

           -- The LHS of the axiom is (N lhs_tys)
           -- We expect it to be      (N ax_tvs)
           ; lintL (mkTyVarTys ax_tvs `eqTypes` lhs_tys)
                   (text "Newtype axiom LHS does not match newtype definition")
           ; lintL (null cvs)
                   (text "Newtype axiom binds coercion variables")
           ; lintL (null eta_tvs)  -- See Note [Eta reduction for data families]
                                   -- which is not about newtype axioms
                   (text "Newtype axiom has eta-tvs")
           ; lintL (ax_role == Representational)
                   (text "Newtype axiom role not representational")
           ; lintL (roles `equalLength` ax_tvs)
                   (text "Newtype axiom roles list is the wrong length." $$
                    text "roles:" <+> sep (map ppr roles))
           ; lintL (roles == takeList roles (tyConRoles tc))
                   (vcat [ text "Newtype axiom roles do not match newtype tycon's."
                         , text "axiom roles:" <+> sep (map ppr roles)
                         , text "tycon roles:" <+> sep (map ppr (tyConRoles tc)) ])
           }

      | isFamilyTyCon tc
      = do { if | isTypeFamilyTyCon tc
                  -> lintL (ax_role == Nominal)
                           (text "type family axiom is not nominal")

                | isDataFamilyTyCon tc
                  -> lintL (ax_role == Representational)
                           (text "data family axiom is not representational")

                | otherwise
                  -> addErrL (text "A family TyCon is neither a type family nor a data family:" <+> ppr tc)

           ; mapM_ (lint_family_branch tc) branch_list }

      | otherwise
      = addErrL (text "Axiom tycon is neither a newtype nor a family.")

lint_branch :: TyCon -> CoAxBranch -> LintM ()
lint_branch ax_tc (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                              , cab_lhs = lhs_args, cab_rhs = rhs })
  = lintBinders LambdaBind (tvs ++ cvs) $ \_ ->
    do { let lhs = mkTyConApp ax_tc lhs_args
       ; lintType lhs
       ; lintType rhs
       ; lhs_kind <- substTyM (typeKind lhs)
       ; rhs_kind <- substTyM (typeKind rhs)
       ; lintL (not (lhs_kind `typesAreApart` rhs_kind)) $
         hang (text "Inhomogeneous axiom")
            2 (text "lhs:" <+> ppr lhs <+> dcolon <+> ppr lhs_kind $$
               text "rhs:" <+> ppr rhs <+> dcolon <+> ppr rhs_kind) }
         -- Type and Constraint are not Apart, so this test allows
         -- the newtype axiom for a single-method class.  Indeed the
         -- whole reason Type and Constraint are not Apart is to allow
         -- such axioms!

-- these checks do not apply to newtype axioms
lint_family_branch :: TyCon -> CoAxBranch -> LintM ()
lint_family_branch fam_tc br@(CoAxBranch { cab_tvs     = tvs
                                         , cab_eta_tvs = eta_tvs
                                         , cab_cvs     = cvs
                                         , cab_roles   = roles
                                         , cab_lhs     = lhs
                                         , cab_incomps = incomps })
  = do { lintL (isDataFamilyTyCon fam_tc || null eta_tvs)
               (text "Type family axiom has eta-tvs")
       ; lintL (all (`elemVarSet` tyCoVarsOfTypes lhs) tvs)
               (text "Quantified variable in family axiom unused in LHS")
       ; lintL (all isTyFamFree lhs)
               (text "Type family application on LHS of family axiom")
       ; lintL (all (== Nominal) roles)
               (text "Non-nominal role in family axiom" $$
                text "roles:" <+> sep (map ppr roles))
       ; lintL (null cvs)
               (text "Coercion variables bound in family axiom")
       ; forM_ incomps $ \ br' ->
           lintL (not (compatibleBranches br br')) $
           hang (text "Incorrect incompatible branches:")
              2 (vcat [text "Branch:"       <+> ppr br,
                       text "Bogus incomp:" <+> ppr br']) }

lint_axiom_group :: NonEmpty (CoAxiom Branched) -> LintM ()
lint_axiom_group (_  :| []) = return ()
lint_axiom_group (ax :| axs)
  = do { lintL (isOpenFamilyTyCon tc)
               (text "Non-open-family with multiple axioms")
       ; let all_pairs = [ (ax1, ax2) | ax1 <- all_axs
                                      , ax2 <- all_axs ]
       ; mapM_ (lint_axiom_pair tc) all_pairs }
  where
    all_axs = ax : axs
    tc      = coAxiomTyCon ax

lint_axiom_pair :: TyCon -> (CoAxiom Branched, CoAxiom Branched) -> LintM ()
lint_axiom_pair tc (ax1, ax2)
  | Just br1@(CoAxBranch { cab_tvs = tvs1
                         , cab_lhs = lhs1
                         , cab_rhs = rhs1 }) <- coAxiomSingleBranch_maybe ax1
  , Just br2@(CoAxBranch { cab_tvs = tvs2
                         , cab_lhs = lhs2
                         , cab_rhs = rhs2 }) <- coAxiomSingleBranch_maybe ax2
  = lintL (compatibleBranches br1 br2) $
    vcat [ hsep [ text "Axioms", ppr ax1, text "and", ppr ax2
                , text "are incompatible" ]
         , text "tvs1 =" <+> pprTyVars tvs1
         , text "lhs1 =" <+> ppr (mkTyConApp tc lhs1)
         , text "rhs1 =" <+> ppr rhs1
         , text "tvs2 =" <+> pprTyVars tvs2
         , text "lhs2 =" <+> ppr (mkTyConApp tc lhs2)
         , text "rhs2 =" <+> ppr rhs2 ]

  | otherwise
  = addErrL (text "Open type family axiom has more than one branch: either" <+>
             ppr ax1 <+> text "or" <+> ppr ax2)

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

       , le_subst :: Subst
                  -- Current substitution, for TyCoVars only.
                  -- Non-CoVar Ids don't appear in here, not even in the InScopeSet
                  -- Used for (a) cloning to avoid shadowing of TyCoVars,
                  --              so that eqType works ok
                  --          (b) substituting for let-bound tyvars, when we have
                  --              (let @a = Int -> Int in ...)

       , le_in_vars :: VarEnv (InVar, OutType)
                    -- Maps an InVar (i.e. its unique) to its binding InVar
                    --    and to its OutType
                    -- /All/ in-scope variables are here (term variables,
                    --    type variables, and coercion variables)
                    -- Used at an occurrence of the InVar

       , le_joins :: IdSet     -- Join points in scope that are valid
                               -- A subset of the InScopeSet in le_subst
                               -- See Note [Join points]

       , le_ue_aliases :: NameEnv UsageEnv
             -- See Note [Linting linearity]
             -- Assigns usage environments to the alias-like binders,
             -- as found in non-recursive lets.
             -- Domain is OutIds

       , le_platform   :: Platform         -- ^ Target platform
       , le_diagOpts   :: DiagOpts         -- ^ Target platform
       }

data LintFlags
  = LF { lf_check_global_ids           :: Bool -- See Note [Checking for global Ids]
       , lf_check_inline_loop_breakers :: Bool -- See Note [Checking for INLINE loop breakers]
       , lf_check_static_ptrs :: StaticPtrCheck -- ^ See Note [Checking StaticPtrs]
       , lf_report_unsat_syns :: Bool -- ^ See Note [Linting type synonym applications]
       , lf_check_linearity :: Bool -- ^ See Note [Linting linearity]
       , lf_check_fixed_rep :: Bool -- See Note [Checking for representation polymorphism]
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

newtype LintM a =
   LintM' { unLintM ::
            LintEnv ->
            WarnsAndErrs ->           -- Warning and error messages so far
            LResult a } -- Result and messages (if any)


pattern LintM :: (LintEnv -> WarnsAndErrs -> LResult a) -> LintM a
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern LintM m <- LintM' m
  where
    LintM m = LintM' (oneShot $ \env -> oneShot $ \we -> m env we)
    -- LintM m = LintM' (oneShot $ oneShot m)
{-# COMPLETE LintM #-}

instance Functor (LintM) where
  fmap f (LintM m) = LintM $ \e w -> mapLResult f (m e w)

type WarnsAndErrs = (Bag SDoc, Bag SDoc)

-- Using a unboxed tuple here reduced allocations for a lint heavy
-- file by ~6%. Using MaybeUB reduced them further by another ~12%.
--
-- Warning: if you don't inline the matcher for JustUB etc, Lint becomes
-- /tremendously/ inefficient, and compiling GHC.Tc.Errors.Types (which
-- contains gigantic types) is very very slow indeed. Conclusion: make
-- sure unfoldings are expose in GHC.Data.Unboxed, and that you compile
-- Lint.hs with optimistation on.
type LResult a = (# MaybeUB a, WarnsAndErrs #)

pattern LResult :: MaybeUB a -> WarnsAndErrs -> LResult a
pattern LResult m w = (# m, w #)
{-# COMPLETE LResult #-}

mapLResult :: (a1 -> a2) -> LResult a1 -> LResult a2
mapLResult f (LResult r w) = LResult (fmapMaybeUB f r) w

-- Just for testing.
fromBoxedLResult :: (Maybe a, WarnsAndErrs) -> LResult a
fromBoxedLResult (Just x, errs) = LResult (JustUB x) errs
fromBoxedLResult (Nothing,errs) = LResult NothingUB errs

{- Note [Checking for global Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before CoreTidy, all locally-bound Ids must be LocalIds, even
top-level ones. See Note [Exported LocalIds] and #9857.

Note [Checking StaticPtrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable for an overview.

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

Note [Linting linearity]
~~~~~~~~~~~~~~~~~~~~~~~~
Lint ignores linearity unless `-dlinear-core-lint` is set.  For why, see below.

* When do we /check linearity/ in Lint?  That is, when is `-dlinear-core-lint`
  lint set?  Answer: we check linearity in the output of the desugarer, shortly
  after type checking.

* When so we /not/ check linearity in Lint?  On all passes after desugaring.  Why?
  Because optimisation passes are not (yet) guaranteed to maintain linearity.
  They should do so semantically (GHC is careful not to duplicate computation)
  but it is much harder to ensure that the statically-checkable constraints of
  Linear Core are maintained. See examples below.

The current Linear Core is described in the wiki at:
https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types/implementation.

Concretely, "ignore linearity in Lint" specifically means two things:
* In `ensureEqTypes`, use `eqTypeIgnoringMultiplicity`
* In `ensureSubMult`, do nothing

Here are some examples of how the optimiser can break linearity checking.  Other
examples are documented in the linear-type implementation wiki page
[https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types/implementation#core-to-core-passes]

* EXAMPLE 1: the binder swap transformation
    Consider

      data T = MkT {-# UNPACK #-} !Int

    The wrapper for MkT is

      $wMkT :: Int %1 -> T
      $wMkT n = case %1 n of
        I# n' -> MkT n'

    This introduces, in particular, a `case %1` (this is not actual Haskell or
    Core syntax), where the `%1` means that the `case` expression consumes its
    scrutinee linearly.

    Now, `case %1` interacts with the binder swap optimisation in a non-trivial
    way. Take a slightly modified version of the code for $wMkT:

      case %1 x of z {
        I# n' -> (x, n')
      }

    Binder-swap changes this to

      case %1 x of z {
        I# n' -> let x = z in (x, n')
      }

    This is rejected by `-dlinear-core-lint` because 1/ n' must be used linearly
    2/ `-dlinear-core-lint` recognises a use of `z` as a use of `n'`. So it sees
    two uses of n' where there should be a single one.

* EXAMPLE 2: letrec
    Some optimisations can create a letrec which uses a variable
    linearly, e.g.

      letrec f True = f False
             f False = x
      in f True

    uses 'x' linearly, but this is not seen by the linter, which considers,
    conservatively, that a letrec always has multiplicity Many (in particular
    that every captured free variable must have multiplicity Many). This issue
    is discussed in ticket #18694.

* EXAMPLE 3: rewrite rules
    Ignoring linearity means in particular that `a -> b` and `a %1 -> b` must be
    treated the same by rewrite rules (see also Note [Rewrite rules ignore
    multiplicities in FunTy] in GHC.Core.Unify). Consider

      m :: Bool -> A
      m' :: (Bool -> Bool) -> A
      {- RULES "ex" forall f. m (f True) = m' f -}

      f :: Bool %1 -> A
      x = m (f True)

    The rule "ex" must match . So the linter must accept `m' f`.

* EXAMPLE 4: eta-reduction
   Eta-expansion can change linear functions into unrestricted functions

     f :: A %1 -> B

     g :: A %Many -> B
     g = \x -> f x

   Eta-reduction undoes this and produces:

     g :: A %Many -> B
     g = f

Historical note: In the original linear-types implementation, we had tried to
make every optimisation pass produce code that passes `-dlinear-core-lint`. It
had proved very difficult. We kept finding corner case after corner
case. Furthermore, to attempt to achieve that goal we ended up restricting
transformations when `-dlinear-core-lint` couldn't typecheck the result.

In the future, we may be able to lint the linearity of the output of
Core-to-Core passes (#19165). But this shouldn't be done at the expense of
producing efficient code. Therefore we lay the following principle.

PRINCIPLE: The type system bends to the optimisation, not the other way around.

There is a useful discussion at https://gitlab.haskell.org/ghc/ghc/-/issues/22123

Note [Linting representation-polymorphic builtins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Representation-polymorphism checking built-ins], on
top of the two main representation-polymorphism invariants described in the
Note [Representation polymorphism invariants], we must perform additional
representation-polymorphism checks on builtin functions which don't have a
binding, for example to ensure that we don't run afoul of the
representation-polymorphism invariants when eta-expanding.

There are two situations:

  1. Builtins which have skolem type variables which must be instantiated to
     concrete types, such as the RuntimeRep type argument r to the catch# primop.

  2. Representation-polymorphic unlifted newtypes, which must always be instantiated
     at a fixed runtime representation.

For 1, consider for example 'coerce':

  coerce :: forall {r} (a :: TYPE r) (b :: TYPE r). Coercible a b => a -> b

We store in the IdDetails of the coerce Id that the first binder, r, must always
be instantiated to a concrete type. We thus check this in Core Lint: whenever we
see an application of the form

  coerce @{rep1} ...

we ensure that 'rep1' is concrete. This is done in the function "checkRepPolyBuiltinApp".
Moreover, not instantiating these type variables at all is also an error, as
we would again not be able to perform eta-expansion. (This is a bit more theoretical,
as in user programs the typechecker will insert these type applications when
instantiating, but it can still arise when constructing Core expressions).

For 2, whenever we have an unlifted newtype such as

  type RR :: Type -> RuntimeRep
  type family RR a

  type F :: forall (a :: Type) -> TYPE (RR a)
  type family F a

  type N :: forall (a :: Type) -> TYPE (RR a)
  newtype N a = MkN (F a)

and an unsaturated occurrence

  MkN @ty -- NB: no value argument!

we check that the (instantiated) argument type has a fixed runtime representation.
This is done in the function "checkRepPolyNewtypeApp".
-}

instance Applicative LintM where
      pure x = LintM $ \ _ errs -> LResult (JustUB x) errs
                                   --(Just x, errs)
      (<*>) = ap

instance Monad LintM where
  m >>= k  = LintM (\ env errs ->
                       let res = unLintM m env errs in
                         case res of
                           LResult (JustUB r) errs' -> unLintM (k r) env errs'
                           LResult NothingUB errs' -> LResult NothingUB errs'
                    )
                          --  LError errs'-> LError errs')
                      --  let (res, errs') = unLintM m env errs in
                          --  Just r -> unLintM (k r) env errs'
                          --  Nothing -> (Nothing, errs'))

instance MonadFail LintM where
    fail err = failWithL (text err)

getPlatform :: LintM Platform
getPlatform = LintM (\ e errs -> (LResult (JustUB $ le_platform e) errs))

data LintLocInfo
  = RhsOf Id            -- The variable bound
  | OccOf Id            -- Occurrence of id
  | LambdaBodyOf Id     -- The lambda-binder
  | RuleOf Id           -- Rules attached to a binder
  | UnfoldingOf Id      -- Unfolding of a binder
  | BodyOfLet Id        -- The let-bound variable
  | BodyOfLetRec [Id]   -- The binders of the let
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
  | InAxiom (CoAxiom Branched)   -- Inside a CoAxiom

data LintConfig = LintConfig
  { l_diagOpts   :: !DiagOpts         -- ^ Diagnostics opts
  , l_platform   :: !Platform         -- ^ Target platform
  , l_flags      :: !LintFlags        -- ^ Linting the result of this pass
  , l_vars       :: ![Var]            -- ^ 'Id's that should be treated as being in scope
  }

initL :: LintConfig
      -> LintM a            -- ^ Action to run
      -> WarnsAndErrs
initL cfg m
  = case unLintM m env (emptyBag, emptyBag) of
      LResult (JustUB _) errs -> errs
      LResult NothingUB errs@(_, e) | not (isEmptyBag e) -> errs
                                    | otherwise -> pprPanic ("Bug in Lint: a failure occurred " ++
                                                      "without reporting an error message") empty
  where
    vars = l_vars cfg
    env = LE { le_flags   = l_flags cfg
             , le_subst   = mkEmptySubst (mkInScopeSetList vars)
             , le_in_vars = mkVarEnv [ (v,(v, varType v)) | v <- vars ]
             , le_joins   = emptyVarSet
             , le_loc     = []
             , le_ue_aliases = emptyNameEnv
             , le_platform = l_platform cfg
             , le_diagOpts = l_diagOpts cfg
             }

setReportUnsat :: Bool -> LintM a -> LintM a
-- Switch off lf_report_unsat_syns
setReportUnsat ru thing_inside
  = LintM $ \ env errs ->
    let env' = env { le_flags = (le_flags env) { lf_report_unsat_syns = ru } }
    in unLintM thing_inside env' errs

-- See Note [Checking for representation polymorphism]
noFixedRuntimeRepChecks :: LintM a -> LintM a
noFixedRuntimeRepChecks thing_inside
  = LintM $ \env errs ->
    let env' = env { le_flags = (le_flags env) { lf_check_fixed_rep = False } }
    in unLintM thing_inside env' errs

getLintFlags :: LintM LintFlags
getLintFlags = LintM $ \ env errs -> fromBoxedLResult (Just (le_flags env), errs)

checkL :: Bool -> SDoc -> LintM ()
checkL True  _   = return ()
checkL False msg = failWithL msg

-- like checkL, but relevant to type checking
lintL :: Bool -> SDoc -> LintM ()
lintL = checkL

checkWarnL :: Bool -> SDoc -> LintM ()
checkWarnL True   _  = return ()
checkWarnL False msg = addWarnL msg

failWithL :: SDoc -> LintM a
failWithL msg = LintM $ \ env (warns,errs) ->
                fromBoxedLResult (Nothing, (warns, addMsg True env errs msg))

addErrL :: SDoc -> LintM ()
addErrL msg = LintM $ \ env (warns,errs) ->
              fromBoxedLResult (Just (), (warns, addMsg True env errs msg))

addWarnL :: SDoc -> LintM ()
addWarnL msg = LintM $ \ env (warns,errs) ->
              fromBoxedLResult (Just (), (addMsg True env warns msg, errs))

addMsg :: Bool -> LintEnv ->  Bag SDoc -> SDoc -> Bag SDoc
addMsg show_context env msgs msg
  = assertPpr (notNull loc_msgs) msg $
    msgs `snocBag` mk_msg msg
  where
   loc_msgs :: [(SrcLoc, SDoc)]  -- Innermost first
   loc_msgs = map dumpLoc (le_loc env)

   cxt_doc = vcat [ vcat $ reverse $ map snd loc_msgs
                  , text "Substitution:" <+> ppr (le_subst env) ]

   context | show_context  = cxt_doc
           | otherwise     = whenPprDebug cxt_doc
     -- Print voluminous info for Lint errors
     -- but not for warnings

   msg_span = case [ span | (loc,_) <- loc_msgs
                          , let span = srcLocSpan loc
                          , isGoodSrcSpan span ] of
               []    -> noSrcSpan
               (s:_) -> s
   !diag_opts = le_diagOpts env
   mk_msg msg = mkLocMessage (mkMCDiagnostic diag_opts WarningWithoutFlag Nothing) msg_span
                             (msg $$ context)

addLoc :: LintLocInfo -> LintM a -> LintM a
addLoc extra_loc m
  = LintM $ \ env errs ->
    unLintM m (env { le_loc = extra_loc : le_loc env }) errs

inCasePat :: LintM Bool         -- A slight hack; see the unique call site
inCasePat = LintM $ \ env errs -> fromBoxedLResult (Just (is_case_pat env), errs)
  where
    is_case_pat (LE { le_loc = CasePat {} : _ }) = True
    is_case_pat _other                           = False

addInScopeId :: InId -> OutType -> (OutId -> LintM a) -> LintM a
-- Unlike addInScopeTyCoVar, this function does no cloning; Ids never get cloned
addInScopeId in_id out_ty thing_inside
  = LintM $ \ env errs ->
    let !(out_id, env') = add env
    in unLintM (thing_inside out_id) env' errs

  where
    add env@(LE { le_in_vars = id_vars, le_joins = join_set
                , le_ue_aliases = aliases, le_subst = subst })
      = (out_id, env1)
      where
        env1 = env { le_in_vars = in_vars', le_joins = join_set', le_ue_aliases = aliases' }

        in_vars' = extendVarEnv id_vars in_id (in_id, out_ty)
        aliases' = delFromNameEnv aliases (idName in_id)
           -- aliases': when shadowing an alias, we need to make sure the
           -- Id is no longer classified as such. E.g.
           --   let x = <e1> in case x of x { _DEFAULT -> <e2> }
           -- Occurrences of 'x' in e2 shouldn't count as occurrences of e1.

        -- A very tiny optimisation, not sure if it's really worth it
        -- Short-cut when the substitution is a no-op
        out_id | isEmptyTCvSubst subst = in_id
               | otherwise             = setIdType in_id out_ty

        join_set'
          | isJoinId out_id = extendVarSet join_set in_id -- Overwrite with new arity
          | otherwise       = delVarSet    join_set in_id -- Remove any existing binding

addInScopeTyCoVar :: InTyCoVar -> OutType -> (OutTyCoVar -> LintM a) -> LintM a
-- This function clones to avoid shadowing of TyCoVars
addInScopeTyCoVar tcv tcv_type thing_inside
  = LintM $ \ env@(LE { le_in_vars = in_vars, le_subst = subst }) errs ->
    let (tcv', subst') = subst_bndr subst
        env' = env { le_in_vars = extendVarEnv in_vars tcv (tcv, tcv_type)
                   , le_subst = subst' }
    in unLintM (thing_inside tcv') env' errs
  where
    subst_bndr subst
      | isEmptyTCvSubst subst                -- No change in kind
      , not (tcv `elemInScopeSet` in_scope)  -- Not already in scope
      = -- Do not extend the substitution, just the in-scope set
        (if (varType tcv `eqType` tcv_type) then (\x->x) else
          pprTrace "addInScopeTyCoVar" (
            vcat [ text "tcv" <+> ppr tcv <+> dcolon <+> ppr (varType tcv)
                 , text "tcv_type" <+> ppr tcv_type ])) $
        (tcv, subst `extendSubstInScope` tcv)

      -- Clone, and extend the substitution
      | let tcv' = uniqAway in_scope (setVarType tcv tcv_type)
      = (tcv', extendTCvSubstWithClone subst tcv tcv')
      where
        in_scope = substInScopeSet subst

getInVarEnv :: LintM (VarEnv (InId, OutType))
getInVarEnv = LintM (\env errs -> fromBoxedLResult (Just (le_in_vars env), errs))

extendTvSubstL :: TyVar -> Type -> LintM a -> LintM a
extendTvSubstL tv ty m
  = LintM $ \ env errs ->
    unLintM m (env { le_subst = Type.extendTvSubst (le_subst env) tv ty }) errs

markAllJoinsBad :: LintM a -> LintM a
markAllJoinsBad m
  = LintM $ \ env errs -> unLintM m (env { le_joins = emptyVarSet }) errs

markAllJoinsBadIf :: Bool -> LintM a -> LintM a
markAllJoinsBadIf True  m = markAllJoinsBad m
markAllJoinsBadIf False m = m

getValidJoins :: LintM IdSet
getValidJoins = LintM (\ env errs -> fromBoxedLResult (Just (le_joins env), errs))

getSubst :: LintM Subst
getSubst = LintM (\ env errs -> fromBoxedLResult (Just (le_subst env), errs))

substTyM :: InType -> LintM OutType
-- Apply the substitution to the type
-- The substitution is often empty, in which case it is a no-op
substTyM ty
  = do { subst <- getSubst
       ; return (substTy subst ty) }

getUEAliases :: LintM (NameEnv UsageEnv)
getUEAliases = LintM (\ env errs -> fromBoxedLResult (Just (le_ue_aliases env), errs))

getInScope :: LintM InScopeSet
getInScope = LintM (\ env errs -> fromBoxedLResult (Just (substInScopeSet $ le_subst env), errs))

lintVarOcc :: InVar -> LintM OutType
-- Used at an occurrence of a variable: term variables, type variables, and coercion variables
-- Checks two things:
-- a) that it is in scope
-- b) that the InType at the ocurrences matches the InType at the binding site
lintVarOcc v_occ
  = do { in_var_env <- getInVarEnv
       ; case lookupVarEnv in_var_env v_occ of
           Nothing | isGlobalId v_occ -> return (idType v_occ)
                   | otherwise        -> failWithL (text pp_what <+> quotes (ppr v_occ)
                                                    <+> text "is out of scope")
           Just (v_bndr, out_ty) -> do { check_bad_global v_bndr
                                       ; ensureEqTys occ_ty bndr_ty $  -- Compares InTypes
                                         mkBndrOccTypeMismatchMsg v_occ bndr_ty occ_ty
                                       ; return out_ty }
             where
               occ_ty  = varType v_occ
               bndr_ty = varType v_bndr }
  where
    pp_what | isTyVar v_occ = "The type variable"
            | isCoVar v_occ = "The coercion variable"
            | otherwise     = "The value variable"

       -- 'check_bad_global' checks for the case where an /occurrence/ is
       -- a GlobalId, but there is an enclosing binding fora a LocalId.
       -- NB: the in-scope variables are mostly LocalIds, checked by lintIdBndr,
       --     but GHCi adds GlobalIds from the interactive context.  These
       --     are fine; hence the test (isLocalId id == isLocalId v)
       -- NB: when compiling Control.Exception.Base, things like absentError
       --     are defined locally, but appear in expressions as (global)
       --     wired-in Ids after worker/wrapper
       --     So we simply disable the test in this case
    check_bad_global v_bndr
      | isGlobalId v_occ
      , isLocalId v_bndr
      , not (isWiredIn v_occ)
      = failWithL $ hang (text "Occurrence is GlobalId, but binding is LocalId")
                       2 (vcat [ hang (text "occurrence:") 2 $ pprBndr LetBind v_occ
                               , hang (text "binder    :") 2 $ pprBndr LetBind v_bndr ])
      | otherwise
      = return ()

lookupJoinId :: Id -> LintM JoinPointHood
-- Look up an Id which should be a join point, valid here
-- If so, return its arity, if not return Nothing
lookupJoinId id
  = do { join_set <- getValidJoins
       ; case lookupVarSet join_set id of
            Just id' -> return (idJoinPointHood id')
            Nothing  -> return NotJoinPoint }

addAliasUE :: OutId -> UsageEnv -> LintM a -> LintM a
addAliasUE id ue thing_inside = LintM $ \ env errs ->
  let new_ue_aliases =
        extendNameEnv (le_ue_aliases env) (getName id) ue
  in
    unLintM thing_inside (env { le_ue_aliases = new_ue_aliases }) errs

varCallSiteUsage :: OutId -> LintM UsageEnv
varCallSiteUsage id =
  do m <- getUEAliases
     return $ case lookupNameEnv m (getName id) of
         Nothing    -> singleUsageUE id
         Just id_ue -> id_ue

ensureEqTys :: OutType -> OutType -> SDoc -> LintM ()
-- check ty2 is subtype of ty1 (ie, has same structure but usage
-- annotations need only be consistent, not equal)
-- Assumes ty1,ty2 are have already had the substitution applied
{-# INLINE ensureEqTys #-} -- See Note [INLINE ensureEqTys]
ensureEqTys ty1 ty2 msg
  = do { flags <- getLintFlags
       ; lintL (eq_type flags ty1 ty2) msg }

eq_type :: LintFlags -> Type -> Type -> Bool
-- When `-dlinear-core-lint` is off, then consider `a -> b` and `a %1 -> b` to
-- be equal. See Note [Linting linearity].
eq_type flags ty1 ty2 | lf_check_linearity flags = eqType                     ty1 ty2
                      | otherwise                = eqTypeIgnoringMultiplicity ty1 ty2

{- Note [INLINE ensureEqTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To make Lint fast, we want to avoid allocating a thunk for <msg> in
      ensureEqTypes ty1 ty2 <msg>
because the test almost always succeeds, and <msg> isn't needed.
So we INLINE `ensureEqTys`.  This actually make a difference of
1-2% when compiling programs with -dcore-lint.
-}

ensureSubUsage :: Usage -> Mult -> SDoc -> LintM ()
ensureSubUsage Bottom     _              _ = return ()
ensureSubUsage Zero       described_mult err_msg = ensureSubMult ManyTy described_mult err_msg
ensureSubUsage (MUsage m) described_mult err_msg = ensureSubMult m described_mult err_msg

ensureSubMult :: Mult -> Mult -> SDoc -> LintM ()
ensureSubMult actual_mult described_mult err_msg = do
    flags <- getLintFlags
    when (lf_check_linearity flags) $
      unless (deepSubMult actual_mult described_mult) $
        addErrL err_msg
  where
    -- Check for submultiplicity using the following rules:
    -- 1. x*y <= z when x <= z and y <= z.
    --    This rule follows from the fact that x*y = sup{x,y} for any
    --    multiplicities x,y.
    -- 2. x <= y*z when x <= y or x <= z.
    --    This rule is not complete: when x = y*z, we cannot
    --    change y*z <= y*z to y*z <= y or y*z <= z.
    --    However, we eliminate products on the LHS in step 1.
    -- 3. One <= x and x <= Many for any x, as checked by 'submult'.
    -- 4. x <= x.
    -- Otherwise, we fail.
    deepSubMult :: Mult -> Mult -> Bool
    deepSubMult m n
      | Just (m1, m2) <- isMultMul m = deepSubMult m1 n  && deepSubMult m2 n
      | Just (n1, n2) <- isMultMul n = deepSubMult m  n1 || deepSubMult m  n2
      | Submult <- m `submult` n = True
      | otherwise = m `eqType` n

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

dumpLoc (RuleOf b)
  = (getSrcLoc b, text "In a rule attached to" <+> pp_binder b)

dumpLoc (UnfoldingOf b)
  = (getSrcLoc b, text "In the unfolding of" <+> pp_binder b)

dumpLoc (BodyOfLet b)
  = (noSrcLoc, text "In the body of a let with binder" <+> pp_binder b)

dumpLoc (BodyOfLetRec [])
  = (noSrcLoc, text "In body of a letrec with no binders")

dumpLoc (BodyOfLetRec bs@(b:_))
  = ( getSrcLoc b, text "In the body of a letrec with binders" <+> pp_binders bs)

dumpLoc (AnExpr e)
  = (noSrcLoc, text "In the expression:" <+> ppr e)

dumpLoc (CaseAlt (Alt con args _))
  = (noSrcLoc, text "In a case alternative:" <+> parens (ppr con <+> pp_binders args))

dumpLoc (CasePat (Alt con args _))
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
dumpLoc (InAxiom ax)
  = (getSrcLoc ax, hang (text "In the coercion axiom")
                      2 (pprCoAxiom ax))

pp_binders :: [Var] -> SDoc
pp_binders bs = sep (punctuate comma (map pp_binder bs))

pp_binder :: Var -> SDoc
pp_binder b | isId b    = hsep [ppr b, dcolon, ppr (idType b)]
            | otherwise = hsep [ppr b, dcolon, ppr (tyVarKind b)]

------------------------------------------------------
--      Messages for case expressions

mkDefaultArgsMsg :: [Var] -> SDoc
mkDefaultArgsMsg args
  = hang (text "DEFAULT case with binders")
         4 (ppr args)

mkCaseAltMsg :: CoreExpr -> Type -> Type -> SDoc
mkCaseAltMsg e ty1 ty2
  = hang (text "Type of case alternatives not the same as the annotation on case:")
         4 (vcat [ text "Actual type:" <+> ppr ty1,
                   text "Annotation on case:" <+> ppr ty2,
                   text "Alt Rhs:" <+> ppr e ])

mkScrutMsg :: Id -> Type -> Type -> SDoc
mkScrutMsg var var_ty scrut_ty
  = vcat [text "Result binder in case doesn't match scrutinee:" <+> ppr var,
          text "Result binder type:" <+> ppr var_ty,--(idType var),
          text "Scrutinee type:" <+> ppr scrut_ty]

mkNonDefltMsg, mkNonIncreasingAltsMsg :: CoreExpr -> SDoc
mkNonDefltMsg e
  = hang (text "Case expression with DEFAULT not at the beginning") 4 (ppr e)
mkNonIncreasingAltsMsg e
  = hang (text "Case expression with badly-ordered alternatives") 4 (ppr e)

nonExhaustiveAltsMsg :: CoreExpr -> SDoc
nonExhaustiveAltsMsg e
  = hang (text "Case expression with non-exhaustive alternatives") 4 (ppr e)

mkBadConMsg :: TyCon -> DataCon -> SDoc
mkBadConMsg tycon datacon
  = vcat [
        text "In a case alternative, data constructor isn't in scrutinee type:",
        text "Scrutinee type constructor:" <+> ppr tycon,
        text "Data con:" <+> ppr datacon
    ]

mkBadPatMsg :: Type -> Type -> SDoc
mkBadPatMsg con_result_ty scrut_ty
  = vcat [
        text "In a case alternative, pattern result type doesn't match scrutinee type:",
        text "Pattern result type:" <+> ppr con_result_ty,
        text "Scrutinee type:" <+> ppr scrut_ty
    ]

integerScrutinisedMsg :: SDoc
integerScrutinisedMsg
  = text "In a LitAlt, the literal is lifted (probably Integer)"

mkBadAltMsg :: Type -> CoreAlt -> SDoc
mkBadAltMsg scrut_ty alt
  = vcat [ text "Data alternative when scrutinee is not a tycon application",
           text "Scrutinee type:" <+> ppr scrut_ty,
           text "Alternative:" <+> pprCoreAlt alt ]

mkNewTyDataConAltMsg :: Type -> CoreAlt -> SDoc
mkNewTyDataConAltMsg scrut_ty alt
  = vcat [ text "Data alternative for newtype datacon",
           text "Scrutinee type:" <+> ppr scrut_ty,
           text "Alternative:" <+> pprCoreAlt alt ]


------------------------------------------------------
--      Other error messages

mkAppMsg :: Type -> Type -> CoreExpr -> SDoc
mkAppMsg expected_arg_ty actual_arg_ty arg
  = vcat [text "Argument value doesn't match argument type:",
              hang (text "Expected arg type:") 4 (ppr expected_arg_ty),
              hang (text "Actual arg type:") 4 (ppr actual_arg_ty),
              hang (text "Arg:") 4 (ppr arg)]

mkNonFunAppMsg :: Type -> Type -> CoreExpr -> SDoc
mkNonFunAppMsg fun_ty arg_ty arg
  = vcat [text "Non-function type in function position",
              hang (text "Fun type:") 4 (ppr fun_ty),
              hang (text "Arg type:") 4 (ppr arg_ty),
              hang (text "Arg:") 4 (ppr arg)]

mkLetErr :: TyVar -> CoreExpr -> SDoc
mkLetErr bndr rhs
  = vcat [text "Bad `let' binding:",
          hang (text "Variable:")
                 4 (ppr bndr <+> dcolon <+> ppr (varType bndr)),
          hang (text "Rhs:")
                 4 (ppr rhs)]

mkTyAppMsg :: OutType -> Type -> SDoc
mkTyAppMsg ty arg_ty
  = vcat [text "Illegal type application:",
              hang (text "Function type:")
                 4 (ppr ty <+> dcolon <+> ppr (typeKind ty)),
              hang (text "Type argument:")
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

emptyRec :: CoreExpr -> SDoc
emptyRec e = hang (text "Empty Rec binding:") 2 (ppr e)

mkRhsMsg :: Id -> SDoc -> Type -> SDoc
mkRhsMsg binder what ty
  = vcat
    [hsep [text "The type of this binder doesn't match the type of its" <+> what <> colon,
            ppr binder],
     hsep [text "Binder's type:", ppr (idType binder)],
     hsep [text "Rhs type:", ppr ty]]

badBndrTyMsg :: Id -> SDoc -> SDoc
badBndrTyMsg binder what
  = vcat [ text "The type of this binder is" <+> what <> colon <+> ppr binder
         , text "Binder's type:" <+> ppr (idType binder) ]

mkNonTopExportedMsg :: Id -> SDoc
mkNonTopExportedMsg binder
  = hsep [text "Non-top-level binder is marked as exported:", ppr binder]

mkNonTopExternalNameMsg :: Id -> SDoc
mkNonTopExternalNameMsg binder
  = hsep [text "Non-top-level binder has an external name:", ppr binder]

mkTopNonLitStrMsg :: Id -> SDoc
mkTopNonLitStrMsg binder
  = hsep [text "Top-level Addr# binder has a non-literal rhs:", ppr binder]

mkKindErrMsg :: TyVar -> Type -> SDoc
mkKindErrMsg tyvar arg_ty
  = vcat [text "Kinds don't match in type application:",
          hang (text "Type variable:")
                 4 (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)),
          hang (text "Arg type:")
                 4 (ppr arg_ty <+> dcolon <+> ppr (typeKind arg_ty))]

mkCastErr :: CoreExpr -> Coercion -> Type -> Type -> SDoc
mkCastErr expr = mk_cast_err "expression" "type" (ppr expr)

mkCastTyErr :: Type -> Coercion -> Kind -> Kind -> SDoc
mkCastTyErr ty = mk_cast_err "type" "kind" (ppr ty)

mk_cast_err :: String -- ^ What sort of casted thing this is
                      --   (\"expression\" or \"type\").
            -> String -- ^ What sort of coercion is being used
                      --   (\"type\" or \"kind\").
            -> SDoc   -- ^ The thing being casted.
            -> Coercion -> Type -> Type -> SDoc
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
mkBadJoinArityMsg var ar n rhs
  = vcat [ text "Join point has too few lambdas",
           text "Join var:" <+> ppr var,
           text "Join arity:" <+> ppr ar,
           text "Number of lambdas:" <+> ppr (ar - n),
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

mkBndrOccTypeMismatchMsg :: InVar -> InType -> InType -> SDoc
mkBndrOccTypeMismatchMsg var bndr_ty occ_ty
  = vcat [ text "Mismatch in type between binder and occurrence"
         , text "Binder:    " <+> ppr var <+> dcolon <+> ppr bndr_ty
         , text "Occurrence:" <+> ppr var <+> dcolon <+> ppr occ_ty ]

mkBadJoinPointRuleMsg :: JoinId -> JoinArity -> CoreRule -> SDoc
mkBadJoinPointRuleMsg bndr join_arity rule
  = vcat [ text "Join point has rule with wrong number of arguments"
         , text "Var:" <+> ppr bndr
         , text "Join arity:" <+> ppr join_arity
         , text "Rule:" <+> ppr rule ]

dupVars :: [NonEmpty Var] -> SDoc
dupVars vars
  = hang (text "Duplicate variables brought into scope")
       2 (ppr (map toList vars))

dupExtVars :: [NonEmpty Name] -> SDoc
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
lintAnnots pname pass guts = {-# SCC "lintAnnots" #-} do
  -- Run the pass as we normally would
  dflags <- getDynFlags
  logger <- getLogger
  when (gopt Opt_DoAnnotationLinting dflags) $
    liftIO $ Err.showPass logger "Annotation linting - first run"
  -- If appropriate re-run it without debug annotations to make sure
  -- that they made no difference.
  if gopt Opt_DoAnnotationLinting dflags
    then do
      nguts <- pass guts
      liftIO $ Err.showPass logger "Annotation linting - second run"
      nguts' <- withoutAnnots pass guts
      -- Finally compare the resulting bindings
      liftIO $ Err.showPass logger "Annotation linting - comparison"
      let binds = flattenBinds $ mg_binds nguts
          binds' = flattenBinds $ mg_binds nguts'
          (diffs,_) = diffBinds True (mkRnEnv2 emptyInScopeSet) binds binds'
      when (not (null diffs)) $ GHC.Core.Opt.Monad.putMsg $ vcat
        [ lint_banner "warning" pname
        , text "Core changes with annotations:"
        , withPprStyle defaultDumpStyle $ nest 2 $ vcat diffs
        ]
      return nguts
    else
      pass guts

-- | Run the given pass without annotations. This means that we both
-- set the debugLevel setting to 0 in the environment as well as all
-- annotations from incoming modules.
withoutAnnots :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
withoutAnnots pass guts = do
  -- Remove debug flag from environment.
  -- TODO: supply tag here as well ?
  let withoutFlag = mapDynFlagsCoreM $ \(!dflags) -> dflags { debugLevel = 0 }
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
  -- Perform pass with all changes applied. Drop the simple count so it doesn't
  -- effect the total also
  dropSimplCount $ withoutFlag $ pass (nukeAnnotsMod guts)
