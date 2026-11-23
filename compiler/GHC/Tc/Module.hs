{-# LANGUAGE CPP #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking a whole module
--
-- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/type-checker
module GHC.Tc.Module (
        tcRnStmt, tcRnExpr, TcRnExprMode(..),
        tcRnType, tcRnTypeSkolemising,
        tcRnImportDecls,
        tcRnLookupRdrName,
        getModuleInterface,
        tcRnDeclsi,
        isGHCiMonad,
        getGHCiMonad,
        getInteractivePrintName,
        runTcInteractive,    -- Used by GHC API clients (#8878)
        withTcMPlugins,      -- Used by GHC API clients (#20499)
        tcRnLookupName,
        tcRnGetInfo,
        TcRnModuleOptions(..),
        tcRnModule, tcRnModuleTcRnM,
        tcTopSrcDecls,
        rnTopSrcDecls,
        checkBootDecl, checkHiBootIface',
        findExtraSigImports,
        implicitRequirements,
        checkUnit,
        mergeSignatures,
        tcRnMergeSignatures,
        instantiateSignature,
        tcRnInstantiateSignature,
        loadUnqualIfaces,
        -- More private...
        checkBootDeclM,
        getRenamedStuff, RenamedStuff
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Plugins
import GHC.Driver.DynFlags
import GHC.Driver.Config.Diagnostic
import GHC.IO.Unsafe ( unsafeInterleaveIO )

import GHC.Tc.Errors.Types
import {-# SOURCE #-} GHC.Tc.Gen.Splice ( finishTH, runRemoteModFinalizers )
import GHC.Tc.Gen.HsType
import GHC.Tc.Validity( checkValidType )
import GHC.Tc.Gen.Match
import GHC.Tc.Utils.Unify( checkConstraints, tcSubTypeSigma )
import GHC.Tc.Zonk.Type
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Monad
import GHC.Tc.Gen.Export
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.ErrCtxt( ReportRedundantConstraints(..) )
import GHC.Tc.Types.Origin
import GHC.Tc.Instance.Family
import GHC.Tc.Gen.Annotation
import GHC.Tc.Gen.Bind
import GHC.Tc.Gen.Default
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.Sig( isCompleteHsSig, tcRules )
import {-# SOURCE #-} GHC.HsToCore.Binds (dsTopLHsBinds)
import GHC.HsToCore.Monad (initDsTc)
import GHC.Core (CoreExpr)
import qualified GHC.Core as Core
import GHC.Core.Utils (exprType)
import GHC.Data.OrdList (fromOL)
import GHC.Tc.Gen.SourceTable (sourceTableTyCons, sourceCaseTables, sourceMapperType, sourceMapperTable, sourceCharacterTables, sourceRetainedWitnessTables, sourceLocalBooleanCases, checkLocalRetainedWitnessExports, isMonomorphicRetainedWitness,
                               isStructuralSourceTable, isNominalStructuralSource, sourceRuleOwners, sourceRuleOwnersAt, visibleSourceMethodRules,
                               hasSourceMethodRule, hasSourceMethodRuleAt, sourceLiteralWorkers, checkSourceMethodUses,
                               checkOptionalSourceUses, checkAndInstallSourceProofs,
                               sourceMapSelector, sourceTableDependencies)
import GHC.Tc.Gen.Foreign
import GHC.Tc.Deriv (DerivInfo)
import GHC.Tc.TyCl.Instance
import GHC.Tc.TyCl.Utils ( addTyConsToGblEnv )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Solver
import GHC.Tc.TyCl
import GHC.Tc.Instance.Typeable ( mkTypeableBinds )
import GHC.Tc.Utils.Backpack
import GHC.Tc.Zonk.TcType

import GHC.Rename.Bind ( rejectBootDecls )
import GHC.Rename.Splice ( rnTopSpliceDecls, traceSplice, SpliceInfo(..) )
import GHC.Rename.HsType
import GHC.Rename.Expr
import GHC.Rename.Fixity ( lookupFixityRn )
import GHC.Rename.Names
import GHC.Rename.Env
import GHC.Rename.Module
import GHC.Rename.Doc
import GHC.Rename.Utils ( mkNameClashErr, mkRnSyntaxExpr )

import GHC.Iface.Decl    ( coAxiomToIfaceDecl )
import GHC.Iface.Env     ( externaliseName )
import GHC.Iface.Load

import GHC.Builtin.WiredIn.Types ( mkListTy, anyTypeOfKind, manyDataConTy, matchableDataConTy, unmatchableDataConTy )
import GHC.Builtin.Modules( mAIN_NAME, gHC_PRIM, rOOT_MAIN, isWiredInOnlyModule )
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs
import GHC.Builtin

import GHC.Hs hiding ( FunDep(..) )
import GHC.Hs.Dump

import GHC.Core.PatSyn
import GHC.Core.Predicate ( classMethodTy )
import GHC.Core.InstEnv
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.ConLike (ConLike(RealDataCon))
import GHC.Core.OpenWhnf (openDataConFieldTypes)
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.Coercion (coercionLKind, coercionRKind)
import GHC.Core.Coercion.Axiom
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.TyCo.Ppr( debugPprType )
import GHC.Core.TyCo.Tidy( tidyTopType )
import GHC.Core.FamInstEnv
   ( FamInst, pprFamInst, famInstsRepTyCons, orphNamesOfFamInst
   , famInstEnvElts, extendFamInstEnvList, emptyFamInstEnvs, normaliseType )

import GHC.Parser.Header       ( mkImplicitImports, mkUnresolvedImport )

import GHC.IfaceToCore

import GHC.Runtime.Context

import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Types.Error
import GHC.Types.Name.Reader
import GHC.Types.DefaultEnv
import GHC.Types.Fixity.Env
import GHC.Types.Id as Id
import GHC.Types.Id.Info( IdDetails(..) )
import GHC.Types.Var (VarBndr(Bndr), isCoVar, setIdDetails)
import GHC.Types.Var.Env
import GHC.Types.Unique.FM (isNullUFM)
import GHC.Types.TypeEnv
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Avail
import GHC.Types.Basic hiding( SuccessFlag(..) )
import GHC.Types.UnresolvedImport
import GHC.Types.Annotations
import GHC.Types.SrcLoc
import GHC.Types.SourceFile
import qualified GHC.LanguageExtensions as LangExt

import GHC.Unit.Env as UnitEnv
import GHC.Unit.External
import GHC.Unit.Types
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Module
import Language.Haskell.Syntax.Module.Name (HsModuleName)
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.Deps
import GHC.Driver.Downsweep

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.List.SetOps
import GHC.Data.Bag
import qualified GHC.Data.BooleanFormula as BF

import Control.Arrow ( second )
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Data.Data (Data, cast, gmapQ)
import Data.Function (on)
import Data.Functor.Classes ( liftEq )
import Data.List (find, partition, sort, sortBy)
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Ord
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable ( for_ )
import Data.Traversable ( for )
import Data.IORef( newIORef )

{-
************************************************************************
*                                                                      *
        Typecheck and rename a module
*                                                                      *
************************************************************************
-}

-- | Options for typechecking a module
data TcRnModuleOptions
  = TcRnModuleOptions
  { tcRnModuleKeepRenamedSyntax :: Bool
     -- ^ keep renamed syntax?
  , tcRnModuleTcMPluginHandling :: TcMPluginHandling
     -- ^ how to handle 'TcM' monad plugins; see 'TcMPluginHandling'
     --
     -- If you want to proceed to desugaring, pass 'StartAndKeepRunningTcMPlugins'
     -- to keep the plugins running for pattern-match checking. The desugarer
     -- will shut the plugins down.
     --
     -- If you do not want to proceed to desugaring, use 'StartAndStopTcMPlugins'.
  }

-- | Top level entry point for renaming and typechecking a single module.
tcRnModule :: HscEnv
           -> ModSummary
           -> TcRnModuleOptions
           -> HsParsedModule
           -> IO (Messages TcRnMessage, Maybe TcGblEnv)
tcRnModule hsc_env mod_sum
   TcRnModuleOptions
     { tcRnModuleKeepRenamedSyntax = save_rn_syntax
     , tcRnModuleTcMPluginHandling = tcm_plugin_handling }
   parsedModule@HsParsedModule {hpm_module= L loc this_module}
 | RealSrcSpan real_loc _ <- loc
 = withTiming logger
              (text "Renamer/typechecker"<+>brackets (ppr this_mod))
              (const ()) $
   initTc tcm_plugin_handling hsc_env hsc_src save_rn_syntax this_mod real_loc $
          tcRnModuleTcRnM hsc_env mod_sum parsedModule this_mod

  | otherwise
  = return (err_msg `addMessage` emptyMessages, Nothing)

  where
    hsc_src = ms_hsc_src mod_sum
    logger  = hsc_logger hsc_env
    home_unit = hsc_home_unit hsc_env
    err_msg = mkPlainErrorMsgEnvelope loc $
              TcRnModMissingRealSrcSpan this_mod

    this_mod
      | Just (L _ mod) <- hsmodName this_module
      = mkHomeModule home_unit (hsModuleName mod)

      | otherwise   -- 'module M where' is omitted
      = mkHomeModule home_unit mAIN_NAME




tcRnModuleTcRnM :: HscEnv
                -> ModSummary
                -> HsParsedModule
                -> Module
                -> TcRn TcGblEnv
-- Factored out separately from tcRnModule so that a Core plugin can
-- call the type checker directly
tcRnModuleTcRnM hsc_env mod_sum
                (HsParsedModule {
                   hpm_module =
                      (L loc (HsModule (XModulePs _ _ mod_deprec maybe_doc_hdr)
                                       maybe_mod export_ies import_decls local_decls)),
                   hpm_src_files = src_files
                })
                this_mod
 = setSrcSpan loc $
   do { let { explicit_mod_hdr = isJust maybe_mod
            ; hsc_src          = ms_hsc_src mod_sum }
      ; -- Load the hi-boot interface for this module, if any
        -- We do this now so that the boot_names can be passed
        -- to tcTyAndClassDecls, because the boot_names are
        -- automatically considered to be loop breakers
        tcg_env <- getGblEnv
      ; boot_info <- tcHiBootIface hsc_src this_mod
      ; setGblEnv (tcg_env { tcg_self_boot = boot_info })
        $ do
        { -- Deal with imports; first add all the implicit imports.
          dflags <- getDynFlags
        ; let
            implicit_imports = mkImplicitImports dflags (moduleName this_mod) import_decls
            is_prelude_import :: LImportDecl GhcPs -> Bool
            is_prelude_import (L _ (ImportDecl { ideclExt = ext })) =
              ideclOrigin ext == ImplicitPreludeImport

        ; when (any is_prelude_import implicit_imports) $ do
            addDiagnostic TcRnImplicitImportOfPrelude

        ; raw_sig_imports <- liftIO
                             $ findExtraSigImports hsc_env hsc_src
                                 (moduleName this_mod)
        ; let { unresolved_imports =
                  map (rnUnresolvedImportPkgQual (renameRawPkgQual (hsc_unit_env hsc_env))
                         . unLoc . mkUnresolvedImport . unLoc)
                      (implicit_imports ++ import_decls) }
        ; raw_req_imports <- liftIO
                             $ implicitRequirements hsc_env unresolved_imports
        ; let { mkImport mod_name = noLocA
                $ (simpleImportDecl mod_name)
                  { ideclImportList = Just (Exactly, [])}}
        ; let { withReason t imps = map (,text t) imps }
        ; let { all_imports = withReason "is implicitly imported" implicit_imports
                  ++ withReason "is directly imported" import_decls
                  ++ withReason "is an extra sig import" (map mkImport raw_sig_imports)
                  ++ withReason "is an implicit req import" (map mkImport raw_req_imports) }
        ; -- OK now finally rename the imports
          (defaultImportsByClass, tcg_env) <-
            {-# SCC "tcRnImports" #-} tcRnImports hsc_env all_imports

        -- Put a version of the header without identifier info into the tcg_env
        -- Make sure to do this before 'tcRnSrcDecls', because we need the
        -- module header when we're splicing TH, since it can be accessed via
        -- 'getDoc'.
        -- We will rename it properly after renaming everything else so that
        -- haddock can link the identifiers
        ; let noHdrIds (L loc (WithHsDocIdentifiers str _)) =
                L loc (WithHsDocIdentifiers (rnHsDocString str) [])
        ; tcg_env <- return (tcg_env
                              { tcg_hdr_info = (noHdrIds <$> maybe_doc_hdr, maybe_mod) })
        ; -- If the whole module is warned about or deprecated
          -- (via mod_deprec) record that in tcg_warns. If we do thereby add
          -- a WarnAll, it will override any subsequent deprecations added to tcg_warns
        ; tcg_env1 <- case mod_deprec of
                             Just (L _ txt) -> do { txt' <- rnWarningTxt txt
                                                  ; pure $ tcg_env {tcg_warns = WarnAll txt'}
                                                  }
                             Nothing            -> pure tcg_env
        ; setGblEnv tcg_env1
          $ do { -- Rename and type check the declarations
                 traceRn "rn1a" empty
               ; tcg_env <-
                   case hsc_src of
                    -- See Note [Don't typecheck GHC.Internal.Prim]
                    _ | tcg_mod tcg_env1 == gHC_PRIM -> pure tcg_env1

                    HsBootOrSig boot_or_sig ->
                      do { tcg_env <- tcRnHsBootDecls boot_or_sig local_decls
                         ; traceRn "rn4a: before exports" empty
                         ; tcg_env <- setGblEnv tcg_env $
                                      rnExports explicit_mod_hdr export_ies
                         ; traceRn "rn4b: after exports" empty
                         ; return tcg_env
                         }
                    HsSrcFile ->
                      {-# SCC "tcRnSrcDecls" #-}
                        tcRnSrcDecls explicit_mod_hdr export_ies local_decls

               ; whenM (goptM Opt_DoCoreLinting) $
                 lintGblEnv (hsc_logger hsc_env) (hsc_dflags hsc_env) tcg_env

               -- Sync the knot-tied type environment before checking
               -- the M.hi-boot interface, if any
               ; syncTypeEnvKnotVars tcg_env

               ; setGblEnv tcg_env
                 $ do { -- Compare hi-boot iface (if any) with the real thing
                        -- Must be done after processing the exports
                        tcg_env <- checkHiBootIface tcg_env boot_info
                      ; checkWiredInOnlyModule tcg_env
                      ; -- The new type env is already available to stuff
                        -- slurped from interface files, via syncTypeEnvKnotVars,
                        -- itself called by tcRnSrcDecls. It's important that this
                        -- includes the stuff in checkHiBootIface,
                        -- because the latter might add new bindings for
                        -- boot_dfuns, which may be mentioned in imported
                        -- unfoldings.
                      ; -- Report unused names
                        -- Do this /after/ type inference, so that when reporting
                        -- a function with no type signature we can give the
                        -- inferred type
                      ; reportUnusedNames tcg_env hsc_src
                      ; reportClashingDefaultImports defaultImportsByClass (tcg_default tcg_env)

                      -- Rename the module header properly after we have renamed everything else
                      ; maybe_doc_hdr <- traverse rnLHsDoc maybe_doc_hdr;
                      ; tcg_env <- return (tcg_env
                                            { tcg_hdr_info = (maybe_doc_hdr, maybe_mod) })

                      ; -- add extra source files to tcg_dependent_files
                        addDependentFiles src_files
                        -- Ensure plugins run with the same tcg_env that we pass in
                      ; setGblEnv tcg_env
                        $ do { tcg_env <- runTypecheckerPlugin mod_sum tcg_env
                             ; -- Dump output and return
                               tcDump tcg_env
                             ; return tcg_env
                             }
                      }
               }
        }
      }

{- Note [Don't typecheck GHC.Internal.Prim]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC (currently) can't type-check GHC.Internal.Prim, as that module
contains primitive functions that can't be defined in source Haskell.
The GHC.Internal.Prim source file is only used to generate Haddock documentation;
the actual contents of the module are wired in to GHC.
-}

{- Note [Disambiguation of multiple default declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See Note [Named default declarations] in GHC.Tc.Gen.Default

Only a single default declaration can be in effect in any single module for
any particular class.

* Two declarations for the same class explicitly declared in the same module
  are considered a static error.

* Definition: given two default declarations for the same class

  default C (Type_1a , … , Type_ma)
  default C (Type_1b , … , Type_nb)

  if the first type sequence Type_1a , … , Type_ma is a sub-sequence of the
  second sequence Type_1b , … , Type_nb (i.e., the former can be obtained by
  removing a number of Type_ib items from the latter), we say that the second
  declaration *subsumes* the first one.

* A default declaration in a module takes precedence over any imported default
  declarations for the same class. However the compiler warns the user if an
  imported declaration is not subsumed by the local declaration.

* For any two imported default declarations for the same class where one
  subsumes the other, we ignore the subsumed declaration.

* If a class has neither a local default declaration nor an imported default
  declaration that subsumes all other imported default declarations for the
  class, the conflict between the imports is unresolvable. The effect is to
  ignore all default declarations for the class, so that no declaration is in
  effect in the module. The compiler emits a warning in this case, but no
  error.
-}

-- See Note [Disambiguation of multiple default declarations]
-- | Warn about any imported default declaration that is not subsumed by either
-- a local or an imported default declaration.
reportClashingDefaultImports :: [NonEmpty ClassDefaults] -> DefaultEnv -> TcM ()
reportClashingDefaultImports importsByClass local = mapM_ check importsByClass
  where
    check cds@(ClassDefaults{cd_class = cls} :| _) = do
      let cdLocal  = lookupDefaultEnv local (className cls)
      case cdLocal of
        Just ClassDefaults{cd_types = localTypes}
          | all ((`isTypeSubsequenceOf` localTypes) . cd_types) cds -> pure ()
        Nothing
          | not (isEmptyDefaultEnv $ subsume cds) -> pure ()
        _ -> do
          warn_default <- woptM Opt_WarnTypeDefaults
          diagnosticTc warn_default $
            TcRnWarnClashingDefaultImports cls (cd_types <$> cdLocal) cds

-- | Collapse a non-empty list of @default@ declarations for the same class to
-- the single declaration among them that subsumes all others, or to no
-- declaration otherwise.
subsume :: NonEmpty ClassDefaults -> DefaultEnv
subsume (deft :| []) = unitDefaultEnv deft
subsume (deft :| deft' : defts)
  | cd_types deft  `isTypeSubsequenceOf` cd_types deft' = subsume (deft' :| defts)
  | cd_types deft' `isTypeSubsequenceOf` cd_types deft  = subsume (deft  :| defts)
  | otherwise = emptyDefaultEnv

isTypeSubsequenceOf :: [Type] -> [Type] -> Bool
isTypeSubsequenceOf [] _ = True
isTypeSubsequenceOf _ [] = False
isTypeSubsequenceOf (t1:t1s) (t2:t2s)
  | tcEqType t1 t2 = isTypeSubsequenceOf t1s t2s
  | otherwise = isTypeSubsequenceOf (t1:t1s) t2s

{-
************************************************************************
*                                                                      *
                Import declarations
*                                                                      *
************************************************************************
-}

tcRnImports :: HscEnv -> [(LImportDecl GhcPs, SDoc)] -> TcM ([NonEmpty ClassDefaults], TcGblEnv)
tcRnImports hsc_env import_decls
  = do  { (rn_imports, imp_user_spec, rdr_env, imports) <- rnImports import_decls
        -- Get the default declarations for the classes imported by this module
        -- and group them by class.
        ; tc_defaults <-(NE.groupBy ((==) `on` cd_class) . (concatMap defaultList))
                        <$> tcGetClsDefaults (M.keys $ imp_mods imports)
        ; this_mod <- getModule
        ; gbl_env <- getGblEnv
        ; let unitId = homeUnitId $ hsc_home_unit hsc_env
              mnwib = GWIB (moduleName this_mod)(hscSourceToIsBoot (tcg_src gbl_env))
          -- We want instance declarations from all home-package
          -- modules below this one, including boot modules, except
          -- ourselves.  The 'except ourselves' is so that we don't
          -- get the instances from this module's hs-boot file.  This
          -- filtering also ensures that we don't see instances from
          -- modules batch (@--make@) compiled before this one, but
          -- which are not below this one.
        ; (home_insts, home_fam_insts) <- liftIO $
              hugInstancesBelow hsc_env unitId mnwib

          -- We use 'unsafeInterleaveIO' to avoid redundant memory allocations
          -- See Note [Lazily loading COMPLETE pragmas] from GHC.HsToCore.Monad
          -- and see https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14274#note_620545
        ; completeSigsBelow <- liftIO $ unsafeInterleaveIO $
              hugCompleteSigsBelow hsc_env unitId mnwib

          -- Record boot-file info in the EPS, so that it's
          -- visible to loadHiBootInterface in tcRnSrcDecls,
          -- and any other incrementally-performed imports
        ; when (isOneShot (ghcMode (hsc_dflags hsc_env))) $ do {
            updateEps_ $ \eps  -> eps { eps_is_boot = imp_boot_mods imports }
         }

                -- Update the gbl env
        ; updGblEnv ( \ gbl ->
            gbl {
              tcg_rdr_env      = tcg_rdr_env gbl `plusGlobalRdrEnv` rdr_env,
              tcg_imports      = tcg_imports gbl `plusImportAvails` imports,
              tcg_complete_match_env = tcg_complete_match_env gbl ++
                                       completeSigsBelow,
              tcg_import_decls = imp_user_spec,
              tcg_rn_imports   = rn_imports,
              tcg_default      = foldMap subsume tc_defaults,
              tcg_inst_env     = tcg_inst_env gbl `unionInstEnv` home_insts,
              tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env gbl)
                                                      home_fam_insts
            }) $ do {

        ; traceRn "rn1" (ppr (imp_direct_dep_mods imports))
                -- Fail if there are any errors so far
                -- The error printing (if needed) takes advantage
                -- of the tcg_env we have now set
--      ; traceIf (text "rdr_env: " <+> ppr rdr_env)
        ; failIfErrsM

                -- Load any orphan-module (including orphan family
                -- instance-module) interfaces, so that their rules and
                -- instance decls will be found.  But filter out a
                -- self hs-boot: these instances will be checked when
                -- we define them locally.
                -- (We don't need to load non-orphan family instance
                -- modules until we either try to use the instances they
                -- define, or define our own family instances, at which
                -- point we need to check them for consistency.)
        ; loadModuleInterfaces (text "Loading orphan modules")
                               (filter (/= this_mod) (imp_orphs imports))

                -- Check type-family consistency between imports.
                -- See Note [The type family instance consistency story]
        ; traceRn "rn1: checking family instance consistency {" empty
        ; let { dir_imp_mods = M.keys
                             . imp_mods
                             $ imports }
        ; logger <- getLogger
        ; withTiming logger (text "ConsistencyCheck"<+>brackets (ppr this_mod)) (const ())
            $ checkFamInstConsistency dir_imp_mods
        ; traceRn "rn1: } checking family instance consistency" empty

        ; gbl_env <- getGblEnv
        ; return (tc_defaults, gbl_env) } }

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module
*                                                                      *
************************************************************************
-}

tcRnSrcDecls :: Bool  -- False => no 'module M(..) where' header at all
             -> Maybe [LIE GhcPs]
             -> [LHsDecl GhcPs]               -- Declarations
             -> TcM TcGblEnv
tcRnSrcDecls explicit_mod_hdr export_ies decls
 = do { -- Do all the declarations
      ; (tcg_env, tcl_env, lie) <- tc_rn_src_decls decls
      ; traceTc "tcRnSrcDecls" (ppr (tcg_type_env tcg_env))

      ------ Simplify constraints ---------
      --
      -- We do this after checkMainType, so that we use the type
      -- info that checkMainType adds
      --
      -- We do it with both global and local env in scope:
      --  * the global env exposes the instances to simplifyTop,
      --    and affects how names are rendered in error messages
      --  * the local env exposes the local Ids to simplifyTop,
      --    so that we get better error messages (monomorphism restriction)
      ; tcg_env <- {-# SCC "simplifyTop" #-}
                   restoreEnvs (tcg_env, tcl_env) $
                   do { lie_main <- checkMainType tcg_env
                      ; ev_binds <- simplifyTop (lie `andWC` lie_main)
                      ; return (tcg_env `addEvBinds` ev_binds) }

        -- Emit Typeable bindings
      ; tcg_env <- restoreEnvs (tcg_env, tcl_env) $
                   mkTypeableBinds

      ; traceTc "Tc9" empty
      ; failIfErrsM    -- Stop now if if there have been errors
                       -- Continuing is a waste of time; and we may get debug
                       -- warnings when zonking about strangely-typed TyCons!

        -- Zonk the final code.  This must be done last.
        -- Even simplifyTop may do some unification.
        -- This pass also warns about missing type signatures
      ; (id_env, ev_binds', binds', fords', imp_specs', rules', pat_syns')
            <- zonkTcGblEnv tcg_env
      ; traceTc "Tc10" empty

      --------- Run finalizers --------------
      -- Finalizers must run after constraints are simplified, lest types
      --    might not be complete when using reify (see #12777).
      -- and also after we zonk the first time because we run typed splices
      --    in the zonker which gives rise to the finalisers.
      ; let -- init_tcg_env:
            --   * Remove accumulated bindings, rules and so on from
            --     TcGblEnv.  They are now in ev_binds', binds', etc.
            --   * Add the zonked Ids from the value bindings to tcg_type_env
            --     Up to now these Ids are only in tcl_env's type-envt
            init_tcg_env = tcg_env { tcg_binds     = []
                                   , tcg_ev_binds  = emptyBag
                                   , tcg_imp_specs = []
                                   , tcg_rules     = []
                                   , tcg_fords     = []
                                   , tcg_patsyns   = []
                                   , tcg_type_env  = mergeZonkedTypes (tcg_type_env tcg_env) id_env }
      ; (tcg_env, tcl_env) <- setGblEnv init_tcg_env
                              run_th_modfinalizers
      ; finishTH
      ; traceTc "Tc11" empty

      --------- Deal with the exports ----------
      -- Can't be done earlier, because the export list must "see"
      -- the declarations created by the finalizers
      ; tcg_env <- restoreEnvs (tcg_env, tcl_env) $
                   rnExports explicit_mod_hdr export_ies

      --------- Emit the ':Main.main = runMainIO main' declaration ----------
      -- Do this /after/ rnExports, so that it can consult
      -- the tcg_exports created by rnExports
      ; tcg_env
           <- restoreEnvs (tcg_env, tcl_env) $
              do { (tcg_env, lie) <- captureTopConstraints $
                                     checkMain explicit_mod_hdr export_ies
                 ; ev_binds <- simplifyTop lie
                 ; return (tcg_env `addEvBinds` ev_binds) }

      ; failIfErrsM    -- Stop now if if there have been errors
                       -- Continuing is a waste of time; and we may get debug
                       -- warnings when zonking about strangely-typed TyCons!

      ---------- Final zonking ---------------
      -- Zonk the new bindings arising from running the finalisers,
      -- and main. This won't give rise to any more finalisers as you
      -- can't nest finalisers inside finalisers.
      ; (id_env_mf, ev_binds_mf, binds_mf, fords_mf, imp_specs_mf, rules_mf, patsyns_mf)
            <- zonkTcGblEnv tcg_env

      ; let { !final_type_env = mergeZonkedTypes (tcg_type_env tcg_env) id_env_mf
              -- Add the zonked Ids from the value bindings (they were in tcl_env)
              -- Keep the origin of tables used before final zonking. The same
              -- family can already occur in a local signature. A provisional
              -- final-Core check must not replace or remove it.
              -- Force !final_type_env, lest we retain an old reference
              -- to the previous tcg_env

            ; tcg_env' = tcg_env
                          { tcg_type_env  = final_type_env
                          , tcg_binds     = binds'     ++ binds_mf
                          , tcg_ev_binds  = ev_binds' `unionBags` ev_binds_mf
                          , tcg_imp_specs = imp_specs' ++ imp_specs_mf
                          , tcg_rules     = rules'     ++ rules_mf
                          , tcg_fords     = fords'     ++ fords_mf
                          , tcg_patsyns   = pat_syns'  ++ patsyns_mf } } ;

        -- The interface also carries the equations of eligible source functions.
        -- See Note [Closed source tables] in GHC.Tc.Gen.SourceTable.
      ; tcg_env' <- setGblEnv tcg_env' $ do
          checkSourceMethodUses tcg_env'
          checkAndInstallSourceProofs tcg_env'
      ; failIfErrsM
      ; tables_enabled <- xoptM LangExt.ExperimentalDependentTypes
      ; write_equations <- goptM Opt_WriteSourceEquations
      ; setGblEnv tcg_env' $ do
          let binds = tcg_binds tcg_env'
          tables <- if tables_enabled || write_equations
                    then sourceTableTyCons False binds else pure []
          (cases, fields) <- sourceCaseTables binds
          -- Keep the existing ordinary certificates when both checkers
          -- accept the same owner. Enumeration cases retain executable origin.
          let case_owners = mkNameSet (map (idName . fst) cases)
              new_tables
                | tables_enabled = tables
                | otherwise = filter (\(owner, _) -> not (idName owner `elemNameSet` case_owners)) tables
          with_fields <- if null fields then getGblEnv else fst <$> addTyConsToGblEnv fields
          let installed = typeEnvIds (tcg_type_env with_fields)
              early_nominal = [(fun, table) | fun <- installed
                                            , SourceTableId _ table NominalSourceTable <- [idDetails fun]
                                            , isNominalStructuralSource fun || sourceMapperType (idType fun)]
          checkLocalRetainedWitnessExports with_fields installed
          failIfErrsM
          env <- setGblEnv with_fields (installSourceTables (early_nominal ++ new_tables ++ cases))
          installSourceMethods (\sel -> tables_enabled || sourceMapSelector sel) env }

-- | Publish only checked method aliases that user rules cannot replace.
-- See Note [Certifying a source method alias] in GHC.Tc.TyCl.Instance.
installSourceMethods :: (Id -> Bool) -> TcGblEnv -> TcM TcGblEnv
installSourceMethods wanted env
  | null (tcg_insts env) = pure env
  | otherwise = case checkedSourceMethods wanted env of
      [] -> pure env
      methods -> setGblEnv env $ do
        let literals = [() | (_, fields) <- methods, (_, _, SourceLiteralMethod{}) <- fields]
        -- Load the workers before collecting rules from their interfaces.
        workers <- if null literals then pure [] else sourceLiteralWorkers
        rules <- visibleSourceMethodRules env
        let local_rules = sourceRuleOwners (tcg_rules env)
            plain v = not (idName v `elemNameSet` local_rules || hasSourceMethodRule rules v)
            workers_ok = all plain workers
            plain_at (v, first) = not (idName v `elemNameSet` sourceRuleOwnersAt first (tcg_rules env)
                                       || hasSourceMethodRuleAt rules first v)
            checked = [(dfun, pairs, literal) | (dfun, fields) <- methods, plain dfun
                      , let pairs = [(sel, target) | (sel, meth, SourceMethodAlias target) <- fields
                                                 , all plain [sel, meth, target]]
                            literal = listToMaybe [(sel, z, s) | (sel, meth, SourceLiteralMethod z s ops) <- fields
                                      , workers_ok, plain meth, plain_at (sel, Just (tyConName (dataConTyCon z))), all plain_at ops]
                      , not (null pairs) || isJust literal]
        for_ checked $ \(_, pairs, _) -> for_ pairs $ \(_, target) ->
          when (nameIsLocalOrFrom (tcg_semantic_mod env) (idName target)) $
            keepAlive (idName target)
        tcExtendGlobalValEnv [setSourceLiteralCons (setSourceMethodIds dfun pairs) literal
                             | (dfun, pairs, literal) <- checked] getGblEnv

{- Note [Source table dependency closure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source equation can call another certified function. Its family RHS keeps
that function's family identity. Recover the transitive term owners before
checking rules. A rule on any owner invalidates the caller's certificate.
Keep each local callee alive so its Id remains available to importers.
An unused runtime function needs no certificate and produces no rule error.
-}

-- | Add the families and their axioms to the module interface. A local use
-- may already have required the family before we finished checking the module.
-- See Note [Source table dependency closure].
installSourceTables :: [(Id, TyCon)] -> TcM TcGblEnv
installSourceTables [] = getGblEnv
installSourceTables tables = do
    env <- getGblEnv
    let types = tcg_type_env env
        new_tables = [tc | (_, tc) <- tables, isNothing (lookupTypeEnv types (tyConName tc))]
        prepare (fun, candidate) =
          let table = case lookupTypeEnv types (tyConName candidate) of
                        Just (ATyCon installed) -> installed
                        _ -> candidate
              owner = case lookupTypeEnv types (idName fun) of
                        Just (AnId existing) -> existing
                        _ -> fun
              ordinary = case idDetails owner of
                           SourceTableId {} -> setIdDetails owner VanillaId
                           _ -> owner
              origin = case idDetails owner of
                         SourceTableId _ _ prior -> prior
                         _ -> case idDetails fun of
                                SourceTableId _ _ proposed -> proposed
                                _ -> NominalSourceTable
          in (ordinary, table, origin)
        prepared = map prepare tables
        candidates = [setIdDetails fun (SourceTableId (idName fun) table origin)
                     | (fun, table, origin) <- prepared]
    -- Make all checked owners available while recovering the dependency closure.
    tcExtendGlobalValEnv candidates $ do
      dependencies <- mapM (\(_, table, _) -> sourceTableDependencies table) prepared
      -- Dependency lookup can load interfaces that contain rules.
      rewriting <- goptM Opt_EnableRewriteRules
      visible <- if rewriting then Just <$> visibleSourceMethodRules env else pure Nothing
      used <- readTcRef (tcg_source_table_uses env)
      let ruled = sourceRuleOwners (tcg_rules env)
          changed fun = idName fun `elemNameSet` ruled
                        || maybe False (`hasSourceMethodRule` fun) visible
      ids <- forM (zip prepared dependencies) $ \((fun, table, origin), callees) -> do
        let name = idName fun
            reject diagnostic = do
              when (tyConName table `elemNameSet` used) $
                setSrcSpan (nameSrcSpan name) (addErr diagnostic)
              pure fun
        case callees of
          Nothing -> reject (TcRnNoSourceTable name)
          Just deps | Just bad <- find changed (case origin of
                        FinalCoreSourceTable -> deps
                        NominalSourceTable | not rewriting, isStructuralSourceTable table -> deps
                        _ -> fun : deps)
            -> reject (TcRnRetainedQuantificationError (RetainedSourceRule (idName bad)))
          Just deps -> do
            for_ deps $ \callee ->
              when (nameIsLocalOrFrom (tcg_semantic_mod env) (idName callee)) $
                keepAlive (idName callee)
            pure $ case idDetails fun of
              VanillaId -> setIdDetails fun (SourceTableId name table origin)
              _ -> fun
      tcExtendGlobalValEnv ids $
        if null new_tables then getGblEnv else fst <$> addTyConsToGblEnv new_tables

-- | Check only the bindings in the declaration group that is now in scope.
sourceTablesFor :: Bool -> NameSet -> TcGblEnv -> TcM [(Id, TyCon)]
sourceTablesFor structural names env = sourceTableTyCons structural (filter requested (tcg_binds env))
  where
    requested bind = any ((`elemNameSet` names) . idName)
                         (collectHsBindBinders CollNoDictBinders (unLoc bind))

{- Note [Early constructor mapper equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A dependent type can need the equations of a local mapper. Schedule
the signed worker in the ordinary value dependency order. After the
group's constraints are solved, check its desugared body with the
existing constructor-case checker. Keep the actual callback binder and
the checked identities of constructor fields and recursive calls.

An empty declaration splice also makes a solved worker available to
later declarations. The local observer checks for pending typed
splices before zonking. The mapper pass checks only the current group's
mapping signature. It adds no table when the checked body fails.

The table has ordinary nominal source authority. Its uses record the
same rule dependencies as a table published at module finish. A later
rule can invalidate an unused early table as well. Pass early mapper
owners back to installSourceTables before writing the interface, so
the final rule environment can withdraw that authority.
-}

-- | Check the requested constructor mappers after solving their value group.
-- See Note [Early constructor mapper equations].
sourceTablesWithMappers :: Bool -> NameSet -> Bag EvBind -> TcGblEnv -> TcM [(Id, TyCon)]
sourceTablesWithMappers structural names evidence env = do
    ordinary <- sourceTablesFor structural names env
    let in_group = [owner | bind <- tcg_binds env
                          , owner <- collectHsBindBinders CollNoDictBinders (unLoc bind)
                          , idName owner `elemNameSet` names]
    candidates <- fmap catMaybes $ forM in_group $ \owner -> do
      ty <- liftZonkM (zonkTcType (idType owner))
      pure (owner <$ guard (sourceMapperType ty))
    mappers <- forM candidates $ \owner ->
      tryTcDiscardingErrs (pure Nothing) $ do
        let name = idName owner
        (definitions, _) <- localOpenDefinitions [(name, nameSrcSpan name)]
                              (tcg_ev_binds env `unionBags` evidence) (tcg_binds env)
        case lookupNameEnv definitions name of
          Just (actual, rhs) | actual == owner -> sourceMapperTable actual rhs
          _ -> pure Nothing
    pure (ordinary ++ catMaybes mappers)

-- | Merge the finalised types and retain a source table already attached to
-- its exact owner. A local signature can already use the table's equations.
mergeZonkedTypes :: TypeEnv -> TypeEnv -> TypeEnv
mergeZonkedTypes original zonked = original `plusTypeEnv` mapNameEnv restore zonked
  where
    restore (AnId fun) = AnId (restoreSourceTableId original fun)
    restore thing = thing

zonkTcGblEnv :: TcGblEnv
             -> TcM (TypeEnv, Bag EvBind, LHsBinds GhcTc,
                       [LForeignDecl GhcTc], [LTcSpecPrag], [LRuleDecl GhcTc], [PatSyn])
zonkTcGblEnv tcg_env@(TcGblEnv { tcg_binds     = binds
                               , tcg_ev_binds  = ev_binds
                               , tcg_imp_specs = imp_specs
                               , tcg_rules     = rules
                               , tcg_fords     = fords
                               , tcg_patsyns   = pat_syns })
  = {-# SCC "zonkTopDecls" #-}
    setGblEnv tcg_env $ -- This sets the GlobalRdrEnv which is used when rendering
                        --   error messages during zonking (notably levity errors)
    zonkTopDecls ev_binds binds rules imp_specs fords pat_syns

-- | Runs TH finalizers and renames and typechecks the top-level declarations
-- that they could introduce.
run_th_modfinalizers :: TcM (TcGblEnv, TcLclEnv)
run_th_modfinalizers = do
  th_modfinalizers_var <- fmap tcg_th_modfinalizers getGblEnv
  th_modfinalizers <- readTcRef th_modfinalizers_var
  if null th_modfinalizers
  then getEnvs
  else do
    writeTcRef th_modfinalizers_var []
    let run_finalizer (lcl_env, f) =
            restoreLclEnv lcl_env (runRemoteModFinalizers f)

    (_, lie_th) <- captureTopConstraints $
                   mapM_ run_finalizer th_modfinalizers

      -- Finalizers can add top-level declarations with addTopDecls, so
      -- we have to run tc_rn_src_decls to get them
    (tcg_env, tcl_env, lie_top_decls) <- tc_rn_src_decls []

    restoreEnvs (tcg_env, tcl_env) $ do
      -- Subsequent rounds of finalizers run after any new constraints are
      -- simplified, or some types might not be complete when using reify
      -- (see #12777).
      new_ev_binds <- {-# SCC "simplifyTop2" #-}
                      simplifyTop (lie_th `andWC` lie_top_decls)
      addTopEvBinds new_ev_binds run_th_modfinalizers
        -- addTopDecls can add declarations which add new finalizers.

tc_rn_src_decls :: [LHsDecl GhcPs]
                -> TcM (TcGblEnv, TcLclEnv, WantedConstraints)
-- Loops around dealing with each top level inter-splice group
-- in turn, until it's dealt with the entire module
-- Never emits constraints; calls captureTopConstraints internally
tc_rn_src_decls ds
 = {-# SCC "tc_rn_src_decls" #-}
   do { (first_group, group_tail) <- findSplice ds
                -- If ds is [] we get ([], Nothing)

        -- Deal with decls up to, but not including, the first splice
      ; experimental <- xoptM LangExt.ExperimentalDependentTypes
      ; local_demands <- if experimental then Just <$> newTcRef [] else pure Nothing
      ; (tcg_env, rn_decls) <- updGblEnv (\env -> env { tcg_local_open_demands = local_demands }) $
                              rnTopSrcDecls first_group
                -- rnTopSrcDecls fails if there are any errors

      ; traceRn "tc_rn_src_decls 77" empty
        -- Get TH-generated top-level declarations and make sure they don't
        -- contain any splices since we don't handle that at the moment
        --
        -- The plumbing here is a bit odd: see #10853
      ; th_topdecls_var <- fmap tcg_th_topdecls getGblEnv
      ; th_ds <- readTcRef th_topdecls_var
      ; writeTcRef th_topdecls_var []

      ; (tcg_env, rn_decls) <-
            if null th_ds
            then return (tcg_env, rn_decls)
            else do { (th_group, th_group_tail) <- findSplice th_ds
                    ; case th_group_tail of
                        { Nothing -> return ()
                        ; Just (SpliceDecl _ (L loc _) _, _) ->
                            setSrcSpanA loc $ addErr $
                            TcRnTHError $ AddTopDeclsError
                              AddTopDeclsUnexpectedDeclarationSplice
                        }
                      -- Rename TH-generated top-level declarations
                    ; (tcg_env, th_rn_decls) <- setGblEnv tcg_env
                       $ rnTopSrcDecls th_group

                      -- Dump generated top-level declarations
                    ; let msg = "top-level declarations added with 'addTopDecls'"
                    ; traceSplice
                        $ SpliceInfo { spliceDescription = msg
                                     , spliceIsDecl    = True
                                     , spliceSource    = Nothing
                                     , spliceGenerated = ppr th_rn_decls }
                    ; return (tcg_env, appendGroups rn_decls th_rn_decls)
                    }

      -- Type check all declarations
      -- NB: set the env **before** captureTopConstraints so that error messages
      -- get reported w.r.t. the right GlobalRdrEnv. It is for this reason that
      -- the captureTopConstraints must go here, not in tcRnSrcDecls.
      ; traceRn "about to typechecke decls" (ppr rn_decls)
      ; demands <- maybe (pure []) (fmap reverse . readTcRef) local_demands
      ; ((tcg_env, tcl_env), lie1) <- setGblEnv (tcg_env { tcg_local_open_demands = Nothing }) $
                                      captureTopConstraints $
                                      tcTopSrcDecls demands rn_decls

        -- If there is no splice, we're nearly done
      ; restoreEnvs (tcg_env, tcl_env) $
        case group_tail of
          { Nothing -> return (tcg_env, tcl_env, lie1)

            -- If there's a splice, we must carry on
          ; Just (SpliceDecl _ (L _ splice) _, rest_ds) ->
            do {
                 -- We need to simplify any constraints from the previous declaration
                 -- group, or else we might reify metavariables, as in #16980.
               ; ev_binds1 <- simplifyTop lie1

                 -- The next group can use a table that this group defines.
               ; enabled <- xoptM LangExt.ExperimentalDependentTypes
               ; let names = mkNameSet (collectHsValBinders CollNoDictBinders (hs_valds rn_decls))
               ; tcg_env <- if enabled
                            then sourceTablesWithMappers False names ev_binds1 tcg_env >>= installSourceTables
                            else return tcg_env

                 -- Rename the splice expression, and get its supporting decls
               ; (spliced_decls, splice_fvs) <- setGblEnv tcg_env (rnTopSpliceDecls splice)

                 -- Glue them on the front of the remaining decls and loop
               ; setGblEnv (tcg_env `addTcgDUs` usesOnly splice_fvs) $
                 addTopEvBinds ev_binds1                             $
                 tc_rn_src_decls (spliced_decls ++ rest_ds)
               }
          }
      }

{-
************************************************************************
*                                                                      *
        Compiling hs-boot source files, and
        comparing the hi-boot interface with the real thing
*                                                                      *
************************************************************************
-}

tcRnHsBootDecls :: HsBootOrSig -> [LHsDecl GhcPs] -> TcM TcGblEnv
tcRnHsBootDecls boot_or_sig decls
   = do { (first_group, group_tail) <- findSplice decls

                -- Rename the declarations
        ; (tcg_env, HsGroup { hs_tyclds = tycl_decls
                            , hs_derivds = deriv_decls
                            , hs_fords  = for_decls
                            , hs_defds  = def_decls
                            , hs_ruleds = rule_decls
                            , hs_annds  = _
                            , hs_valds  = XValBindsLR (HsVBG val_binds val_sigs) })
              <- rnTopSrcDecls first_group

        ; (gbl_env, lie) <- setGblEnv tcg_env $ captureTopConstraints $ do {
              -- NB: setGblEnv **before** captureTopConstraints so that
              -- if the latter reports errors, it knows what's in scope

                -- Check for illegal declarations
        ; case group_tail of
             Just (SpliceDecl _ d _, _) -> rejectBootDecls boot_or_sig BootSpliceDecls [d]
             Nothing                    -> return ()
        ; rejectBootDecls boot_or_sig BootForeignDecls for_decls
        ; rejectBootDecls boot_or_sig BootDefaultDecls def_decls
        ; rejectBootDecls boot_or_sig BootRuleDecls    rule_decls

                -- Typecheck type/class/instance decls
        ; traceTc "Tc2 (boot)" empty
        ; (tcg_env, inst_infos, _th_bndrs, _deriv_binds, _)
             <- tcTyClsInstDecls (Just []) tycl_decls deriv_decls def_decls val_binds
        ; setGblEnv tcg_env     $ do {

        -- Emit (signatures for) Typeable bindings
        ; tcg_env <- mkTypeableBinds
        ; setGblEnv tcg_env $ do {

                -- Typecheck value declarations
        ; traceTc "Tc5" empty
        ; val_ids <- tcHsBootSigs val_binds val_sigs

                -- Wrap up
                -- No simplification or zonking to do
        ; traceTc "Tc7a" empty
        ; gbl_env <- getGblEnv

                -- Make the final type-env
                -- Include the dfun_ids so that their type sigs
                -- are written into the interface file.
        ; let { type_env0 = tcg_type_env gbl_env
              ; type_env1 = extendTypeEnvWithIds type_env0 val_ids
              ; type_env2 = extendTypeEnvWithIds type_env1 dfun_ids
              ; dfun_ids  = map iDFunId inst_infos
              ; gbl_env'  = gbl_env { tcg_type_env = type_env2 }
              }

        ; return gbl_env'
   }}}
   ; traceTc "boot" (ppr lie); return gbl_env }

{-
Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).
-}

checkHiBootIface :: TcGblEnv -> SelfBootInfo -> TcM TcGblEnv
-- Compare the hi-boot file for this module (if there is one)
-- with the type environment we've just come up with
-- In the common case where there is no hi-boot file, the list
-- of boot_names is empty.

checkHiBootIface tcg_env boot_info
  | NoSelfBoot <- boot_info  -- Common case
  = return tcg_env

  | HsBootFile <- tcg_src tcg_env   -- Current module is already a hs-boot file!
  = return tcg_env

  | SelfBoot { sb_mds = boot_details } <- boot_info
  , TcGblEnv { tcg_binds    = binds
             , tcg_insts    = local_insts
             , tcg_type_env = local_type_env
             , tcg_exports  = local_exports } <- tcg_env
  = do  { -- This code is tricky, see Note [DFun knot-tying]
        ; imp_prs <- checkHiBootIface' local_insts local_type_env
                                       local_exports boot_details

        -- Now add the impedance-matching boot bindings:
        --
        --  - dfun bindings  $fxblah = $fblah
        --  - record bindings fld{var} = fld{rec field of ..}
        --
        -- to (a) the type envt, and (b) the top-level bindings
        ; let boot_impedance_bds = map fst imp_prs
              !type_env'         = extendTypeEnvWithIds local_type_env boot_impedance_bds
              impedance_binds    =  [ mkVarBind boot_id (nlHsVar id)
                                    | (boot_id, id) <- imp_prs ]
              tcg_env_w_binds
                = tcg_env { tcg_type_env = type_env'
                          , tcg_binds = binds ++ impedance_binds }

        ; return tcg_env_w_binds }

{- Note [DFun impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We return a list of "impedance-matching" bindings for the dfuns
defined in the hs-boot file, such as
          $fxEqT = $fEqT
We need these because the module and hi-boot file might differ in
the name it chose for the dfun: the name of a dfun is not
uniquely determined by its type; there might be multiple dfuns
which, individually, would map to the same name (in which case
we have to disambiguate them.)  There's no way for the hi file
to know exactly what disambiguation to use... without looking
at the hi-boot file itself.

In fact, the names will always differ because we always pick names
prefixed with "$fx" for boot dfuns, and "$f" for real dfuns
(so that this impedance matching is always possible).

Note [Record field impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a hs-boot file defines a function whose implementation in the hs file
is a record selector, we have to do something similar to Note [DFun impedance matching].

Example:

  -- M.hs-boot
  module M where
    data A
    fld :: A -> ()

  -- M.hs
  module M where
    data A = MkA { fld :: () }

Recall from Note [Record field namespacing] in GHC.Types.Name.Occurrence that
record fields have their own namespaces. This means that M.hs exports the Id
fld{record selector of MkA} :: A -> (), while M.hs-boot exports the Id
fld{variable} :: A -> ().

To remedy this, we add an impedance-matching binding in M.hs:

  fld{variable} :: A -> ()
  fld{variable} = fld{record selector of MkA}

Note that we imperatively need to add a binding for fld{variable} in M.hs, as we
might have an exact Name reference to it (e.g. in a module that imports M.hs-boot).
Not doing so would cause Core Lint errors, at the very least.

These bindings are returned by the check_export in checkHiBootIface', and
added to the DFun impedance-matching bindings.

[Wrinkle: exports]

  We MUST NOT add fld{variable} to the export list of M.hs, as this
  would mean that M.hs exports both a record field and variable with the same
  occNameFS, which would cause ambiguity errors at use-sites.
  It's OK to only export the field name even though the boot-file exported
  the variable: name resolution will take care of that.

Another situation is that we are re-exporting, e.g. (with M as above):

  -- N.hs-boot
  module N ( module M ) where
    import {-# SOURCE #-} M

  -- N.hs
  module N ( module M where )
    import M

In this case, N.hs-boot re-exports the variable fld, and N re-exports the
record field fld, but not the variable fld. We don't need to do anything in
this situation; in particular, don't re-export the variable name from N.hs,
as per [Wrinkle: exports] above.

Note [DFun knot-tying]
~~~~~~~~~~~~~~~~~~~~~~
The 'SelfBootInfo' that is fed into 'checkHiBootIface' comes from
typechecking the hi-boot file that we are presently implementing.
Suppose we are typechecking the module A: when we typecheck the
hi-boot file, whenever we see an identifier A.T, we knot-tie this
identifier to the *local* type environment (via if_rec_types.)  The
contract then is that we don't *look* at 'SelfBootInfo' until we've
finished typechecking the module and updated the type environment with
the new tycons and ids.

This most works well, but there is one problem: DFuns!  We do not want
to look at the mb_insts of the ModDetails in SelfBootInfo, because a
dfun in one of those ClsInsts is gotten (in GHC.IfaceToCore.tcIfaceInst) by a
(lazily evaluated) lookup in the if_rec_types.  We could extend the
type env, do a syncTypeEnvKnotVars etc; but that all seems very indirect.
It is much more directly simply to extract the DFunIds from the
md_types of the SelfBootInfo.

See #4003, #16038 for why we need to take care here.
-}

checkHiBootIface' :: [ClsInst] -> TypeEnv -> [AvailInfo]
                  -> ModDetails -> TcM [(Id, Id)]
-- Variant which doesn't require a full TcGblEnv; you could get the
-- local components from another ModDetails.
checkHiBootIface'
        local_insts local_type_env local_exports
        (ModDetails { md_types = boot_type_env
                    , md_fam_insts = boot_fam_insts
                    , md_exports = boot_exports })
  = do  { traceTc "checkHiBootIface" $ vcat
             [ ppr boot_type_env, ppr boot_exports ]

        ; mod <- tcg_mod <$> getGblEnv

        -- See Note [Don't typecheck GHC.Internal.Prim]
        ; if mod == gHC_PRIM then pure [] else do {

        ; gre_env <- getGlobalRdrEnv

                -- Check the exports of the boot module, one by one
        ; fld_prs <- mapMaybeM (check_export gre_env) boot_exports

                -- Check for no family instances
        ; unless (null boot_fam_insts) $
            panic ("GHC.Tc.Module.checkHiBootIface: Cannot handle family " ++
                   "instances in boot files yet...")
            -- FIXME: Why?  The actual comparison is not hard, but what would
            --        be the equivalent to the dfun bindings returned for class
            --        instances?  We can't easily equate tycons...

                -- Check instance declarations
                -- and generate an impedance-matching binding
        ; dfun_prs <- mapMaybeM check_cls_inst boot_dfuns

        ; failIfErrsM

        ; return (fld_prs ++ dfun_prs) }}

  where
    boot_dfun_names = map idName boot_dfuns
    boot_dfuns      = filter isDFunId $ typeEnvIds boot_type_env
       -- NB: boot_dfuns is /not/ defined thus: map instanceDFunId md_insts
       --     We don't want to look at md_insts!
       --     Why not?  See Note [DFun knot-tying]

    check_export gre_env boot_avail     -- boot_avail is exported by the boot iface
      | name `elem` boot_dfun_names
      = return Nothing

        -- Check that the actual module exports the same thing
      | missing_name:_ <- missing_names
      = -- Lookup might have failed because the hs-boot file defines a variable
        -- that is implemented in the hs file as a record selector, which
        -- lives in a different namespace.
        --
        -- See Note [Record field impedance matching].
        let missing_occ = nameOccName missing_name
            mb_ok :: GlobalRdrElt -> Maybe (GlobalRdrElt, Maybe Id)
            mb_ok gre
              -- Ensure that this GRE refers to an Id that is exported.
              | isNothing $ lookupNameEnv local_export_env (greName gre)
              = Nothing
              -- We locally define the field: create an impedance-matching
              -- binding for the variable.
              | Just (AnId id) <- lookupTypeEnv local_type_env (greName gre)
              = Just (gre, Just id)
              -- We are re-exporting the field but not the variable: not a problem,
              -- as per [Wrinkle: exports] in Note [Record field impedance matching].
              | otherwise
              = Just (gre, Nothing)
            matching_flds
              | isVarOcc missing_occ -- (This only applies to variables.)
              = lookupGRE gre_env $
                LookupOccName missing_occ (RelevantGREsFOS WantField)
              | otherwise
              = [] -- BootFldReexport T18999_NoDisambiguateRecordFields T16745A

        in case mapMaybe mb_ok $ matching_flds of

          -- At least 2 matches: report an ambiguity error.
          (gre1,_):(gre2,_):gres_ids -> do
           addErrAt (nameSrcSpan missing_name) $
             mkNameClashErr gre_env (nameRdrName missing_name)
               (gre1 NE.:| gre2 : map fst gres_ids)
           return Nothing

          -- Single match: resolve the issue.
          [(_,mb_fld_id)] ->
            -- See Note [Record field impedance matching].
            for mb_fld_id $ \ fld_id -> do
              let local_boot_var =
                    Id.mkExportedVanillaId missing_name (idType fld_id)
              return (local_boot_var, fld_id)

          -- Otherwise: report that the hs file does not export something
          -- that the hs-boot file exports.
          [] -> do
           addErrAt (nameSrcSpan missing_name)
             (missingBootThing HsBoot missing_name MissingBootExport)
           return Nothing

        -- If the boot module does not *define* the thing, we are done
        -- (it simply re-exports it, and names match, so nothing further to do)
      | isNothing mb_boot_thing
      = return Nothing

        -- Check that the actual module also defines the thing, and
        -- then compare the definitions
      | Just real_thing <- lookupTypeEnv local_type_env name,
        Just boot_thing <- mb_boot_thing
      = do checkBootDeclM HsBoot boot_thing real_thing
           return Nothing

      | otherwise
      = do addErrTc (missingBootThing HsBoot name MissingBootDefinition)
           return Nothing
      where
        name          = availName boot_avail
        mb_boot_thing = lookupTypeEnv boot_type_env name
        missing_names = case lookupNameEnv local_export_env name of
                          Nothing    -> [name]
                          Just avail -> availNames boot_avail
                            `minusList` availNames avail

    local_export_env :: NameEnv AvailInfo
    local_export_env = availsToNameEnv local_exports

    check_cls_inst :: DFunId -> TcM (Maybe (Id,Id))
        -- Returns a pair of the boot dfun in terms of the equivalent
        -- real dfun. Delicate (like checkBootDecl) because it depends
        -- on the types lining up precisely even to the ordering of
        -- the type variables in the foralls.
    check_cls_inst boot_dfun
      | (real_dfun : _) <- find_real_dfun boot_dfun
      , let dfun_name = idName boot_dfun
            local_boot_dfun = Id.mkExportedVanillaId dfun_name (idType real_dfun)
      = return $ Just (local_boot_dfun, real_dfun)
          -- Two tricky points here:
          --
          --  * The local_boot_fun should have a Name from the /boot-file/,
          --    but type from the dfun defined in /this module/.
          --    That ensures that the TyCon etc inside the type are
          --    the ones defined in this module, not the ones gotten
          --    from the hi-boot file, which may have a lot less info
          --    (#8743, comment:10).
          --
          --  * The DFunIds from boot_details are /GlobalIds/, because
          --    they come from typechecking M.hi-boot.
          --    But all bindings in this module should be for /LocalIds/,
          --    otherwise dependency analysis fails (#16038). This
          --    is another reason for using mkExportedVanillaId, rather
          --    that modifying boot_dfun, to make local_boot_fun.
          --
          -- See Note [DFun impedance matching].

      | otherwise
      = setSrcSpan (nameSrcSpan (getName boot_dfun)) $
        do { traceTc "check_cls_inst" $ vcat
                [ text "local_insts"  <+>
                     vcat (map (ppr . idType . instanceDFunId) local_insts)
                , text "boot_dfun_ty" <+> ppr (idType boot_dfun) ]

           ; addErrTc $ TcRnBootMismatch HsBoot
                      $ MissingBootInstance boot_dfun
           ; return Nothing }

    find_real_dfun :: DFunId -> [DFunId]
    find_real_dfun boot_dfun
       = [dfun | inst <- local_insts
               , let dfun = instanceDFunId inst
               , idType dfun `eqType` boot_dfun_ty ]
       where
          boot_dfun_ty   = idType boot_dfun


-- In general, to perform these checks we have to
-- compare the TyThing from the .hi-boot file to the TyThing
-- in the current source file.  We must be careful to allow alpha-renaming
-- where appropriate, and also the boot declaration is allowed to omit
-- constructors and class methods.
--
-- See rnfail055 for a good test of this stuff.

-- | Compares two things for equivalence between boot-file and normal code,
-- reporting an error if they don't match up.
checkBootDeclM :: HsBootOrSig
               -> TyThing -- ^ boot thing
               -> TyThing -- ^ real thing
               -> TcM ()
checkBootDeclM boot_or_sig boot_thing real_thing
  = for_ boot_errs $ \ boot_err ->
      addErrAt span $
        TcRnBootMismatch boot_or_sig $
        BootMismatch boot_thing real_thing boot_err
  where
    boot_errs = execWriter $ checkBootDecl boot_or_sig boot_thing real_thing

    -- Here we use the span of the boot thing or, if it doesn't have a sensible
    -- span, that of the real thing,
    span
      | let span = nameSrcSpan (getName boot_thing)
      , isGoodSrcSpan span
      = span
      | otherwise
      = nameSrcSpan (getName real_thing)

-- | Writer monad for accumulating errors when comparing an hs-boot or
-- signature file with its implementing module.
type BootErrsM err = Writer [err] ()

-- | If the test in the first parameter is True, succeed.
-- Otherwise, record the given error.
check :: Bool -> err -> BootErrsM err
check True  _   = checkSuccess
check False err = bootErr err

-- | Record an error.
bootErr :: err -> BootErrsM err
bootErr err = tell [err]

-- | A convenience synonym for a lack of errors, for @checkBootDecl@ and friends.
checkSuccess :: BootErrsM err
checkSuccess = return ()

-- | Map over the error types in an error-accumulating computation.
embedErrs :: (err1 -> err2) -> BootErrsM err1 -> BootErrsM err2
embedErrs f = mapWriter (second (fmap f))

-- | Wrap up a list of errors into a single message.
wrapErrs :: (NE.NonEmpty err1 -> err2) -> BootErrsM err1 -> BootErrsM err2
wrapErrs f w =
  case execWriter w of
    []         -> checkSuccess
    err : errs -> bootErr (f $ err :| errs)

-- | Compares the two things for equivalence between boot-file and normal
-- code. Returns @Nothing@ on success or @Just "some helpful info for user"@
-- failure. If the difference will be apparent to the user, @Just empty@ is
-- perfectly suitable.
checkBootDecl :: HsBootOrSig -> TyThing -> TyThing -> BootErrsM BootMismatchWhat

checkBootDecl _ (AnId id1) (AnId id2)
  = assert (id1 == id2) $
    check (idType id1 `eqType` idType id2)
          (BootMismatchedIdTypes id1 id2)

checkBootDecl boot_or_sig (ATyCon tc1) (ATyCon tc2)
  = wrapErrs (BootMismatchedTyCons tc1 tc2) $
    checkBootTyCon boot_or_sig tc1 tc2

checkBootDecl _ t1 t2
  = pprPanic "checkBootDecl" (ppr t1 $$ ppr t2)

-- | Run the check provided for every pair of elements in the lists.
--
-- Records an error:
--
--  - when any two items at the same position in the two lists don't match
--    according to the given function,
--  - when the lists are of different lengths.
checkListBy :: (a -> a -> BootErrsM err) -> [a] -> [a]
            -> (BootListMismatches a err -> err2)
            -> BootErrsM err2
checkListBy check_fun as bs mk_err = wrapErrs mk_err $ go 1 as bs
  where
    go _  [] [] = checkSuccess
    go !i (x:xs) (y:ys) =
      do { embedErrs (MismatchedThing i x y) $ check_fun x y
         ; go (i+1) xs ys }
    go _  _  _ = bootErr MismatchedLength

----------------
checkBootTyCon :: HsBootOrSig -> TyCon -> TyCon -> BootErrsM BootTyConMismatch
checkBootTyCon boot_or_sig tc1 tc2
  | not (eqType (tyConKind tc1) (tyConKind tc2))
  -- First off, check the kind
  = bootErr TyConKindMismatch

  | Just c1 <- tyConClass_maybe tc1
  , Just c2 <- tyConClass_maybe tc2
  , let (clas_tvs1, clas_fds1, sc_theta1, _, ats1, op_stuff1)
          = classExtraBigSig c1
        (clas_tvs2, clas_fds2, sc_theta2, _, ats2, op_stuff2)
          = classExtraBigSig c2
  , Just env <- eqVarBndrs emptyRnEnv2 clas_tvs1 clas_tvs2
  = do { check_roles
       ; embedErrs (TyConMismatchedClasses c1 c2) $
    do { -- Checks kind of class
       ; check (liftEq (eqFD env) clas_fds1 clas_fds2)
           MismatchedFunDeps
       ; unless (isAbstractTyCon tc1) $
    do { check (liftEq (eqTypeX env) sc_theta1 sc_theta2)
           MismatchedSuperclasses
       ; checkListBy (compatClassOp env boot_or_sig) op_stuff1 op_stuff2
           MismatchedMethods
       ; checkListBy (compatAT env boot_or_sig) ats1 ats2
           MismatchedATs
       ; check (classMinimalDef c1 `BF.implies` classMinimalDef c2)
           MismatchedMinimalPragmas
       } } }

  | Just syn_rhs1 <- synTyConRhs_maybe tc1
  , Just syn_rhs2 <- synTyConRhs_maybe tc2
  , Just env <- eqVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = assert (tc1 == tc2) $
  do { check_roles
     ; check (eqTypeX env syn_rhs1 syn_rhs2) $
        TyConSynonymMismatch syn_rhs1 syn_rhs2 }

  -- This allows abstract 'data T a' to be implemented using 'type T = ...'
  -- and abstract 'class K a' to be implement using 'type K = ...'
  -- See Note [Synonyms implement abstract data]
  | Hsig <- boot_or_sig -- don't support for hs-boot yet
  , isAbstractTyCon tc1
  , Just (tvs, ty) <- synTyConDefn_maybe tc2
  = checkSynAbsData tc1 tc2 tvs ty

  | Just fam_flav1 <- famTyConFlav_maybe tc1
  , Just fam_flav2 <- famTyConFlav_maybe tc2
  = assert (tc1 == tc2) $
    do { let injInfo1 = tyConInjectivityInfo tc1
             injInfo2 = tyConInjectivityInfo tc2
       ; -- check equality of roles, family flavours and injectivity annotations
         -- (NB: Type family roles are always nominal. But the check is
         -- harmless enough.)
       ; check_roles
       ; compatFamFlav fam_flav1 fam_flav2
       ; check (injInfo1 == injInfo2) TyConInjectivityMismatch }

  | isAlgTyCon tc1 && isAlgTyCon tc2
  , Just env <- eqVarBndrs emptyRnEnv2 (tyConTyVars tc1) (tyConTyVars tc2)
  = assert (tc1 == tc2) $
  do { check_roles
     ; let rhs1 = algTyConRhs tc1
           rhs2 = algTyConRhs tc2
     ; embedErrs (TyConMismatchedData rhs1 rhs2) $
  do { check (liftEq (eqTypeX env)
                     (tyConStupidTheta tc1) (tyConStupidTheta tc2))
                      MismatchedDatatypeContexts
     ; compatAlgRhs rhs1 rhs2 } }

  | otherwise = bootErr TyConsVeryDifferent
                -- two very different types;
                -- should be obvious to the user what the problem is
  where
    check_roles = checkRoles boot_or_sig tc1 (tyConRoles tc2)


emptyRnEnv2 :: RnEnv2
emptyRnEnv2 = mkRnEnv2 emptyInScopeSet

-- | Check that two class methods have compatible type signatures.
compatClassOp :: RnEnv2 -> HsBootOrSig -> ClassOpItem -> ClassOpItem -> BootErrsM BootMethodMismatch
compatClassOp env boot_or_sig (id1, def_meth1) (id2, def_meth2)
  = do { check (name1 == name2) $
           MismatchedMethodNames
       ; check (eqTypeX env op_ty1 op_ty2) $
           MismatchedMethodTypes op_ty1 op_ty2
       ; case boot_or_sig of
           HsBoot ->
             check (liftEq eqDM def_meth1 def_meth2) $
               MismatchedDefaultMethods False
           Hsig ->
             check (subDM op_ty1 def_meth1 def_meth2) $
               MismatchedDefaultMethods True }
  where
    name1 = idName id1
    name2 = idName id2
    op_ty1 = classMethodTy id1
    op_ty2 = classMethodTy id2

    eqDM (_, VanillaDM)    (_, VanillaDM)    = True
    eqDM (_, GenericDM t1) (_, GenericDM t2) = eqTypeX env t1 t2
    eqDM _ _ = False

    -- NB: first argument is from hsig, second is from real impl.
    -- Order of pattern matching matters.
    subDM _ Nothing _ = True
    subDM _ _ Nothing = False

    -- If the hsig wrote:
    --
    --   f :: a -> a
    --   default f :: a -> a
    --
    -- this should be validly implementable using an old-fashioned
    -- vanilla default method.
    subDM t1 (Just (_, GenericDM gdm_t1)) (Just (_, VanillaDM))
     = eqType t1 gdm_t1   -- Take care (#22476).  Both t1 and gdm_t1 come
                             -- from tc1, so use eqType, and /not/ eqTypeX

    -- This case can occur when merging signatures
    subDM t1 (Just (_, VanillaDM)) (Just (_, GenericDM t2))
     = eqTypeX env t1 t2

    subDM _ (Just (_, VanillaDM)) (Just (_, VanillaDM)) = True
    subDM _ (Just (_, GenericDM t1)) (Just (_, GenericDM t2))
     = eqTypeX env t1 t2

-- | Check that two associated types are compatible.
compatAT :: RnEnv2 -> HsBootOrSig -> ClassATItem -> ClassATItem
         -> BootErrsM BootATMismatch
compatAT env boot_or_sig (ATI tc1 def_ats1) (ATI tc2 def_ats2)
  = do { embedErrs MismatchedTyConAT $
           checkBootTyCon boot_or_sig tc1 tc2
       ; check (compatATDef def_ats1 def_ats2)
           MismatchedATDefaultType }

  where
    -- Ignore the location of the defaults
    compatATDef Nothing             Nothing             = True
    compatATDef (Just (ty1, _loc1)) (Just (ty2, _loc2)) = eqTypeX env ty1 ty2
    compatATDef _ _ = False

-- | Check that two functional dependencies are the same.
eqFD :: RnEnv2 -> FunDep TyVar -> FunDep TyVar -> Bool
eqFD env (as1,bs1) (as2,bs2) =
  liftEq (eqTypeX env) (mkTyVarTys as1) (mkTyVarTys as2) &&
  liftEq (eqTypeX env) (mkTyVarTys bs1) (mkTyVarTys bs2)

-- | Check compatibility of two type family flavours.
compatFamFlav :: FamTyConFlav -> FamTyConFlav -> BootErrsM BootTyConMismatch
compatFamFlav (DataFamilyTyCon {}) (DataFamilyTyCon {}) = checkSuccess
compatFamFlav OpenTypeFamilyTyCon OpenTypeFamilyTyCon = checkSuccess
compatFamFlav ConstrainedTypeFamilyTyCon ConstrainedTypeFamilyTyCon = checkSuccess
compatFamFlav flav1@(ClosedTypeFamilyTyCon ctf1) flav2@(ClosedTypeFamilyTyCon ctf2) =
  case (ctf1, ctf2) of
    (CTF_Abstract, CTF_Abstract) -> checkSuccess -- This case only happens for hsig merging
    (CTF {}, CTF_Abstract) -> checkSuccess
    (CTF_Abstract, CTF {}) -> checkSuccess
    (CTF ax1, CTF ax2) -> eqClosedFamilyAx ax1 ax2
    (CTF_Scoped owner1 sum1 ax1, CTF_Scoped owner2 sum2 ax2)
      | owner1 == owner2, sum1 == sum2 -> eqClosedFamilyAx (Just ax1) (Just ax2)
    (CTF_BuiltIn {}, CTF_BuiltIn {}) -> checkSuccess
    _ -> bootErr $ TyConFlavourMismatch flav1 flav2
compatFamFlav flav1 flav2 = bootErr $ TyConFlavourMismatch flav1 flav2

-- | Check that two 'AlgTyConRhs's are compatible.
compatAlgRhs :: AlgTyConRhs -> AlgTyConRhs -> BootErrsM BootDataMismatch
compatAlgRhs (AbstractTyCon {}) _rhs2 =
  checkSuccess -- rhs2 is guaranteed to be injective, since it's an AlgTyCon
compatAlgRhs  tc1@DataTyCon{} tc2@DataTyCon{} =
  checkListBy compatCon (data_cons tc1) (data_cons tc2) MismatchedConstructors
compatAlgRhs  tc1@NewTyCon{ data_con = dc1 } tc2@NewTyCon{ data_con = dc2 } =
  embedErrs (MismatchedConstructors . NE.singleton . MismatchedThing 1 dc1 dc2) $
    compatCon (data_con tc1) (data_con tc2)
compatAlgRhs _ _ = bootErr MismatchedNewtypeVsData

-- | Check that two 'DataCon's are compatible.
compatCon :: DataCon -> DataCon -> BootErrsM BootDataConMismatch
compatCon c1 c2
  = do { check (dataConName c1 == dataConName c2)
           MismatchedDataConNames
       ; check (dataConIsInfix c1 == dataConIsInfix c2)
           MismatchedDataConFixities
       ; check (liftEq eqHsBang (dataConImplBangs c1) (dataConImplBangs c2))
           MismatchedDataConBangs
       ; check (map flSelector (dataConFieldLabels c1) == map flSelector (dataConFieldLabels c2))
           MismatchedDataConFieldLabels
       ; check (eqType (dataConWrapperType c1) (dataConWrapperType c2))
           MismatchedDataConTypes }

eqClosedFamilyAx :: Maybe (CoAxiom br) -> Maybe (CoAxiom br1)
                 -> BootErrsM BootTyConMismatch
eqClosedFamilyAx Nothing Nothing  = checkSuccess
eqClosedFamilyAx Nothing (Just _) = bootErr $ TyConAxiomMismatch $ NE.singleton MismatchedLength
eqClosedFamilyAx (Just _) Nothing = bootErr $ TyConAxiomMismatch $ NE.singleton MismatchedLength
eqClosedFamilyAx (Just (CoAxiom { co_ax_branches = branches1 }))
                 (Just (CoAxiom { co_ax_branches = branches2 }))
  = checkListBy eqClosedFamilyBranch branch_list1 branch_list2
      TyConAxiomMismatch
  where
    branch_list1 = fromBranches branches1
    branch_list2 = fromBranches branches2

eqClosedFamilyBranch :: CoAxBranch -> CoAxBranch -> BootErrsM BootAxiomBranchMismatch
eqClosedFamilyBranch (CoAxBranch { cab_tvs = tvs1, cab_cvs = cvs1
                                 , cab_lhs = lhs1, cab_rhs = rhs1 })
                     (CoAxBranch { cab_tvs = tvs2, cab_cvs = cvs2
                                 , cab_lhs = lhs2, cab_rhs = rhs2 })
  | Just env1 <- eqVarBndrs emptyRnEnv2 tvs1 tvs2
  , Just env  <- eqVarBndrs env1        cvs1 cvs2
  = do { check (liftEq (eqTypeX env) lhs1 lhs2) MismatchedAxiomLHS
       ; check (eqTypeX env rhs1 rhs2)          MismatchedAxiomRHS }
  | otherwise
  = bootErr MismatchedAxiomBinders

{- Note [Role subtyping]
~~~~~~~~~~~~~~~~~~~~~~~~
In the current formulation of roles, role subtyping is only OK if the
"abstract" TyCon was not representationally injective.  Among the most
notable examples of non representationally injective TyCons are abstract
data, which can be implemented via newtypes (which are not
representationally injective).  The key example is
in this example from #13140:

     -- In an hsig file
     data T a -- abstract!
     type role T nominal

     -- Elsewhere
     foo :: Coercible (T a) (T b) => a -> b
     foo x = x

We must NOT allow foo to typecheck, because if we instantiate
T with a concrete data type with a phantom role would cause
Coercible (T a) (T b) to be provable.  Fortunately, if T is not
representationally injective, we cannot make the inference that a ~N b if
T a ~R T b.

Unconditional role subtyping would be possible if we setup
an extra set of roles saying when we can project out coercions
(we call these proj-roles); then it would NOT be valid to instantiate T
with a data type at phantom since the proj-role subtyping check
would fail.  See #13140 for more details.

One consequence of this is we get no role subtyping for non-abstract
data types in signatures. Suppose you have:

     signature A where
         type role T nominal
         data T a = MkT

If you write this, we'll treat T as injective, and make inferences
like T a ~R T b ==> a ~N b (mkSelCo).  But if we can
subsequently replace T with one at phantom role, we would then be able to
infer things like T Int ~R T Bool which is bad news.

We could allow role subtyping here if we didn't treat *any* data types
defined in signatures as injective.  But this would be a bit surprising,
replacing a data type in a module with one in a signature could cause
your code to stop typechecking (whereas if you made the type abstract,
it is more understandable that the type checker knows less).

It would have been best if this was purely a question of defaults
(i.e., a user could explicitly ask for one behavior or another) but
the current role system isn't expressive enough to do this.
Having explicit proj-roles would solve this problem.
-}

checkRoles :: HsBootOrSig -> TyCon -> [Role] -> BootErrsM BootTyConMismatch
checkRoles boot_or_sig tc1 r2
  |  boot_or_sig == HsBoot
  || isInjectiveTyCon tc1 Representational -- See Note [Role subtyping]
  = check (r1 == r2) (TyConRoleMismatch False)
  | otherwise
  = check (r2 `rolesSubtypeOf` r1) (TyConRoleMismatch True)
  where

    r1 = tyConRoles tc1

    rolesSubtypeOf [] [] = True
    -- NB: this relation is the OPPOSITE of the subroling relation
    rolesSubtypeOf (x:xs) (y:ys) = x >= y && rolesSubtypeOf xs ys
    rolesSubtypeOf _ _ = False

{- Note [Synonyms implement abstract data]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An abstract data type or class can be implemented using a type synonym,
but ONLY if:

  1. T, as a standalone occurrence, is a valid type
     (T is "curryable"), and

  2. T is valid in an instance head.

This gives rise to the following conditions under which we can implement
an abstract data declaration @data T@ using a type synonym @type T tvs = rhs@:

  1. The type synonym T is nullary (tvs is null).

  2. The rhs must not contain any foralls, quantified types, or type family
     applications.
     See 'invalidAbsDataSubTypes' which computes a collection of
     invalid subtypes.

See also 'HowAbstract' and Note [Skolem abstract data].
-}

-- | We are implementing an abstract data declaration of the form @data T@
-- in a signature file, with a type synonym @type T tvs = rhs@ in the
-- implementing module.
--
-- This function checks that the implementation is valid:
--
--  1. the type synonym T is nullary, i.e. tvs is null,
--  2. rhs doesn't contain any type families, foralls, or qualified types.
--
-- See Note [Synonyms implement abstract data]
checkSynAbsData :: TyCon   -- ^ @tc1@, the abstract data 'TyCon' we are implementing
                -> TyCon   -- ^ @tc2@, a type synonym @type T tvs = ty@
                           --   we are using to implement @tc1@
                -> [TyVar] -- ^ @tvs@
                -> Type    -- ^ @ty@
                -> BootErrsM BootTyConMismatch
checkSynAbsData tc1 tc2 syn_tvs syn_rhs
  -- We are implementing @data T@ with @type T tvs = rhs@.
  -- Check the conditions of Note [Synonyms implement abstract data].
  = do { -- (1): T is nullary.
       ; check (null syn_tvs) $
           SynAbstractData SynAbsDataTySynNotNullary
         -- (2): the RHS of the type synonym is valid.
       ; case invalidAbsDataSubTypes syn_rhs of
          []       -> checkSuccess
          err:errs -> bootErr $ SynAbstractData $
                      SynAbstractDataInvalidRHS (err :| errs)
         -- NB: this allows implementing e.g. @data T :: Nat@ with @type T = 3@.
         -- See #15138.

  -- TODO: When it's a synonym implementing a class, we really
  -- should check that the fundeps are satisfied, but
  -- there is not an obvious way to do this for a constraint synonym.
  -- So for now, let it all through (it won't cause segfaults, anyway).
  -- Tracked at #12704.

         -- ... we also need to check roles.
       ; if | Just (tc2', args) <- tcSplitTyConApp_maybe syn_rhs
            , null syn_tvs -- Don't report role errors unless the type synonym is nullary
            -> assert (null (tyConRoles tc2)) $
               -- If we have something like:
               --
               --  signature H where
               --      data T a
               --  module H where
               --      data K a b = ...
               --      type T = K Int
               --
               -- we need to drop the first role of K when comparing!
               checkRoles Hsig tc1 (drop (length args) (tyConRoles tc2'))
            | otherwise
            -> checkSuccess
       }

{-
    -- Hypothetically, if we were allow to non-nullary type synonyms, here
    -- is how you would check the roles
    if length tvs == length roles1
    then checkRoles roles1 roles2
    else case tcSplitTyConApp_maybe ty of
            Just (tc2', args) ->
                checkRoles Hsig tc1 (drop (length args) (tyConRoles tc2') ++ roles2)
            Nothing -> Just roles_msg
-}

-- | Is this type a valid implementation of abstract data?
--
-- Returns a list of invalid sub-types encountered.
invalidAbsDataSubTypes :: Type -> [Type]
invalidAbsDataSubTypes = execWriter . go
  where
    go :: Type -> Writer [Type] ()
    go ty
      | Just ty' <- coreView ty
      = go ty'
    go ty@(TermAppTy {})
      = invalid ty
    go ty@(TermVarTy {})
      = invalid ty
    go ty@(DepFunTy {})
      = invalid ty
    go TyVarTy{}
      = ok -- We report an error at the binding site of type variables,
           -- e.g. in the TySyn LHS or in the forall.
           -- It's not useful to report a second error for their occurrences
    go (AppTy t1 t2)
      = do { go t1; go t2 }
    go ty@(TyConApp tc tys)
      | isTypeFamilyTyCon tc
      = invalid ty
      | otherwise
      = mapM_ go tys
    go ty@(ForAllTy{})
      = invalid ty
    go ty@(FunTy af ma w t1 t2)
      | af == FTF_T_T
      = do { go ma; go w
           ; go (typeKind t1) ; go t1
           ; go (typeKind t2) ; go t2
           }
      | otherwise
      = invalid ty
    go LitTy{}
      = ok
    go ty@(CastTy{})
      = invalid ty
    go ty@(CoercionTy{})
      = invalid ty

    ok         = pure ()
    invalid ty = tell [ty]

{-
************************************************************************
*                                                                      *
        Type-checking the top level of a module (continued)
*                                                                      *
************************************************************************
-}

rnTopSrcDecls :: HsGroup GhcPs -> TcM (TcGblEnv, HsGroup GhcRn)
-- Fails if there are any errors
rnTopSrcDecls group
 = do { -- Rename the source decls
        traceRn "rn12" empty ;
        (tcg_env, rn_decls) <- checkNoErrs $ rnSrcDecls group ;
        traceRn "rn13" empty ;
        (tcg_env, rn_decls) <- runRenamerPlugin tcg_env rn_decls ;
        traceRn "rn13-plugin" empty ;

        -- save the renamed syntax, if we want it
        let { tcg_env'
                | Just grp <- tcg_rn_decls tcg_env
                  = tcg_env{ tcg_rn_decls = Just (appendGroups grp rn_decls) }
                | otherwise
                   = tcg_env };

                -- Dump trace of renaming part
        rnDump rn_decls ;
        return (tcg_env', rn_decls)
   }

tcTopSrcDecls :: [(Name, SrcSpan)] -> HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDecls demands group = tcTopSrcDeclsThen demands group pure

-- | Finish inside the scopes of all local definitions checked for this group.
tcTopSrcDeclsThen :: [(Name, SrcSpan)] -> HsGroup GhcRn
                  -> ((TcGblEnv, TcLclEnv) -> TcM (TcGblEnv, TcLclEnv))
                  -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDeclsThen [] group finish = tcTopSrcTables group >>= finish
tcTopSrcDeclsThen requests group finish = do
    experimental <- xoptM LangExt.ExperimentalDependentTypes
    let selectors = mkNameSet
          [unLoc name
          | grp <- hs_tyclds group
          , L _ ClassDecl{tcdCExt = (HsNestedGroup{ng_sigs = sigs}, _)} <- group_tyclds grp
          , L _ (ClassOpSig _ _ names _) <- sigs
          , name <- names]
        demands | experimental = filter (not . (`elemNameSet` selectors) . fst) requests
                | otherwise = requests
    checkLocalOpenDeferrals demands
    env <- getGblEnv
    let checked = mkNameSet
          [ idName owner | bind <- tcg_binds env
                         , owner <- collectHsBindBinders CollNoDictBinders (unLoc bind) ]
        (previous, current) = partition ((`elemNameSet` checked) . fst) demands
        check_current
          | null current = tcTopSrcTables group >>= finish
          | otherwise = tcIndependentFamilyProducers current group finish
    if null previous then check_current else do
      -- tc_rn_src_decls has solved these bindings before running each splice.
      failIfErrsM
      definitions <- localOpenDefinitions previous (tcg_ev_binds env) (tcg_binds env)
      withLocalCaseTables previous definitions check_current

-- | Do not observe a solved definition whose errors may have been deferred.
checkLocalOpenDeferrals :: [(Name, SrcSpan)] -> TcM ()
checkLocalOpenDeferrals [] = pure ()
checkLocalOpenDeferrals demands = do
    defer <- goptM Opt_DeferTypeErrors
    holes <- goptM Opt_DeferTypedHoles
    not_in_scope <- goptM Opt_DeferOutOfScopeVariables
    when (defer || holes || not_in_scope) $
      rejectLocalOpen demands "Disable deferred errors before observing a local definition."

-- | Publish the demanded cases inside the solved binding scope.
-- See Note [Checked local character cases].
withLocalCaseTables :: [(Name, SrcSpan)] -> (NameEnv (Id, CoreExpr), NameEnv Id)
                    -> TcM (TcGblEnv, TcLclEnv) -> TcM (TcGblEnv, TcLclEnv)
withLocalCaseTables demands definitions@(cores, _) check = do
    let alias_target name = pure $ case lookupNameEnv cores name of
          Just (_, Core.Var target) -> unitNameSet (idName target)
          _ -> emptyNameSet
    demanded <- closeLocalNames alias_target emptyNameSet (map fst demands)
    let selected (owner, _) = idName owner `elemNameSet` demanded
        cases = filterNameEnv selected cores
    tables <- sourceCharacterTables cases
    witnesses <- sourceRetainedWitnessTables cases
    (boolean_cases, selectors) <- sourceLocalBooleanCases cases cores
    with_selectors <- if null selectors then getGblEnv else fst <$> addTyConsToGblEnv selectors
    checked <- setGblEnv with_selectors (installSourceTables (tables ++ witnesses ++ boolean_cases))
    setGblEnv checked (withLocalOpenDefinitions definitions check)

{- Note [Checked local character cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Renaming resolves the call to the ordinary term Name. For a same-group
demand, tcLocalOpenGroup checks its dependency closure in an early batch.
For an earlier-group demand, the splice path has already solved the
bindings. localOpenDefinitions checks the resolved bindings and rejects
pending typed splices before zonking.

Certify only demanded bindings and their exact alias targets. Give the
type checker the installed family for the same owner. Checking stays
inside the solved scope and uses the terms already in scope. Record family
use through the usual rule checks so a later rule can invalidate the proof.
-}

-- | Check only an independent producer fragment before family equations.
-- See Note [Independent producers for family equations].
tcIndependentFamilyProducers :: [(Name, SrcSpan)] -> HsGroup GhcRn
                             -> ((TcGblEnv, TcLclEnv) -> TcM (TcGblEnv, TcLclEnv))
                             -> TcM (TcGblEnv, TcLclEnv)
tcIndependentFamilyProducers demands group@HsGroup
  { hs_valds = XValBindsLR (HsVBG binds sigs), hs_tyclds = groups } finish = do
    env <- getGblEnv
    plugins <- readTcRef (tcg_plugins env)
    let roots = mkNameSet (map fst demands)
        pending = concatMap instance_uses (concatMap group_instds groups)
        instance_uses inst = case unLoc inst of
          TyFamInstD {tfid_inst = equation@TyFamInstDecl
            {tfid_eqn = FamEqn {feqn_tycon = L _ family}}}
            | not (nameIsLocalOrFrom (tcg_semantic_mod env) family) ->
                [(getLocA inst, localUses roots equation)]
          ClsInstD {cid_inst = ClsInstDecl
            {cid_ext = (_, HsNestedGroup {ng_tyfam_insts = equations})}} ->
              [(getLocA rhs, localUses roots rhs)
              | L _ TyFamInstDecl {tfid_eqn = FamEqn {feqn_rhs = rhs}} <- equations]
          _ -> []
        in_equation (name, loc) (inst_loc, uses) = loc `isSubspanOf` inst_loc && name `elem` uses
        selected = [d | d <- demands, any (in_equation d) pending]
        ordinary_solver = null (solverTcMPlugins plugins) && null (defaultingTcMPlugins plugins) &&
                          isNullUFM (rewriterTcMPlugins plugins)
    let this_mod = tcg_semantic_mod env
        data_groups = mkNameEnv
          [(tcdName (unLoc decl), tg) | tg <- groups, decl <- group_tyclds tg]
        constructor_owners = mkNameEnv
          [(con, tcdName (unLoc decl))
          | tg <- groups, decl@(L _ DataDecl{tcdDataDefn = defn}) <- group_tyclds tg
          , DataTypeCons False cons <- [dd_cons defn], con_decl <- cons, L _ con <- getConNames (unLoc con_decl)]
        value_names = mkNameSet (collectHsValBinders CollNoDictBinders (hs_valds group))
        closed_family_uses decl@(L _ FamDecl{tcdFam = FamilyDecl
          {fdLName = L _ family, fdInfo = ClosedTypeFamily _ (Just equations)}})
          | null (localUses value_names decl)
          , all (not . elemNameSet family . externalDeclUses . feqn_rhs . unLoc) equations =
              Just (delFromNameSet (externalDeclUses decl) family)
        closed_family_uses _ = Nothing
        early_type_dependencies name
          | not (nameIsLocalOrFrom this_mod name) = Right emptyNameSet
          | otherwise = case lookupNameEnv data_groups name of
              Just tg
                | Just fv_sets <- traverse early_decl_uses (group_tyclds tg)
                , null (group_instds tg)
                , let decls = group_tyclds tg
                      signatures = group_kisigs tg
                , isSingleton decls || all (not . isFamilyDecl . unLoc) decls
                , null (localUses value_names signatures)
                , not (hasSplice (decls, signatures)) ->
                    Right (unionNameSets (group_ext tg : externalDeclUses signatures : fv_sets))
              _ -> Left "a type declaration dependency"
        early_decl_uses decl
          | Just refs <- closed_family_uses decl = Just refs
          | L _ SynDecl{tcdTyVars = tvs, tcdRhs = rhs} <- decl
          , Just params <- traverse ordinary_parameter (hsQTvExplicit tvs) =
              independentTypeRefs (mkNameSet params) rhs
          | otherwise = independentDataDecl decl
        ordinary_parameter (L _ b@HsTvb{tvb_flag = HsBndrRequired{}, tvb_kind = HsBndrNoKind{}}) =
          hsTyVarName b
        ordinary_parameter _ = Nothing
        type_prerequisites constructors signature_types =
          either (const Nothing) Just $
            closeLocalNames early_type_dependencies emptyNameSet
              (nameSetElemsStable signature_types ++
               mapMaybe (lookupNameEnv constructor_owners) (nameSetElemsStable constructors))
        check_data_import name
          | name `hasKey` liftedTypeKindTyConKey = pure True
          | isTyConName name = do
              thing <- tcLookupGlobal name
              pure $ case thing of
                ATyCon tc -> independentDataKind tc
                _ -> False
          | otherwise = pure False
    candidate <- case independentClosure this_mod constructor_owners binds sigs (map fst selected) of
      Just (names, constructors, imported, signature_types)
        | not (null selected), ordinary_solver
        , Just prerequisites <- type_prerequisites constructors signature_types -> do
            let local_data = filterNameSet (nameIsLocalOrFrom this_mod) prerequisites
                imported_data = prerequisites `minusNameSet` local_data
            checked_data <- and <$> traverse check_data_import (nameSetElemsStable imported_data)
            if checked_data then do
              verified <- traverse independentImport (nameSetElemsStable imported)
              pure ((names, constructors, local_data,) . mkNameSet <$> sequence verified)
            else pure Nothing
      _ -> pure Nothing
    case candidate of
      Just (names, constructors, data_names, imported) -> do
        let (early_groups, rest_groups) = partition
              (any ((`elemNameSet` data_names) . tcdName . unLoc) . group_tyclds) groups
            (early_binds, rest_binds) = partition (any (selectedBind names) . snd) binds
            (early_sigs, rest_sigs) = splitSignatures names sigs
            early = emptyRnGroup { hs_tyclds = early_groups
                                 , hs_valds = XValBindsLR (HsVBG early_binds early_sigs) }
            rest = group { hs_tyclds = rest_groups
                         , hs_valds = XValBindsLR (HsVBG rest_binds rest_sigs) }
        (envs@(early_env, _), wanted) <- captureTopConstraints (tcTopSrcDeclGroup early)
        restoreEnvs envs $ do
          evs <- checkNoErrs (simplifyTop wanted)
          checked@(definitions, _) <- localOpenDefinitions selected
            (tcg_ev_binds early_env `unionBags` evs) (tcg_binds early_env)
          workers <- traverse independentImport (nameSetElemsStable constructors)
          let allowed = unionNameSets [names, imported, mkNameSet (catMaybes workers)]
              planned_types = mkNameSet
                [tcdName decl | tg <- early_groups, L _ decl <- group_tyclds tg,
                               isFamilyDecl decl || isSynDecl decl]
          unless (all isJust workers && isEmptyBag evs &&
                    all (independentCore planned_types allowed . snd)
                        (nonDetNameEnvElts definitions)) $
            rejectLocalOpen selected "The checked producer requires evidence outside the independent fragment."
          addTopEvBinds evs $ withLocalOpenDefinitions checked $
            tcTopSrcDeclsThen (filter (not . (`elemNameSet` names) . fst) demands) rest finish
      _ -> tcLocalOpenGroup demands group finish
tcIndependentFamilyProducers demands group finish = tcLocalOpenGroup demands group finish

-- | Select ordinary declarations and retain resolved dependencies omitted
-- from the local SCC graph, including references to imported families.
independentDataDecl :: LTyClDecl GhcRn -> Maybe NameSet
independentDataDecl (L _ DataDecl
  { tcdDExt = DataDeclRn{tcdFVs = fvs}, tcdModifiers = []
  , tcdTyVars = HsQTvs{hsq_explicit = params}
  , tcdDataDefn = HsDataDefn{dd_derivs = [], dd_ctxt = Nothing, dd_kindSig = Nothing,
                            dd_cons = DataTypeCons False cons} })
  | all bare_binder params, all vanilla_con cons =
      let bound = mkNameSet (mapMaybe (hsTyVarName . unLoc) params)
      in Just (fvs `minusNameSet` bound)
  where
    bare_binder (L _ HsTvb{tvb_kind = HsBndrNoKind _}) = True
    bare_binder _ = False
    vanilla_con (L _ ConDeclH98{con_modifiers = [], con_telescope = []}) = True
    vanilla_con _ = False
independentDataDecl _ = Nothing

-- | Close a producer over independent signatures and lexical applications.
independentClosure :: Module -> NameEnv Name -> [(RecFlag, [LHsBind GhcRn])] -> [LSig GhcRn]
                   -> [Name] -> Maybe (NameSet, NameSet, NameSet, NameSet)
independentClosure this_mod constructors binds sigs roots = do
    names <- foldM (visit emptyNameSet) emptyNameSet roots
    let local = filterNameSet (nameIsLocalOrFrom this_mod) names
    let local_cons = filterNameSet (`elemNameEnv` constructors) local
        local_binds = local `minusNameSet` local_cons
    signature_types <- traverse (fmap snd . signature) (nameSetElemsStable local_binds)
    pure (local_binds, local_cons, names `minusNameSet` local, unionNameSets signature_types)
  where
    signatures = signaturesByOwner sigs
    functions = mkNameEnv
      [(name, matches) | (NonRecursive, [L _ FunBind{fun_id = L _ name, fun_matches = matches}]) <- binds]

    -- Complete signatures can split a mutually recursive group for typing.
    -- Check cycles in the resolved body dependencies too.
    visit active done name
      | name `elemNameSet` done = Just done
      | name `elemNameSet` active = Nothing
      | otherwise = do
          refs <- deps name
          checked <- foldM (visit (extendNameSet active name)) done (nameSetElemsStable refs)
          pure (extendNameSet checked name)

    deps name
      | name `elemNameEnv` constructors = Just emptyNameSet
      | isExternalName name, not (nameIsLocalOrFrom this_mod name) = Just emptyNameSet
      | Just matches <- lookupNameEnv functions name
      , Just (tyvars, _) <- signature name
      , Just refs <- match_refs tyvars emptyNameSet matches
      = Just refs
      | otherwise = Nothing

    signature name
      | let pragmas = fromMaybe [] (lookupNameEnv signatures name)
      , all ordinary_pragma pragmas = independent_signature (filter is_type_sig pragmas)
      | otherwise = Nothing

    is_type_sig TypeSig{} = True
    is_type_sig _ = False
    ordinary_pragma TypeSig{} = True
    ordinary_pragma InlineSig{} = True
    ordinary_pragma _ = False

    independent_signature [] = Just (emptyNameSet, emptyNameSet)
    independent_signature [TypeSig _ [] _ (HsWC _ (L _ HsSig
      { sig_bndrs = outer, sig_body = body }))]
      | Just params <- signature_binders outer
      , let bound = mkNameSet params
      , Just refs <- independentTypeRefs bound body = Just (bound, refs)
    independent_signature _ = Nothing
    signature_binders HsOuterImplicit{hso_ximplicit = quantified} = Just quantified
    signature_binders HsOuterExplicit{hso_matchability = Nothing, hso_bndrs = quantified} =
      traverse bare_binder quantified
    signature_binders _ = Nothing
    bare_binder (L _ b@HsTvb {tvb_kind = HsBndrNoKind _}) = hsTyVarName b
    bare_binder _ = Nothing
    arrow_type params ty = case unLoc (ignoreParens ty) of
      HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) input result ->
        arrow_type params input && arrow_type params result
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ n)) -> n `elemNameSet` params
      _ -> False

    match_refs tyvars locals (MG {mg_alts = L _ [L _ Match
      { m_pats = L _ pats
      , m_grhss = GRHSs _ (L _ (GRHS _ [] rhs) :| []) (L _ EmptyLocalBinds{}) }]}) = do
      binders <- traverse pat_binders pats
      expr_refs tyvars (locals `unionNameSet` mkNameSet (catMaybes binders)) rhs
    match_refs _ _ _ = Nothing
    pat_binders (L _ (VarPat _ (L _ n))) = Just (Just n)
    pat_binders (L _ WildPat{}) = Just Nothing
    pat_binders (L _ (ParPat _ p)) = pat_binders p
    pat_binders _ = Nothing
    expr_refs tyvars locals (L _ expr) = case expr of
      HsVar _ (L _ (WithUserRdr _ n)) ->
        Just (if n `elemNameSet` locals then emptyNameSet else unitNameSet n)
      HsPar _ e -> recur e
      HsApp _ fun arg -> unionNameSet <$> recur fun <*> recur arg
      HsLam _ LamSingle matches -> match_refs tyvars locals matches
      ExprWithTySig _ e (HsWC [] (L _ HsSig
        { sig_bndrs = HsOuterImplicit{hso_ximplicit = []}, sig_body = ty }))
        | arrow_type tyvars ty -> recur e
      _ -> Nothing
      where
        recur = expr_refs tyvars locals

-- | Read dependencies in an ordinary producer signature or synonym RHS.
independentTypeRefs :: NameSet -> LHsType GhcRn -> Maybe NameSet
independentTypeRefs params ty = case unLoc (ignoreParens ty) of
  HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) input result ->
    unionNameSet <$> recur input <*> recur result
  HsAppTy _ fun arg -> unionNameSet <$> recur fun <*> recur arg
  HsTyVar _ NotPromoted (L _ (WithUserRdr _ n))
    | n `elemNameSet` params -> Just emptyNameSet
    | isTyConName n, isExternalName n -> Just (unitNameSet n)
  _ -> Nothing
  where
    recur = independentTypeRefs params

-- | Verify an imported occurrence and return the name found in checked Core.
independentImport :: Name -> TcM (Maybe Name)
independentImport name = do
    thing <- tcLookupGlobal name
    pure $ case thing of
      AnId ident
        | independentType emptyNameSet (idType ident) -> Just (idName ident)
      AConLike (RealDataCon con)
        | isJust (independentDataFields con (mkTyVarTys (dataConUnivTyVars con))) ->
            Just (idName (dataConWorkId con))
      _ -> Nothing

-- | Admit only boxed ordinary datatypes with a telescope of Type binders.
independentDataKind :: TyCon -> Bool
independentDataKind tc =
  isVanillaAlgTyCon tc && isBoxedDataTyCon tc && isGenerativeTyCon tc Nominal &&
  independentConstant liftedTypeKind (tyConResKind tc) &&
  all (\v -> isTyVar v && independentConstant liftedTypeKind (varType v)) (tyConTyVars tc)

-- | Match a constructor whose user arguments and worker arguments agree.
independentDataFields :: DataCon -> [Type] -> Maybe [Type]
independentDataFields con parameters
  | independentDataKind (dataConTyCon con)
  , dataConUserTyVars con == dataConUnivTyVars con =
      openDataConFieldTypes con parameters
  | otherwise = Nothing

-- | Recheck pre-optimisation Core emitted by the source fragment.
independentCore :: NameSet -> NameSet -> CoreExpr -> Bool
independentCore planned_types producers = go emptyNameSet emptyNameSet
  where
    valid_type = independentTypeWith planned_types

    go tyvars locals expression
      | (Core.Var worker, args) <- Core.collectArgs expression
      , Just con <- isDataConWorkId_maybe worker =
          getName worker `elemNameSet` producers &&
          let (types, fields) = span Core.isTyCoArg args
          in case traverse core_type types of
            Just parameters
              | Just field_types <- independentDataFields con parameters ->
                  equalLength fields field_types &&
                  and (zipWith (\expr kind -> exprType expr `eqType` kind) fields field_types) &&
                  all (valid_type tyvars) parameters &&
                  all (go tyvars locals) fields
            _ -> False
    go tyvars locals (Core.Var v)
      | getName v `elemNameSet` locals =
          valid_type tyvars (varType v)
      | getName v `elemNameSet` producers =
          valid_type emptyNameSet (varType v)
      | otherwise = False
    go tyvars locals (Core.Lam v body)
      | isTyVar v, independentConstant liftedTypeKind (varType v) =
          go (extendNameSet tyvars (getName v)) locals body
      | term_binder tyvars v = go tyvars (extendNameSet locals (getName v)) body
      | otherwise = False
    go tyvars locals (Core.App fun operand) = go tyvars locals fun && go tyvars locals operand
    go tyvars locals (Core.Let (Core.NonRec v rhs) body) =
      independent_let tyvars locals v rhs body
    -- The singleton inferred AbsBinds path in dsAbsBinds emits mkLetRec.
    -- Check its RHS without the monomorphic binder in scope.
    go tyvars locals (Core.Let (Core.Rec [(v, rhs)]) body) =
      independent_let tyvars locals v rhs body
    go tyvars locals (Core.Cast body co)
      | not (isEmptyNameSet planned_types)
      , let lhs = coercionLKind co
            rhs = coercionRKind co
            normalize = reductionReducedType . normaliseType
                          emptyFamInstEnvs Nominal
      , valid_type tyvars lhs
      , valid_type tyvars rhs
      , normalize lhs `eqType` normalize rhs =
          go tyvars locals body
    go tyvars _ (Core.Type ty) = valid_type tyvars ty
    go _ _ _ = False

    independent_let tyvars locals v rhs body =
      term_binder tyvars v && not (getName v `elemNameSet` producers) &&
      go tyvars locals rhs &&
      go tyvars (extendNameSet locals (getName v)) body

    term_binder tyvars v =
      isId v && not (isCoVar v) && independentConstant manyDataConTy (idMult v) &&
      valid_type tyvars (varType v)

    core_type (Core.Type ty) = Just ty
    core_type _ = Nothing

-- | Check variables, ordinary arrows and generative boxed data types.
independentType :: NameSet -> Type -> Bool
independentType = independentTypeWith emptyNameSet

independentTypeWith :: NameSet -> NameSet -> Type -> Bool
independentTypeWith planned_types = check
  where
    check tyvars (TyVarTy var) = getName var `elemNameSet` tyvars
    check tyvars FunTy{ft_af = FTF_T_T, ft_ma = ma, ft_mult = mult,
                                 ft_arg = input, ft_res = result} =
      independentConstant unmatchableDataConTy ma && independentConstant manyDataConTy mult &&
      check tyvars input && check tyvars result
    check tyvars (ForAllTy ma (Bndr var (Invisible _)) body) =
      independentConstant unmatchableDataConTy ma && isTyVar var &&
      independentConstant liftedTypeKind (varType var) &&
      check (extendNameSet tyvars (getName var)) body
    check tyvars ty@(TyConApp tc args)
      | tyConName tc `elemNameSet` planned_types
      , Just expanded <- coreView ty =
          check tyvars expanded
      | lengthIs args (tyConArity tc)
      , independentDataKind tc ||
        (isTypeFamilyTyCon tc && tyConName tc `elemNameSet` planned_types) =
          all (check tyvars) args
    check _ ty = independentConstant liftedTypeKind ty

-- | Match a wired-in type constant without ignoring casts in its arguments.
independentConstant :: Type -> Type -> Bool
independentConstant expected actual
  | TyConApp left xs <- coreFullView expected
  , TyConApp right ys <- coreFullView actual =
      left == right && equalLength xs ys && and (zipWith independentConstant xs ys)
  | otherwise = False

{- Note [Independent producers for family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An imported family can be named by an imported signature or a class
instance. A renamed name closure alone cannot justify moving an ordinary
producer ahead of that family's equation. Check a separate early batch
with lexical lambda/application bindings. An expression signature may
mention only ordinary arrows and the producer's scoped type variables.
Reject named types, wildcards and newly quantified annotation variables,
and syntax with overloaded operations. An imported Id
can enter this batch if its entire declared type is independent.
Check all binder kinds, argument types and arrow metadata. This
check uses its interface, without evaluating or certifying its body.
Imported data constructors have a source DataCon Name and a distinct
Core worker Name. Match that worker at its exact saturated application,
and check the representation and lazy field types. A generative boxed
datatype can appear in an imported type when the constructor kind
telescope is independent too. The observer checks the body and source
RULE dependencies only if the checked producer needs the imported Id.

For example, in "keep x = const x (hidden Proxy)", the dictionary for
hidden may change when the family equation comes into scope. Observing
that keep returns x cannot establish which dictionary GHC selected.
Even an ignored type annotation may use the very equation we are
checking. Inspect the renamed syntax before desugaring can erase it.
Compare resolved names. A type with the same spelling as a producer's
type variable can resolve to a different binder.

Check the ordinary bindings and solve them without the candidate
equation. Keep normal top-level inference for an unsigned binding.
The source dependency graph still excludes recursive SCCs. Check
each inferred monomorphic binding and its wrapper in the solved Core
before observing the actual producer body.

A constructor in the producer can require a local datatype. Move its
entire renamed type SCC, including role annotations, before checking
the producer. Follow the local dependencies in group_ext. Also check
DataDeclRn.tcdFVs, which retains imported references omitted from
group_ext. An imported family in a field can depend on the pending
equation. Reject it before moving any group. The regular source checker
then receives the remaining type groups and all instances, methods,
defaults and deriving declarations.

An associated equation can also use an independent local producer.
Select calls in the right-hand side only. Keep the entire class instance
in the regular pass. That pass checks its head and equations before
its methods. The early producer cannot use a dictionary that would
depend on the pending equation.
A signed producer can name a closed type family in an input or result.
Use those resolved names as additional roots of the same type SCC
closure. Check the family's declaration, its standalone kind signature
and their external references. Keep the signature with its group.
The wired-in Type synonym in the signature has no declaration dependency.
A recursive family or a cycle through another family blocks the split.
A local synonym with ordinary binders and a checked RHS can also
belong to the closure. Follow every dependency in it before moving
the group, including arguments that synonym expansion could erase.
The ordinary type checker still checks arity and synonym cycles.
Expand only the planned synonyms when validating the solved Core.
An imported open family in the transitive closure blocks the split,
even when it occurs only in a field of an unused constructor.

The checked Core may contain a cast from a planned closed family to
its reduced type. Validate the endpoints by nominal normalization with
empty open family instance environments. This check cannot use the
postponed equation or an unrelated instance. Restrict the planned
closed families to nonrecursive declarations before normalizing.
The source annotation and imported Id checks still use the original
family-free fragment.

With a typechecker plugin, the early batch could have effects or solve
constraints differently. Use the ordinary declaration order in that case.
-}

tcTopSrcTables :: HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcTables group = do
    enabled <- xoptM LangExt.ExperimentalDependentTypes
    env <- getGblEnv
    let use = TableInType (sourceTableTypeDependencies env group)
    case if enabled then splitLocalSourceTables use (tcg_dus env) group else Nothing of
      Nothing -> tcTopSrcDeclGroup group
      Just (needed, early, rest) -> do
        -- See Note [Deriving for early source tables].
        ((envs@(early_env, _), _, deriving_info), wanteds) <- captureTopConstraints $
          tcTopSrcDeclGroupWithMethods False Nothing early
        restoreEnvs envs $ do
          evs <- simplifyTop wanteds
          let names = mkNameSet (collectHsValBinders CollNoDictBinders (hs_valds early))
          ordinary <- sourceTablesWithMappers True names evs early_env
          let known = mkNameSet (map (tyConName . snd) ordinary)
              witness_owners = mkNameSet
                [idName owner | bind <- tcg_binds early_env
                              , owner <- collectHsBindBinders CollNoDictBinders (unLoc bind)
                              , idName owner `elemNameSet` names
                              , isMonomorphicRetainedWitness owner]
              witnesses = [(term, nameSrcSpan term)
                          | (term, family) <- needed, not (family `elemNameSet` known)
                          , term `elemNameSet` witness_owners]
          cases <- if null witnesses then pure [] else do
            -- See Note [Scheduling a retained witness projection].
            checkLocalOpenDeferrals witnesses
            failIfErrsM
            (cores, _) <- localOpenDefinitions witnesses (tcg_ev_binds early_env `unionBags` evs) (tcg_binds early_env)
            let wanted_names = mkNameSet (map fst witnesses)
                selected (owner, _) = idName owner `elemNameSet` wanted_names
            sourceRetainedWitnessTables (filterNameEnv selected cores)
          let tables = ordinary ++ cases
              found = mkNameSet (map (tyConName . snd) tables)
          for_ needed $ \(term, family) ->
            unless (family `elemNameSet` found) $
              setSrcSpan (nameSrcSpan term) (addErr (TcRnNoSourceTable term))
          failIfErrsM
          addTopEvBinds evs $ do
            tables_env <- installSourceTables tables
            setGblEnv tables_env $ do
              (rest_envs, _, _) <- tcTopSrcDeclGroupWithMethods False (Just deriving_info) rest
              pure rest_envs

{- Note [Scheduling a retained witness projection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A dependent result signature can name a local ordinary witness
projection. The renamer's existing source-table dependency selects
the typed worker before the consumer signature, even in reversed
source order. Read its identity from the checked bindings. The
early group's value Ids may not be in the global type environment.

After solving the early group, obtain the demanded Core definitions
through the local observer. This checks for pending typed splices
before zonking. Use the same direct witness check as the final
interface writer. Install the resulting table through the normal
dependency and RULE checks. The renamer still owns group and cycle
eligibility.
-}

{- Note [Deriving for early source tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked source worker can use a local datatype before the rest of its
declaration group. Check the datatype and worker first, then install the
worker's equations. Keep the datatype's DerivInfo for the regular instance
pass. The ordinary deriving code needs the explicit instances, defaults
and other deriving declarations in the same group. Run it once with all
the DerivInfo.

For example, deriving Show for a datatype with an imported field can use
a local Show instance for the field type. The source certificate checks
the early worker without this instance environment. It accepts only
checked matches and constructor equations without dictionary evidence.
-}

-- | Follow the same-group types needed by a source-table signature.
-- See Note [Ordering local source tables] in GHC.Rename.Module.
sourceTableTypeDependencies :: TcGblEnv -> HsGroup GhcRn -> (Name -> Maybe Int)
                            -> [(Name, Maybe Int)] -> Maybe NameSet
sourceTableTypeDependencies env HsGroup{hs_tyclds = groups} ready uses = do
    guard (all saturated uses)
    closeLocalNames dependencies emptyNameSet (map fst uses)
  where
    local_types = mkNameSet (map (tcdName . unLoc) (tyClGroupTyClDecls groups))
    aliases = mkNameEnv
      [ (alias, (length params, deps))
      | TyClGroup{group_ext = deps, group_kisigs = [], group_instds = [],
                   group_tyclds = [L _ SynDecl{tcdLName = L _ alias, tcdTyVars = tvs, tcdRhs = rhs}]} <- groups
      , let params = hsQTvExplicit tvs
      , all ordinary_parameter params
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ target))) <- [ignoreParens rhs]
      , target `elem` hsExplicitLTyVarNames tvs ||
        (isTyConName target && alias /= target && isNothing (sourceTableTermOcc (nameOccName target))) ]

    ordinary_parameter (L _ HsTvb{tvb_flag = HsBndrRequired{}, tvb_kind = HsBndrNoKind{}}) = True
    ordinary_parameter _ = False

    saturated (name, Nothing)
      | name `elemNameSet` local_types = isJust (ready name)
      | Just (ATyCon tc) <- lookupTypeEnv (tcg_type_env env) name
      = isVanillaAlgTyCon tc || (isTypeSynonymTyCon tc && isFamFreeTyCon tc)
      | Just gre <- lookupGRE_Name (tcg_rdr_env env) name
      = any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
      | otherwise = False
    saturated (name, Just supplied)
      | Just (arity, _) <- lookupNameEnv aliases name = supplied == arity
      | Just arity <- ready name = supplied == arity
      | supplied == 0 = True
      | Just (ATyCon tc) <- lookupTypeEnv (tcg_type_env env) name
      , isTypeSynonymTyCon tc, isFamFreeTyCon tc
      = supplied == length (tyConVisibleTyVars tc)
      | otherwise = False

    dependencies name
      | isJust (ready name) || not (name `elemNameSet` local_types) = Just emptyNameSet
      | otherwise = snd <$> lookupNameEnv aliases name

{- Note [Scheduling local open definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only the new application-head fallback records a demand. It keeps the
resolved local term Name and the call's source span. Pass the group's
demands to the scheduler before checking its type declarations. A standalone
kind signature can make a synonym's binder rigid. The term producer still
needs a checked body. Its ordinary forall
is erased and does not bind a retained term in the caller's type.

Follow the producer's resolved RHS dependencies in renamed SCC order.
The producer and any unsigned helper use ordinary inference. Include
every member of a recursive value SCC. Ordinary checking handles the
recursive term bindings. A type dependency on an unavailable declaration
still prevents early checking. Observation uses the existing reduction
limit when it demands a divergent member. Check
selected signatures for dependencies on declarations that have not
been checked. If a signature or RHS needs a local datatype or synonym,
include its whole renamed TyClGroup and lexical prerequisites. A reference
to a constructor or field selects its parent group. group_ext omits term
names: inspect the declarations and standalone kind signatures before
moving them ahead of the value group. A dependency back to the requested
function is a mixed cycle. If a type needs a different local function,
stage the ready functions first, then retry the remaining calls with their
checked definitions in scope. Each pass removes at least one demand.
See Note [Prepare TyClGroup FVs] in GHC.Rename.Module.
Instance selection and defaulting can affect expressions that never
name the relevant declaration. Bring all deriving clauses, standalone
deriving declarations and default declarations into the first pass along
with their required type groups. tcTyClsInstDecls installs the defaults
and derives the instances before checking the selected functions.
Later groups inherit both environments. If an instance or default
needs a type that depends on an unfinished local value, report the edge.
Keep INLINE pragmas with their bindings. Split a shared
signature between the groups at its original location. Check the ordinary
instance heads with the early group. Their original instance groups can
depend on method bodies. Build dependencies from the heads, modifiers
and associated type equations before moving them. The first pass checks
these equations with the instance heads and saves the InstInfo for each
instance. Check the methods after the remaining value bindings and before
returning to the enclosing splice group. Run that pass inside the
continuations of all scheduled batches. A method can then use a producer
whose checked Core becomes available in a later batch.

Schedule family instances with the types in their equations, constructor
fields and deriving clauses. The ordinary instance pass introduces data
constructors, record selectors and derived instances before checking
the early value bindings. These constructors and selectors belong to
the instance. They do not need a separate type declaration group.
This order concerns term uses of constructors. See Note [No dependencies
on data instances] in GHC.Rename.Module for the limits on promotion.

The ordinary late source RULE check validates the owners the observer
actually uses. An unrelated RULE does not stop the split. A producer
after a splice cannot be renamed
from the group's prefix. For a producer in an earlier group, use the binds
and evidence solved before the splice. Build the Core map on demand.

Solve and zonk the early binds before desugaring. Drop duplicate
desugaring messages and keep ordinary runtime compilation unchanged.
Follow the checked bindings' resolved term references to find helpers
from earlier groups. Inspect only the resulting closure for pending
typed splices. Pass only its checked Core to the observer. The type
checker cannot execute an object for the current module.

For observation, desugar each AbsBinds export separately. Keep the full
monomorphic binding group, evidence and export wrapper. This uses the
single-export desugaring path instead of building a synthetic tuple
and selecting its field. Ordinary Core generation still receives the
original AbsBinds. When observation demands a monomorphic binder, its
ABExport identifies the source owner. Record that owner for RULE checks.
-}

-- | Check the independently schedulable local definitions first.
tcLocalOpenGroup :: [(Name, SrcSpan)] -> HsGroup GhcRn
                 -> ((TcGblEnv, TcLclEnv) -> TcM (TcGblEnv, TcLclEnv))
                 -> TcM (TcGblEnv, TcLclEnv)
tcLocalOpenGroup demands group@HsGroup
  { hs_valds = XValBindsLR (HsVBG binds sigs)
  , hs_tyclds = tycls
  , hs_derivds = standalone, hs_defds = defaults } finish
  = do
      instances <- traverse (instance_group . snd) early_instances
      for_ late $ \(_, inst) ->
        when (hasSplice inst) $
          rejectLocalOpen (instance_calls (getLocA inst)) "This deferred type instance contains a splice."
      when (hasSplice (standalone, defaults)) $
        rejectLocalOpen demands "A splice prevents checking a standalone deriving or default declaration early."
      let heads = concatMap (nameSetElemsStable . group_ext) instances
          prerequisite_types = do
            with_heads <- closeLocalNames (typeDeps "the instance declarations" instanceNeedsTerm)
                                          emptyNameSet heads
            closeLocalNames (typeDeps "the deriving and default declarations" needsTerm)
                            with_heads instance_default_roots
      prerequisites <- case prerequisite_types of
        Left reason -> rejectLocalOpen demands reason
        Right names -> pure names
      for_ late_calls $ \(inst, family, leader) ->
        when (leader `elemNameSet` prerequisites) $
          rejectLocalOpen (instance_calls (getLocA inst))
            ("Cannot defer this type instance because the earlier declarations require "
             ++ getOccString family ++ ". This may form a type dependency cycle.")
      let (selected, ready, blocked) = foldl' tryDemand (prerequisites, [], []) demands
      case (ready, reverse blocked) of
        ([], (demand, reason) : _) -> rejectLocalOpen [demand] reason
        _ -> pure ()
      let (early_binds, rest_binds) = partition (any (selectedBind selected) . snd) binds
          (early_sigs, rest_sigs) = splitSignatures selected sigs
          type_groups = [tg { group_instds = [] } | tg <- tycls, not (null (group_tyclds tg))]
          (early_tys, rest_tys) = partition (any ((`elemNameSet` selected) . tcdName . unLoc)
                                                 . group_tyclds) type_groups
          early = emptyRnGroup { hs_valds = XValBindsLR (HsVBG early_binds early_sigs)
                               , hs_tyclds = early_tys ++ instances
                               , hs_derivds = standalone, hs_defds = defaults }
          rest = group { hs_valds = XValBindsLR (HsVBG rest_binds rest_sigs)
                       , hs_tyclds = rest_tys ++ map (uncurry make_instance_group) late
                       , hs_derivds = [], hs_defds = [] }
      ((envs@(early_env, _), methods, _), wanted) <- captureTopConstraints $
        tcTopSrcDeclGroupWithMethods (not (null instances)) (Just []) early
      restoreEnvs envs $ do
        evs <- checkNoErrs (simplifyTop wanted)
        let checked_demands = reverse ready
        definitions <- localOpenDefinitions checked_demands
                         (tcg_ev_binds early_env `unionBags` evs) (tcg_binds early_env)
        addTopEvBinds evs $ withLocalCaseTables checked_demands definitions $
          tcTopSrcDeclsThen (map fst (reverse blocked)) rest $ \rest_envs ->
            case methods of
              Nothing -> finish rest_envs
              Just (classes, infos) -> restoreEnvs rest_envs $ do
                bindings <- tcInstDecls2 classes infos
                (env, lcl) <- getEnvs
                finish (env { tcg_binds = tcg_binds env ++ bindings }, lcl)
  where
    group_names = mkNameSet (hsGroupBinders group)
    value_names = mkNameSet (collectHsValBinders CollNoDictBinders (hs_valds group))
    all_instances = [(group_ext tg, inst) | tg <- tycls, inst <- group_instds tg]
    (late, early_instances) = partition (belongs_to_late_family . snd) all_instances
    late_calls = mapMaybe (late_family_instance . snd) all_instances
    late_families = mkNameSet [family | (_, family, _) <- late_calls]
    late_family_leaders = mkNameSet [leader | (_, _, leader) <- late_calls]
    belongs_to_late_family inst =
      case unLoc inst of
        TyFamInstD{tfid_inst = TyFamInstDecl{tfid_eqn = FamEqn{feqn_tycon = L _ family}}} ->
          family `elemNameSet` late_families
        _ -> False

    instance_group inst = do
      names <- case unLoc inst of
        ClsInstD{cid_inst = ClsInstDecl
          { cid_poly_ty = head_ty, cid_modifiers = modifiers
          , cid_ext = (_, HsNestedGroup { ng_tyfam_insts = equations
                                       , ng_datafam_insts = data_instances }) }} ->
          concat <$> sequence [ instance_part "This instance head"
                                  (getLocA head_ty : map getLocA modifiers) (head_ty, modifiers)
                              , instance_part "An associated type equation" (map getLocA equations) equations
                              , instance_part "An associated data instance" (map getLocA data_instances) data_instances ]
        TyFamInstD{tfid_inst = equation} -> instance_part "This type instance" [getLocA inst] equation
        DataFamInstD{dfid_inst = data_inst} -> instance_part "This data instance" [getLocA inst] data_inst
      let deps = [leader | name <- names, Just (leader, _) <- [lookupNameEnv types name]]
      pure (make_instance_group (mkNameSet deps) inst)

    make_instance_group deps inst =
      TyClGroup { group_ext = deps, group_tyclds = []
                , group_kisigs = [], group_roles = [], group_instds = [inst] }

    -- See Note [Deferring a local type instance].
    late_family_instance inst
      | TyFamInstD{tfid_inst = equation@TyFamInstDecl
           {tfid_eqn = FamEqn{feqn_tycon = L _ family}}} <- unLoc inst
      , not (hasSplice equation)
      , not (null (localUses value_names equation))
      , Just (leader, tg) <- lookupNameEnv types family
      , any (declares_open_family family . unLoc) (group_tyclds tg) = Just (inst, family, leader)
      | otherwise = Nothing
    declares_open_family name FamDecl{tcdFam = FamilyDecl{fdLName = L _ family, fdInfo = info}} =
      name == family && isOpenTypeFamilyInfo info
    declares_open_family _ _ = False

    instance_calls loc =
      case filter ((`isSubspanOf` loc) . snd) demands of
        [] -> demands
        calls -> calls

    instance_part :: Data a => String -> [SrcSpan] -> a -> TcM [Name]
    instance_part what locations part
      | hasSplice part = rejectLocalOpen demands (what ++ " contains a splice.")
      | term : _ <- localUses value_names part = do
          let calls = [d | d@(n, loc) <- demands, n == term, any (loc `isSubspanOf`) locations]
              reason = what ++ " requires the unfinished local function " ++ getOccString term ++ "."
          -- A generated declaration can have no real source span.
          rejectLocalOpen (if null calls then demands else calls) reason
      | otherwise = pure (localUses group_names part)

    instance_default_roots =
      [tcdName (unLoc decl) | decl <- tyClGroupTyClDecls tycls, not (no_deriving decl)]
      ++ localUses group_names (standalone, defaults)
    instanceNeedsTerm dep term = "Checking an instance declaration requires "
                              ++ getOccString dep
                              ++ ", whose declaration uses the local function "
                              ++ getOccString term ++ "."
    needsTerm dep term = "Checking the deriving and default declarations requires "
                      ++ getOccString dep ++ ", whose declaration uses the local function "
                      ++ getOccString term ++ "."
    types = mkNameEnv [(binder, (leader, tg)) | tg <- tycls, first : _ <- [group_tyclds tg]
                                             , let leader = tcdName (unLoc first)
                                             , binder <- hsTyClForeignBinders [tg { group_instds = [] }] []]
    instance_binders = mkNameSet (hsTyClForeignBinders [tg { group_tyclds = [] } | tg <- tycls] [])
    graph = mkNameEnv
      [ (owner, (bind, fvs, group_edge))
      | (_, scc_binds) <- binds
      , Just definitions <- [traverse functionBinding scc_binds]
      , (representative, _, _) : _ <- [definitions]
      , let members = mkNameSet [name | (name, _, _) <- definitions]
      , (owner, bind, fvs) <- definitions
      , let group_edge = if owner == representative then members else unitNameSet representative ]
    functionBinding bind@(L _ FunBind{fun_id = L _ name, fun_ext = fvs}) =
      Just (name, bind, fvs)
    functionBinding _ = Nothing
    signatures = signaturesByOwner sigs

    tryDemand (selected, ready, blocked) demand@(name, _) =
      case visit name selected name of
        Right result -> (result, demand : ready, blocked)
        Left reason -> (selected, ready, (demand, reason) : blocked)

    visit root selected name
      | name `elemNameSet` selected = Right selected
      | otherwise = case lookupNameEnv graph name of
          Just (bind, fvs, members)
            | hasSplice bind -> Left ("A pending splice prevents observing " ++ getOccString name ++ ".")
            | otherwise -> do
                sig_deps <- mapM (signatureDeps name) (fromMaybe [] (lookupNameEnv signatures name))
                let rhs_deps = (fvs `intersectNameSet` group_names) `minusNameSet` value_names
                    required = unionNameSets (rhs_deps : sig_deps)
                    values = members `unionNameSet` (fvs `intersectNameSet` value_names)
                checked <- closeLocalNames (producerTypeDeps root name)
                                           (extendNameSet selected name)
                                           (nameSetElemsStable required)
                foldM (visit root) checked (nameSetElemsStable values)
          _ -> Left ("The local dependency " ++ getOccString name
                     ++ " is not an ordinary function binding.")
    signatureDeps name sig = case sig of
      TypeSig _ [] _ tau
        | not (isCompleteHsSig tau) ->
            Left ("The signature of " ++ getOccString name ++ " contains a wildcard.")
        | hasSplice tau ->
            Left ("The signature of " ++ getOccString name ++ " contains a splice.")
        | dep : _ <- localUses value_names tau ->
            Left ("The signature of " ++ getOccString name ++ " requires the local function "
                  ++ getOccString dep ++ " before it can be observed.")
        | otherwise -> Right (mkNameSet (localUses group_names tau))
      InlineSig{} -> Right emptyNameSet
      _ -> Left ("The local dependency " ++ getOccString name
                 ++ " has a signature or pragma that cannot be checked early.")

    valueNeedsTerm root owner dep term
      | term == root = "This call forms a type dependency cycle through " ++ getOccString owner
                       ++ " and " ++ getOccString dep ++ "."
      | otherwise = "The declaration of " ++ getOccString dep ++ " requires the local function "
                    ++ getOccString term ++ " before it can be observed."

    producerTypeDeps root owner dep
      | dep `elemNameSet` late_family_leaders =
          Left ("The local function " ++ getOccString owner ++ " requires "
                ++ getOccString dep ++ " before the dependent type instance can be checked.")
      | otherwise =
          typeDeps ("local dependency " ++ getOccString owner) (valueNeedsTerm root owner) dep

    typeDeps _ _ dep | dep `elemNameSet` instance_binders = Right emptyNameSet
    typeDeps owner needs_term dep = case lookupNameEnv types dep of
      Just (leader, tg)
        | dep /= leader -> Right (unitNameSet leader)
        | let decls = (group_tyclds tg, group_kisigs tg)
        , term : _ <- localUses value_names decls -> Left (needs_term dep term)
        | all (ordinaryType . unLoc) (group_tyclds tg)
        , not (hasSplice (group_tyclds tg, group_kisigs tg)) ->
            -- group_ext contains type dependencies. A class default method
            -- can also use a local constructor, selector or class method.
            Right (group_ext tg `unionNameSet` mkNameSet
                    [use | L _ decl@ClassDecl{} <- group_tyclds tg, use <- localUses group_names decl])
      _ -> Left ("Cannot check " ++ owner ++ " before the same-group declaration "
                 ++ getOccString dep ++ ".")
    ordinaryType SynDecl{} = True
    ordinaryType FamDecl{} = True
    ordinaryType DataDecl{} = True
    ordinaryType ClassDecl{} = True
    ordinaryType _ = False

    no_deriving (L _ DataDecl{tcdDataDefn = HsDataDefn{dd_derivs = ds}}) =
      all empty_deriving ds
    no_deriving _ = True
    empty_deriving (L _ HsDerivingClause{deriv_clause_tys = L _ (DctMulti _ [])}) = True
    empty_deriving _ = False
tcLocalOpenGroup demands _ _ =
    rejectLocalOpen demands
      "Early checking of local definitions requires renamed value bindings."

{- Note [Deferring a local type instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A standalone type instance can need a local function in its RHS.
Delay an equation only for an open type family declared in the same
group. Keep class instances, data instances, deriving and defaults in
their ordinary early pass. The earlier batches must not check the
family declaration. This excludes references from their signatures,
bodies and instance environments, including transitive references.
Imported or earlier declarations cannot refer to the fresh family.

Keep all same-group equations for that family together and retain each
equation's renamed dependencies. Put the singleton instance groups after
the remaining type groups, in their original order. See Note [Put instances
at the end] in GHC.Rename.Module. The ordinary family instance checker
checks the LHS, RHS and consistency with other equations. The continuation keeps
the needed, solved Core in scope. This split does not move a splice or
change the class instance environment used to check an early producer.
-}

-- | Return the owners of signatures and pragmas relevant to scheduling.
signatureOwners :: Sig GhcRn -> [Name]
signatureOwners (TypeSig _ _ ns _) = map unLoc ns
signatureOwners (InlineSig _ (L _ n) _) = [n]
signatureOwners (SCCFunSig _ (L _ n) _) = [n]
signatureOwners (SpecSig _ (L _ n) _ _) = [n]
signatureOwners (SpecSigE n _ _ _) = [n]
signatureOwners _ = []

-- | Collect the signatures and pragmas that move with each producer.
signaturesByOwner :: [LSig GhcRn] -> NameEnv [Sig GhcRn]
signaturesByOwner = foldr add emptyNameEnv
  where
    add (L _ sig) env =
      foldr (\owner acc -> extendNameEnv_C (++) acc owner [sig]) env (signatureOwners sig)

-- | Select the function bindings that belong to an early group.
selectedBind :: NameSet -> LHsBind GhcRn -> Bool
selectedBind names (L _ FunBind{fun_id = L _ b}) = b `elemNameSet` names
selectedBind _ _ = False

-- | Keep each shared signature with its corresponding set of owners.
splitSignatures :: NameSet -> [LSig GhcRn] -> ([LSig GhcRn], [LSig GhcRn])
splitSignatures selected = foldr split ([], [])
  where
    split sig@(L loc (TypeSig ext mods owners tau)) (early, rest) =
      case partition ((`elemNameSet` selected) . unLoc) owners of
        ([], _) -> (early, sig : rest)
        (_, []) -> (sig : early, rest)
        (before, after) ->
          (L loc (TypeSig ext mods before tau) : early,
           L loc (TypeSig ext mods after tau) : rest)
    split sig@(L _ decl) (early, rest)
      | any (`elemNameSet` selected) (signatureOwners decl) = (sig : early, rest)
      | otherwise = (early, sig : rest)

-- | Desugar only demanded bindings after their owning group has solved.
localOpenDefinitions :: [(Name, SrcSpan)] -> Bag EvBind -> LHsBinds GhcTc
                     -> TcM (NameEnv (Id, CoreExpr), NameEnv Id)
localOpenDefinitions demands evidence binds = do
    let requested = mkNameSet (map fst demands)
        named_binds =
          [ (mkNameSet (map idName owners), bind)
          | bind <- binds
          , let owners = collectHsBindBinders CollNoDictBinders (unLoc bind)
          , not (null owners) ]
        local_names = unionNameSets (map fst named_binds)
        by_owner = mkNameEnv
          [ (owner, bind) | (owners, bind) <- named_binds, owner <- nameSetElemsStable owners ]
        dependencies name = do
          case lookupNameEnv by_owner name of
            Just bind -> Right (mkNameSet (localOpenReferences bind) `intersectNameSet` local_names)
            Nothing -> Left "No checked Core definition is available for this call."
    selected <- case closeLocalNames dependencies emptyNameSet (nameSetElemsStable requested) of
      Right names -> pure names
      Left reason -> rejectLocalOpen demands reason
    let relevant_binds = [(owners, bind) | (owners, bind) <- named_binds
                                       , intersectsNameSet owners selected]
        relevant = map snd relevant_binds
        definition_names = unionNameSets (map fst relevant_binds)
    when (any hasPendingTypedSplice relevant) $
      rejectLocalOpen demands "This earlier definition contains a pending typed splice."
    (_, _, zonked, _, _, _, _) <- zonkTopDecls evidence relevant [] [] [] []
    (messages, proposal) <- initDsTc (dsTopLHsBinds (concatMap singleExport zonked))
    let owners = mkNameEnv (concatMap monoOwners zonked)
        definitions = mkNameEnv
          [ (idName owner, (owner, rhs))
          | core <- maybeToList proposal, not (errorsFound messages)
          , (owner, rhs) <- fromOL core
          , idName owner `elemNameSet` definition_names
          , exprType rhs `eqType` idType owner ]
        missing = filter (not . (`elemNameEnv` definitions) . fst) demands
    unless (null missing) $
      rejectLocalOpen missing "No checked Core definition is available for this call."
    pure (definitions, owners)
  where
    singleExport :: LHsBind GhcTc -> LHsBinds GhcTc
    singleExport (L loc (XHsBindsLR (abs_binds@AbsBinds{abs_exports = exports@(_ : _ : _)}))) =
      [L loc (XHsBindsLR (abs_binds { abs_exports = [owner] })) | owner <- exports]
    singleExport bind = [bind]

    monoOwners :: LHsBind GhcTc -> [(Name, Id)]
    monoOwners (L _ (XHsBindsLR AbsBinds{abs_exports = exports})) =
      [(idName mono, poly) | ABE{abe_mono = mono, abe_poly = poly} <- exports]
    monoOwners _ = []

    -- zonkExpr executes a pending typed splice. The ordinary module
    -- zonk still owns this operation. Observation must not execute it.
    hasPendingTypedSplice :: Data a => a -> Bool
    hasPendingTypedSplice node = case cast node :: Maybe (HsExpr GhcTc) of
      Just HsTypedSplice{} -> True
      _ -> or (gmapQ hasPendingTypedSplice node)

-- | Close the requested names over the eligible bindings in the graph.
closeLocalNames :: Monad m => (Name -> m NameSet) -> NameSet -> [Name] -> m NameSet
closeLocalNames dependencies = foldM visit
  where
    visit checked name
      | name `elemNameSet` checked = pure checked
      | otherwise = do
          uses <- dependencies name
          foldM visit (extendNameSet checked name) (nameSetElemsStable uses)

-- | Collect resolved term references from one checked binding.
localOpenReferences :: Data a => a -> [Name]
localOpenReferences node = case cast node :: Maybe (HsExpr GhcTc) of
    Just (HsVar _ (L _ v)) -> [idName v]
    Just HsTypedSplice{} -> []
    _ -> concat (gmapQ localOpenReferences node)

-- | Confine the checked definitions to the current typechecking continuation.
withLocalOpenDefinitions :: (NameEnv (Id, CoreExpr), NameEnv Id)
                         -> TcM (TcGblEnv, TcLclEnv) -> TcM (TcGblEnv, TcLclEnv)
withLocalOpenDefinitions (definitions, owners) check = do
    env <- getGblEnv
    let old_defs = tcg_local_open_defs env
        old_owners = tcg_local_open_owners env
    (checked_env, checked_lcl) <- setGblEnv
      (env { tcg_local_open_defs = old_defs `plusNameEnv` definitions
           , tcg_local_open_owners = old_owners `plusNameEnv` owners }) check
    pure (checked_env { tcg_local_open_defs = old_defs
                      , tcg_local_open_owners = old_owners }, checked_lcl)

-- | Retain resolved external references that the local SCC graph omits.
externalDeclUses :: Data a => a -> NameSet
externalDeclUses node
  | Just name <- cast node =
      if isExternalName name && (isTyConName name || isDataConName name || isVarName name)
      then unitNameSet name else emptyNameSet
  | otherwise = unionNameSets (gmapQ externalDeclUses node)

-- | Find resolved local names in a renamed declaration or signature.
localUses :: Data a => NameSet -> a -> [Name]
localUses names node
  | Just name <- cast node = [name | name `elemNameSet` names]
  | otherwise = concat (gmapQ (localUses names) node)

hasSplice :: Data a => a -> Bool
hasSplice node =
    isJust (cast node :: Maybe (HsTypedSplice GhcRn))
    || isJust (cast node :: Maybe (HsUntypedSplice GhcRn))
    || or (gmapQ hasSplice node)

rejectLocalOpen :: [(Name, SrcSpan)] -> String -> TcM a
rejectLocalOpen demands reason = do
    for_ demands $ \(name, loc) ->
      setSrcSpan loc (addErr (TcRnOpenTypeEval name Nothing reason))
    failM

-- | Solve independent source functions before retained calls in the same value group.
-- See Note [Ordering local source tables] in GHC.Rename.Module.
tcSourceValueBinds :: HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
tcSourceValueBinds group = do
    enabled <- xoptM LangExt.ExperimentalDependentTypes
    env <- getGblEnv
    case if enabled then splitLocalSourceTables TableInTerm (tcg_dus env) group else Nothing of
      Nothing -> check_group group
      Just (_, first, rest) -> do
        (envs@(first_env, _), wanteds) <- captureTopConstraints (check_group first)
        restoreEnvs envs $ do
          evs <- simplifyTop wanteds
          let names = mkNameSet (collectHsValBinders CollNoDictBinders (hs_valds first))
          tables <- sourceTablesFor False names first_env
          addTopEvBinds evs $ do
            env' <- installSourceTables tables
            setGblEnv env' (check_group rest)
  where
    check_group :: HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
    check_group HsGroup{hs_valds = XValBindsLR (HsVBG binds sigs)} = tcTopBinds binds sigs
    check_group _ = panic "tcSourceValueBinds: expected renamed value bindings"

tcTopSrcDeclGroup :: HsGroup GhcRn -> TcM (TcGblEnv, TcLclEnv)
tcTopSrcDeclGroup group = do
    (envs, _, _) <- tcTopSrcDeclGroupWithMethods False (Just []) group
    pure envs

-- | Check a source batch. If deriving info is absent, save the attached
-- deriving clauses for a later batch. The instance method pass can also wait.
tcTopSrcDeclGroupWithMethods :: Bool -> Maybe [DerivInfo] -> HsGroup GhcRn
  -> TcM ((TcGblEnv, TcLclEnv), Maybe ([LTyClDecl GhcRn], [InstInfo GhcRn]), [DerivInfo])
tcTopSrcDeclGroupWithMethods defer_methods previous_deriving group@(HsGroup { hs_tyclds = tycl_decls,
                         hs_derivds = deriv_decls,
                         hs_fords  = foreign_decls,
                         hs_defds  = default_decls,
                         hs_annds  = annotation_decls,
                         hs_ruleds = rule_decls,
                         hs_valds  = hs_val_binds@(XValBindsLR
                                              (HsVBG val_binds val_sigs)) })
 = do {         -- Type-check the type and class decls, and all imported decls
                -- The latter come in via tycl_decls
        traceTc "Tc2 (src)" empty ;

                -- Source-language instances, including derivings,
                -- and import the supporting declarations
        traceTc "Tc3" empty ;
        (tcg_env, inst_infos, th_bndrs,
         XValBindsLR (HsVBG deriv_binds deriv_sigs), deferred_deriving)
            <- tcTyClsInstDecls previous_deriving tycl_decls deriv_decls default_decls val_binds ;

        updLclCtxt (\tcl_env -> tcl_env { tcl_th_bndrs = th_bndrs `plusNameEnv` tcl_th_bndrs tcl_env }) $
        setGblEnv tcg_env       $ do {

                -- Foreign import declarations next.
        traceTc "Tc4" empty ;
        (fi_ids, fi_decls, fi_gres) <- tcForeignImports foreign_decls ;
        tcExtendGlobalValEnv fi_ids     $ do {

                -- Value declarations next.
                -- It is important that we check the top-level value bindings
                -- before the GHC-generated derived bindings, since the latter
                -- may be defined in terms of the former. (For instance,
                -- the bindings produced in a Data instance.)
        traceTc "Tc5" empty ;
        tc_envs <- tcSourceValueBinds group;
        restoreEnvs tc_envs $ do {

                -- Now GHC-generated derived bindings, generics, and selectors
                -- Do not generate warnings from compiler-generated code;
                -- hence the use of discardWarnings
        tc_envs@(tcg_env, tcl_env)
            <- discardWarnings (tcTopBinds deriv_binds deriv_sigs) ;
        restoreEnvs tc_envs $ do {  -- Environment doesn't change now

                -- Second pass over class and instance declarations,
                -- now using the kind-checked decls
        traceTc "Tc6" empty ;
        let { method_classes = tyClGroupTyClDecls tycl_decls } ;
        (inst_binds, pending_methods) <- if defer_methods
          then pure ([], Just (method_classes, inst_infos))
          else (, Nothing) <$> tcInstDecls2 method_classes inst_infos ;

                -- Foreign exports
        traceTc "Tc7" empty ;
        (foe_binds, foe_decls, foe_gres) <- tcForeignExports foreign_decls ;

                -- Annotations
        annotations <- tcAnnotations annotation_decls ;

                -- Rules
        rules <- tcRules rule_decls ;

                -- Wrap up
        traceTc "Tc7a" empty ;
        let { all_binds = inst_binds ++ foe_binds

            ; fo_gres = fi_gres `unionBags` foe_gres
            ; fo_fvs = foldr (\gre fvs -> fvs `addOneFN` (greName gre))
                                emptyFNs fo_gres

            ; sig_names = mkNameSet (collectHsValBinders CollNoDictBinders hs_val_binds)
                          `minusNameSet` getTypeSigNames val_sigs

                -- Extend the GblEnv with the (as yet un-zonked)
                -- bindings, rules, foreign decls
            ; tcg_env' = tcg_env { tcg_binds   = tcg_binds tcg_env ++ all_binds
                                 , tcg_sigs    = tcg_sigs tcg_env `unionNameSet` sig_names
                                 , tcg_rules   = tcg_rules tcg_env
                                                      ++ flattenRuleDecls rules
                                 , tcg_anns    = tcg_anns tcg_env ++ annotations
                                 , tcg_ann_env = extendAnnEnvList (tcg_ann_env tcg_env) annotations
                                 , tcg_fords   = tcg_fords tcg_env ++ foe_decls ++ fi_decls
                                 , tcg_dus     = tcg_dus tcg_env `plusDU` usesOnly fo_fvs } } ;
                                 -- tcg_dus: see Note [Newtype constructor usage in foreign declarations]

        -- See Note [Newtype constructor usage in foreign declarations]
        addUsedGREs NoDeprecationWarnings (bagToList fo_gres) ;

        return ((tcg_env', tcl_env), pending_methods, deferred_deriving)
    }}}}}

tcTopSrcDeclGroupWithMethods _ _ _ = panic "tcTopSrcDeclGroup: ValBindsIn"

---------------------------
tcTyClsInstDecls :: Maybe [DerivInfo]  -- Nothing defers attached deriving
                 -> [TyClGroup GhcRn]
                 -> [LDerivDecl GhcRn]
                 -> [LDefaultDecl GhcRn]
                 -> [(RecFlag, LHsBinds GhcRn)]
                 -> TcM (TcGblEnv,            -- The full inst env
                         [InstInfo GhcRn],    -- Source-code instance decls to
                                              -- process; contains all dfuns for
                                              -- this module
                          ThBindEnv,          -- TH binding levels
                          HsValBinds GhcRn,  -- Supporting bindings for derived instances
                         [DerivInfo])        -- Attached deriving for a later batch

tcTyClsInstDecls previous_deriving tycl_decls deriv_decls default_decls binds
 = tcAddDataFamConPlaceholders (tycl_decls >>= group_instds) $
   tcAddPatSynPlaceholders (getPatSynBinds binds) $
   do { (tcg_env, inst_info, deriv_info, th_bndrs)
          <- tcTyAndClassDecls tycl_decls ;

      ; setGblEnv tcg_env $ do {

          -- With the @TyClDecl@s and @InstDecl@s checked we're ready to
          -- process the deriving clauses, including data family deriving
          -- clauses discovered in @tcTyAndClassDecls@.
          --
          -- But only after we've typechecked 'default' declarations.
          -- See Note [Typechecking default declarations]
          defaults <- tcDefaultDecls default_decls
          ; extendDefaultEnvWithLocalDefaults defaults $ do {

          -- Careful to quit now in case there were instance errors, so that
          -- the deriving errors don't pile up as well.
          ; failIfErrsM
          ; case previous_deriving of
              Nothing -> pure (tcg_env, inst_info, th_bndrs, emptyValBindsRn, deriv_info)
              Just earlier -> do
                (tcg_env', inst_info', val_binds)
                    <- tcInstDeclsDeriv (deriv_info ++ earlier) deriv_decls
                setGblEnv tcg_env' $ do
                  failIfErrsM
                  pure (tcg_env', inst_info' ++ inst_info, th_bndrs, val_binds, [])
      }}}

{- *********************************************************************
*                                                                      *
        Checking for 'main'
*                                                                      *
************************************************************************
-}

-- | A module consisting solely of wired-in declarations must not define
-- instances or rules: GHC never loads its interface to look for them.
-- See Note [Loading instances for wired-in things] in GHC.Iface.Load.
checkWiredInOnlyModule :: TcGblEnv -> TcM ()
checkWiredInOnlyModule tcg_env
  | isWiredInOnlyModule (tcg_mod tcg_env)
  , not (null (tcg_insts tcg_env) && null (tcg_fam_insts tcg_env) && null (tcg_rules tcg_env))
  = pprPanic "checkWiredInOnlyModule" $
      vcat [ ppr (tcg_mod tcg_env) <+> text "must not declare instances or rules."
           , text "See Note [Loading instances for wired-in things] in GHC.Iface.Load." ]
  | otherwise
  = return ()

checkMainType :: TcGblEnv -> TcRn WantedConstraints
-- If this is the Main module, and it defines a function main,
--   check that its type is of form IO tau.
-- If not, do nothing
-- See Note [Dealing with main]
checkMainType tcg_env
  = do { hsc_env <- getTopEnv
       ; if tcg_mod tcg_env /= mainModIs (hsc_HUE hsc_env)
         then return emptyWC else

    do { rdr_env <- getGlobalRdrEnv
       ; let dflags    = hsc_dflags hsc_env
             main_occ  = getMainOcc dflags
             main_gres = lookupGRE rdr_env (LookupOccName main_occ SameNameSpace)
       ; case filter isLocalGRE main_gres of {
            []         -> return emptyWC ;
            (_:_:_)    -> return emptyWC ;
            [main_gre] ->

    do { let main_name = greName main_gre
             ctxt      = FunSigCtxt main_name NoRRC
       ; main_id   <- tcLookupId main_name
       ; (io_ty,_) <- getIOType
       ; let main_ty   = idType main_id
             eq_orig   = TypeEqOrigin { uo_actual    = main_ty
                                      , uo_expected  = io_ty
                                      , uo_thing     = Nothing
                                      , uo_invisible = Nothing }
       ; (_, lie)  <- captureTopConstraints       $
                      setMainCtxt main_name io_ty $
                      tcSubTypeSigma eq_orig ctxt main_ty io_ty
       ; return lie } } } }

checkMain :: Bool              -- False => no 'module M(..) where' header at all
          -> Maybe [LIE GhcPs] -- Export specs of Main module
          -> TcM TcGblEnv
-- If we are in module Main, check that 'main' is exported,
-- and generate the runMainIO binding that calls it
-- See Note [Dealing with main]
checkMain explicit_mod_hdr export_ies
 = do { hsc_env  <- getTopEnv
      ; tcg_env <- getGblEnv

      ; let dflags      = hsc_dflags hsc_env
            main_mod    = mainModIs (hsc_HUE hsc_env)
            main_occ    = getMainOcc dflags

            exported_mains :: [Name]
            -- Exported things that are called 'main'
            exported_mains  = [ name | avail <- tcg_exports tcg_env
                                     , name  <- availNames avail
                                     , nameOccName name == main_occ ]

      ; if | tcg_mod tcg_env /= main_mod
           -> -- Not the main module
              return tcg_env

           | [main_name] <- exported_mains
           -> -- The module indeed exports a function called 'main'
              generateMainBinding tcg_env main_name

           | otherwise
           -> assert (null exported_mains) $
              -- A fully-checked export list can't contain more
              -- than one function with the same OccName
              do { complain_no_main dflags main_mod main_occ
                 ; return tcg_env } }
  where
    complain_no_main dflags main_mod main_occ
      = unless (interactive && not explicit_mod_hdr) $
        addErrTc (noMainMsg main_mod main_occ)          -- #12906
      where
        interactive = ghcLink dflags == LinkInMemory
        -- Without an explicit module header...
        -- in interactive mode, don't worry about the absence of 'main'.
        -- in other modes, add error message and go on with typechecking.

    noMainMsg main_mod main_occ
      = TcRnMissingMain explicit_export_list main_mod main_occ
    explicit_export_list = explicit_mod_hdr && isJust export_ies

-- | Get the unqualified name of the function to use as the \"main\" for the main module.
-- Either returns the default name or the one configured on the command line with -main-is
getMainOcc :: DynFlags -> OccName
getMainOcc dflags = case mainFunIs dflags of
                      Just fn -> mkVarOccFS (mkFastString fn)
                      Nothing -> mkVarOccFS (fsLit "main")

generateMainBinding :: TcGblEnv -> Name -> TcM TcGblEnv
-- There is a single exported 'main' function, called 'foo' (say),
-- which may be locally defined or imported
-- Define and typecheck the binding
--     :Main.main :: IO res_ty = runMainIO res_ty foo
-- This wraps the user's main function in the top-level stuff
-- defined in runMainIO (eg catching otherwise un-caught exceptions)
-- See Note [Dealing with main]
generateMainBinding tcg_env main_name = do
    { traceTc "checkMain found" (ppr main_name)
    ; (io_ty, res_ty) <- getIOType
    ; let loc = getSrcSpan main_name
          main_expr_rn = L (noAnnSrcSpan loc) (mkHsVar (L (noAnnSrcSpan loc) main_name))
    ; (ev_binds, main_expr) <- setMainCtxt main_name io_ty $
                               tcCheckMonoExpr main_expr_rn io_ty

            -- See Note [Root-main Id]
            -- Construct the binding
            --      :Main.main :: IO res_ty = runMainIO res_ty main
    ; run_main_id <- tcLookupKnownOccId runMainIOOcc
    ; let { root_main_name =  mkExternalName rootMainKey rOOT_MAIN
                               (mkVarOccFS (fsLit "main"))
                               (getSrcSpan main_name)
          ; root_main_id = Id.mkExportedVanillaId root_main_name io_ty
          ; co  = mkWpTyApps [res_ty]
          -- The ev_binds of the `main` function may contain deferred
          -- type errors when type of `main` is not `IO a`. The `ev_binds`
          -- must be put inside `runMainIO` to ensure the deferred type
          -- error can be emitted correctly. See #13838.
          ; rhs = nlHsApp (mkLHsWrap co (nlHsVar run_main_id)) $
                    mkHsDictLet ev_binds main_expr
          ; main_bind = mkVarBind root_main_id rhs }

    ; return (tcg_env { tcg_main  = Just main_name
                      , tcg_binds = tcg_binds tcg_env
                                    ++ [main_bind]
                      , tcg_dus   = tcg_dus tcg_env
                                    `plusDU` usesOnly (unitFN main_name) })
                    -- Record the use of 'main', so that we don't
                    -- complain about it being defined but not used
    }

getIOType :: TcM (TcType, TcType)
-- Return (IO alpha, alpha) for fresh alpha
getIOType = do { ioTyCon <- tcLookupKnownKeyTyCon ioTyConKey
               ; res_ty <- newFlexiTyVarTy liftedTypeKind
               ; return (mkTyConApp ioTyCon [res_ty], res_ty) }

setMainCtxt :: Name -> TcType -> TcM a -> TcM (TcEvBinds, a)
setMainCtxt main_name io_ty thing_inside
  = setSrcSpan (getSrcSpan main_name) $
    addErrCtxt (MainCtxt main_name)   $
    checkConstraints skol_info [] []  $  -- Builds an implication if necessary
    thing_inside                         -- e.g. with -fdefer-type-errors
  where
    skol_info = SigSkol (FunSigCtxt main_name NoRRC) io_ty []

{- Note [Dealing with main]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dealing with the 'main' declaration is surprisingly tricky. Here are
the moving parts:

* The flag -main-is=M.foo allows you to set the main module to 'M',
  and the main function to 'foo'.  We access them through
      mainModIs  :: HscEnv -> Module     -- returns M
      getMainOcc :: DynFlags -> OccName  -- returns foo
  Of course usually M = Main, and foo = main.

* checkMainType: when typechecking module M, we add an extra check that
    foo :: IO tau, for some type tau.
  This avoids getting ambiguous-type errors from the monomorphism restriction
  applying to things like
      main = return ()
  Note that checkMainType does not consult the export list because
  we have not yet done rnExports (and can't do it until later).

* rnExports: checks the export list.  Very annoyingly, we can only do
  this after running any finalisers, which may add new declarations.
  That's why checkMainType and checkMain have to be separate.

* checkMain: does two things:
  - check that the export list does indeed export something called 'foo'
  - generateMainBinding: generate the root-main binding
       :Main.main = runMainIO M.foo
  See Note [Root-main Id]

An annoying consequence of having both checkMainType and checkMain is
that, when (but only when) -fdefer-type-errors is on, we may report an
ill-typed 'main' twice (as warnings): once in checkMainType and once
in checkMain. See test typecheck/should_fail/T13292.

We have the following tests to check this processing:
----------------+----------------------------------------------------------------------------------+
                |                                  Module Header:                                  |
                +-------------+-------------+-------------+-------------+-------------+------------+
                | module      | module Main | <No Header> | module Main |module       |module Main |
                |  Main(main) |             |             |   (module X)|   Main ()   |  (Sub.main)|
----------------+==================================================================================+
`main` function | ERROR:      | Main.main   | ERROR:      | Main.main   | ERROR:      | Sub.main   |
in Main module  |  Ambiguous  |             |  Ambiguous  |             |  `main` not |            |
and in imported |             |             |             |             |  exported   |            |
module Sub.     | T19397E1    | T16453M0    | T19397E2    | T16453M3    |             | T16453M1   |
                |             |             |             | X = Main    | Remark 2)   |            |
----------------+-------------+-------------+-------------+-------------+-------------+------------+
`main`function  | Sub.main    | ERROR:      | Sub.main    | Sub.main    | ERROR:      | Sub.main   |
only in imported|             | No `main` in|             |             |  `main` not |            |
submodule Sub.  |             |   `Main`    |             |             |  exported   |            |
                | T19397M0    | T16453E1    | T19397M1    | T16453M4    |             | T16453M5   |
                |             |             |             | X = Sub     | Remark 2)   |            |
----------------+-------------+-------------+-------------+-------------+-------------+------------+
`foo` function  | Sub.foo     | ERROR:      | Sub.foo     | Sub.foo     | ERROR:      | Sub.foo    |
in submodule    |             | No `foo` in |             |             |  `foo` not  |            |
Sub.            |             |   `Main`    |             |             |  exported   |            |
GHC option:     |             |             |             |             |             |            |
  -main-is foo  | T19397M2    | T19397E3    | T19397M3    | T19397M4    | T19397E4    | T16453M6   |
                | Remark 1)   |             |             | X = Sub     |             | Remark 3)  |
----------------+-------------+-------------+-------------+-------------+-------------+------------+

Remarks:
* The first line shows the exported `main` function or the error.
* The second line shows the coresponding test case.
* The module `Sub` contains the following functions:
     main :: IO ()
     foo :: IO ()
* Remark 1) Here the header is `Main (foo)`.
* Remark 2) Here we have no extra test case. It would exercise the same code path as `T19397E4`.
* Remark 3) Here the header is `Main (Sub.foo)`.


Note [Root-main Id]
~~~~~~~~~~~~~~~~~~~
The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)

This is unusual: it's a LocalId whose Name has a Module from another
module. Tiresomely, we must filter it out again in GHC.Iface.Make, less we
get two defns for 'main' in the interface file!

When using `-fwrite-if-simplified-core` the root_main_id can end up in an interface file.
When the interface is read back in we have to add a special case when creating the
Id because otherwise we would go looking for the :Main module which obviously doesn't
exist. For this logic see GHC.IfaceToCore.mk_top_id.

There is also some similar (probably dead) logic in GHC.Rename.Env which says it
was added for External Core which faced a similar issue.

Note [runTcInteractive module graph]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `withInteractiveModuleNode` function sets up the module graph which contains
the interactive module used by `runTcInteractive`.

The module graph is essentially the ambient module graph which is set up when
ghci loads a module using `load`, with the addition of the interactive module (Ghci<N>),
which imports the parts specified by the `InteractiveImports`.

Therefore `downsweepInteractiveImports` presumes that any import which is
determined to be from the home module is already present in the module graph.
This saves resummarising and performing the whole downsweep again if it's already been
done.

On the other hand, when GHCi starts up, and no modules have been loaded yet, the
module graph will be empty. Therefore `downsweepInteractiveImports` will perform
for the unit portion of the graph, if it's not already been performed.


Note [Interactive family instance visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The home family instance environment also contains instances from modules
that GHCi has loaded but the prompt has not imported. Collect the family
instance modules from each interactive import, just as rnImports does for
a source module. Include instances from earlier prompt declarations. A
type numeral in Vec Bool (n + 1) can then use a visible FromNatural instance.
See Note [Registered type numerals] in GHC.Tc.Gen.HsType.

*********************************************************
*                                                       *
                GHCi stuff
*                                                       *
*********************************************************
-}

-- See Note [runTcInteractive module graph]
withInteractiveModuleNode :: HscEnv -> TcM a -> TcM a
withInteractiveModuleNode hsc_env thing_inside = do
  mg <- liftIO $ downsweepInteractiveImports hsc_env (hsc_IC hsc_env)
  -- The interactive module is not defined by a file, so the known home modules
  -- are unchanged.
  updTopEnv (\ env -> setModuleGraph (hsc_known_home_modules env) mg env) thing_inside

runTcInteractive
  :: TcMPluginHandling
  -> HscEnv
  -> TcRn a
  -> IO (Messages TcRnMessage, Maybe a)
-- ^ Initialise the tcg_inst_env with instances from all home modules.
-- This mimics the more selective call to hptInstances in tcRnImports
runTcInteractive tcm_plugin_handling hsc_env thing_inside
  = initTcInteractive tcm_plugin_handling hsc_env $
    withInteractiveModuleNode hsc_env $
    do { traceTc "setInteractiveContext" $
            vcat [ text "ic_tythings:" <+> vcat (map ppr (ic_tythings icxt))
                 , text "ic_insts:" <+> vcat (map (pprBndr LetBind . instanceDFunId) (instEnvElts ic_insts))
                 , text "icReaderEnv (LocalDef)" <+>
                      vcat (map ppr [ local_gres | gres <- nonDetOccEnvElts (icReaderEnv icxt)
                                                 , let local_gres = filter isLocalGRE gres
                                                 , not (null local_gres) ]) ]

       ; !(orphs, finsts) <- fmap (force . mconcat) . forM (ic_imports icxt) $ \i -> do
             -- Force the result to release the imported interfaces. See #15111.
             iface <- case i of
               IIModule m -> loadModuleInterface (text "runTcInteractive") m
               IIDecl decl -> do
                 let m = hsModuleName (unLoc (ideclName decl))
                     pkg = renameRawPkgQual (hsc_unit_env hsc_env) m (ideclPkgQual decl)
                 loadSrcInterface (text "runTcInteractive") LookupUser m NotBoot pkg
             pure (mi_module iface : dep_orphs (mi_deps iface), importedFamInstModules iface)

       ; (home_insts, home_fam_insts) <- liftIO $ UnitEnv.hugAllInstances (hsc_unit_env hsc_env)

       ; let prompt_finsts = map (nameModule . getName) ic_finsts
             imports = emptyImportAvails { imp_orphs = orphs
                                         , imp_finsts = moduleSetElts (mkModuleSet (prompt_finsts ++ finsts)) }

             upd_envs (gbl_env, lcl_env) = (gbl_env', lcl_env')

               where
                 gbl_env' = gbl_env
                   { tcg_rdr_env      = icReaderEnv icxt
                   , tcg_type_env     = type_env
                   , tcg_inst_env     = tcg_inst_env gbl_env `unionInstEnv` ic_insts `unionInstEnv` home_insts
                   , tcg_fam_inst_env = extendFamInstEnvList
                              (extendFamInstEnvList (tcg_fam_inst_env gbl_env)
                                                    ic_finsts)
                              home_fam_insts
                   , tcg_fix_env      = ic_fix_env icxt
                   , tcg_default      = ic_default icxt
                        -- See Note [Interactive family instance visibility].
                   , tcg_imports      = imports }

                 lcl_env' = modifyLclCtxt (tcExtendLocalTypeEnv lcl_ids) lcl_env

       ; updEnvs upd_envs thing_inside }
  where
    icxt = hsc_IC hsc_env
    (ic_insts, ic_finsts) = ic_instances icxt
    (lcl_ids, top_ty_things) = partitionWith is_closed (ic_tythings icxt)

    is_closed :: TyThing -> Either (Name, TcTyThing) TyThing
    -- Put Ids with free type variables (always RuntimeUnks)
    -- in the *local* type environment
    -- See Note [Initialising the type environment for GHCi]
    is_closed thing
      | AnId id <- thing
      , not (isTypeClosedLetBndr id)
      = Left (idName id, ATcId { tct_id = id
                               , tct_info = NotLetBound })
      | otherwise
      = Right thing

    type_env1 = mkTypeEnvWithImplicits top_ty_things
    type_env  = extendTypeEnvWithIds type_env1
              $ map instanceDFunId (instEnvElts ic_insts)
                -- Putting the dfuns in the type_env
                -- is just to keep Core Lint happy

{- Note [Initialising the type environment for GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the Ids in ic_things, defined by the user in 'let' stmts,
have closed types. E.g.
   ghci> let foo x y = x && not y

However the GHCi debugger creates top-level bindings for Ids whose
types have free RuntimeUnk skolem variables, standing for unknown
types.  If we don't register these free TyVars as global TyVars then
the typechecker will try to quantify over them and fall over in
skolemiseQuantifiedTyVar. so we must add any free TyVars to the
typechecker's global TyVar set.  That is done by using
tcExtendLocalTypeEnv.

We do this by splitting out the Ids with open types, using 'is_closed'
to do the partition.  The top-level things go in the global TypeEnv;
the open, NotTopLevel, Ids, with free RuntimeUnk tyvars, go in the
local TypeEnv.

Note that we don't extend the local RdrEnv (tcl_rdr); all the in-scope
things are already in the interactive context's GlobalRdrEnv.
Extending the local RdrEnv isn't terrible, but it means there is an
entry for the same Name in both global and local RdrEnvs, and that
lead to duplicate "perhaps you meant..." suggestions (e.g. T5564).

We don't bother with the tcl_th_bndrs environment either.
-}

-- | The returned [Id] is the list of new Ids bound by this statement. It can
-- be used to extend the InteractiveContext via extendInteractiveContext.
--
-- The returned TypecheckedHsExpr is of type IO [ Any ], a list of the bound
-- values, coerced to Any.
tcRnStmt :: HscEnv -> GhciLStmt GhcPs
         -> IO (Messages TcRnMessage, Maybe ([Id], LHsExpr GhcTc, FixityEnv))
tcRnStmt hsc_env rdr_stmt
  = runTcInteractive StartAndStopTcMPlugins hsc_env $ do {
    -- Don't bother to keep TcM plugins running, as the consumer of
    -- the output of 'tcRnStmt' is 'deSugarExpr' which has no need to
    -- invoke 'TcM' plugins, unlike Note [Stop TcM plugins after desugaring] in GHC.Driver.Main.

    -- The real work is done here
    ((bound_ids, tc_expr), fix_env) <- tcUserStmt rdr_stmt ;
    zonked_expr <- zonkTopLExpr tc_expr ;
    zonked_ids  <- zonkTopBndrs bound_ids ;
    env <- getGblEnv ;
    checkOptionalSourceUses env ;

    failIfErrsM ;  -- we can't do the next step if there are
                   -- representation polymorphism errors
                   -- test case: ghci/scripts/T13202{,a}

        -- None of the Ids should be of unboxed type, because we
        -- cast them all to HValues in the end!
    mapM_ (addErr . TcRnGhciUnliftedBind) $
      filter (mightBeUnliftedType . idType) zonked_ids ;

    traceTc "tcs 1" empty ;
    this_mod <- getModule ;
    global_ids <- mapM (externaliseAndTidyId this_mod) zonked_ids ;
        -- Note [Interactively-bound Ids in GHCi] in GHC.Driver.Env

    traceOptTcRn Opt_D_dump_tc
        (vcat [text "Bound Ids" <+> pprWithCommas ppr global_ids,
               text "Typechecked expr" <+> ppr zonked_expr]) ;

    return (global_ids, zonked_expr, fix_env)
    }

{-
--------------------------------------------------------------------------
                Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]

        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]

        expr (of non-IO type,
          result not showable)  ==>     error
-}

-- | A plan is an attempt to lift some code into the IO monad.
type PlanResult = ([Id], LHsExpr GhcTc)
type Plan = TcM PlanResult

-- | Try the plans in order. If one fails (by raising an exn), try the next.
-- If one succeeds, take it.
runPlans :: NonEmpty Plan -> Plan
runPlans = foldr1 (flip tryTcDiscardingErrs)

-- | Typecheck (and 'lift') a stmt entered by the user in GHCi into the
-- GHCi 'environment'.
--
-- By 'lift' and 'environment we mean that the code is changed to
-- execute properly in an IO monad. See Note [Interactively-bound Ids
-- in GHCi] in GHC.Driver.Env for more details. We do this lifting by trying
-- different ways ('plans') of lifting the code into the IO monad and
-- type checking each plan until one succeeds.
tcUserStmt :: GhciLStmt GhcPs -> TcM (PlanResult, FixityEnv)

-- An expression typed at the prompt is treated very specially
tcUserStmt (L loc (BodyStmt _ expr _ _))
  = do  { (rn_expr, fvs) <- checkNoErrs (rnLExpr expr)

        ; dumpOptTcRn Opt_D_dump_rn_ast "Renamer" FormatHaskell
            (showAstData NoBlankSrcSpan NoBlankEpAnnotations rn_expr)
               -- Don't try to typecheck if the renamer fails!
        ; ghciStep <- getGhciStepIO
        ; uniq <- newUnique
        ; let loc' = noAnnSrcSpan $ locA loc
        ; interPrintName <- getInteractivePrintName
        ; bindIOName     <- rnLookupKnownOccName bindIOIdOcc
        ; thenIOName     <- rnLookupKnownOccName thenIOIdOcc
        ; let fresh_it  = itName uniq (locA loc)
              matches   = [mkMatch (mkPrefixFunRhs (L loc' fresh_it) noAnn) (noLocA []) rn_expr
                                   (noLocA emptyLocalBinds)]
              -- [it = expr]
              the_bind  = L loc $ (mkTopFunBind FromSource
                                     (L loc' fresh_it) matches)
                                         { fun_ext = fvs }
              -- Care here!  In GHCi the expression might have
              -- free variables, and they in turn may have free type variables
              -- (if we are at a breakpoint, say).  We must put those free vars

              -- [let it = expr]
              let_stmt  = L loc $ LetStmt noAnn $ noLocA $ HsValBinds noAnn
                           $ XValBindsLR
                               (HsVBG [(NonRecursive,[the_bind])] [])

              -- [it <- e]
              bind_stmt = L loc $ BindStmt
                                       (XBindStmtRn
                                          { xbsrn_bindOp = mkRnSyntaxExpr bindIOName
                                          , xbsrn_failOp = Nothing
                                          })
                                       (L loc (VarPat noExtField (L loc' fresh_it)))
                                       (nlHsApp ghciStep rn_expr)

              -- [; print it]
              print_it  = L loc $ BodyStmt noExtField
                                           (nlHsApp (nlHsVar interPrintName)
                                           (nlHsVar fresh_it))
                                           (mkRnSyntaxExpr thenIOName)
                                                  noSyntaxExpr

              -- NewA
              no_it_a = L loc $ BodyStmt noExtField (nlHsApps bindIOName
                                       [rn_expr , nlHsVar interPrintName])
                                       (mkRnSyntaxExpr thenIOName)
                                       noSyntaxExpr

              no_it_b = L loc $ BodyStmt noExtField (rn_expr)
                                       (mkRnSyntaxExpr thenIOName)
                                       noSyntaxExpr

              no_it_c = L loc $ BodyStmt noExtField
                                      (nlHsApp (nlHsVar interPrintName) rn_expr)
                                      (mkRnSyntaxExpr thenIOName)
                                      noSyntaxExpr

              -- See Note [GHCi Plans]

              it_plans =
                    -- Plan A
                    do { stuff@([it_id], _) <- tcGhciStmts [bind_stmt, print_it]
                       ; it_ty <- liftZonkM $ zonkTcType (idType it_id)
                       ; when (isUnitTy it_ty) failM
                       ; return stuff } :|

                        -- Plan B; a naked bind statement
                  [ tcGhciStmts [bind_stmt]

                        -- Plan C; check that the let-binding is typeable all by itself.
                        -- If not, fail; if so, try to print it.
                        -- The two-step process avoids getting two errors: one from
                        -- the expression itself, and one from the 'print it' part
                        -- This two-step story is very clunky, alas
                  , do { _ <- checkNoErrs (discardWarnings (tcGhciStmts [let_stmt]))
                                --- checkNoErrs defeats the error recovery of let-bindings
                                --- discardWarnings: warnings come from the second typecheck
                       ; tcGhciStmts [let_stmt, print_it] } ]

              -- Plans where we don't bind "it"
              no_it_plans =
                tcGhciStmts [no_it_a] :|
                tcGhciStmts [no_it_b] :
                tcGhciStmts [no_it_c] :
                []

        ; generate_it <- goptM Opt_NoIt

        -- We disable `-fdefer-type-errors` in GHCi for naked expressions.
        -- See Note [Deferred type errors in GHCi]

        -- NB: The flag `-fdefer-type-errors` implies `-fdefer-type-holes`
        -- and `-fdefer-out-of-scope-variables`. However the flag
        -- `-fno-defer-type-errors` doesn't imply `-fdefer-type-holes` and
        -- `-fno-defer-out-of-scope-variables`. Thus the later two flags
        -- also need to be unset here.
        ; plan <- unsetGOptM Opt_DeferTypeErrors $
                  unsetGOptM Opt_DeferTypedHoles $
                  unsetGOptM Opt_DeferOutOfScopeVariables $
                    runPlans $ if generate_it
                                 then no_it_plans
                                 else it_plans

        ; dumpOptTcRn Opt_D_dump_tc_ast "Typechecker AST" FormatHaskell
              (showAstData NoBlankSrcSpan NoBlankEpAnnotations plan)

        ; fix_env <- getFixityEnv
        ; return (plan, fix_env) }

{- Note [Deferred type errors in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHCi, we ensure that type errors don't get deferred when type checking the
naked expressions. Deferring type errors here is unhelpful because the
expression gets evaluated right away anyway. It also would potentially emit
two redundant type-error warnings, one from each plan.

#14963 reveals another bug that when deferred type errors is enabled
in GHCi, any reference of imported/loaded variables (directly or indirectly)
in interactively issued naked expressions will cause ghc panic. See more
detailed discussion in #14963.

The interactively issued declarations, statements, as well as the modules
loaded into GHCi, are not affected. That means, for declaration, you could
have

    Prelude> :set -fdefer-type-errors
    Prelude> x :: IO (); x = putStrLn True
    <interactive>:14:26: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True

But for naked expressions, you will have

    Prelude> :set -fdefer-type-errors
    Prelude> putStrLn True
    <interactive>:2:10: error:
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘it’: it = putStrLn True

    Prelude> let x = putStrLn True
    <interactive>:2:18: warning: [-Wdeferred-type-errors]
        ? Couldn't match type ‘Bool’ with ‘[Char]’
          Expected type: String
            Actual type: Bool
        ? In the first argument of ‘putStrLn’, namely ‘True’
          In the expression: putStrLn True
          In an equation for ‘x’: x = putStrLn True
-}

tcUserStmt rdr_stmt@(L loc _)
  = do { (([rn_stmt], fix_env), fvs) <- checkNoErrs $
           rnStmts (HsDoStmt GhciStmtCtxt) rnExpr [rdr_stmt] $ \_ -> do
             fix_env <- getFixityEnv
             return (fix_env, emptyFNs)
            -- Don't try to typecheck if the renamer fails!
       ; traceRn "tcRnStmt" (vcat [ppr rdr_stmt, ppr rn_stmt, ppr fvs])
       ; rnDump rn_stmt ;

       ; opt_pr_flag <- goptM Opt_PrintBindResult
       ; ghciStep   <- getGhciStepIO
       ; printName  <- rnLookupKnownOccName printIdOcc
       ; thenIOName <- rnLookupKnownOccName thenIOIdOcc
       ; let gi_stmt | (L loc (BindStmt x pat expr)) <- rn_stmt
                     = L loc $ BindStmt x pat (nlHsApp ghciStep expr)
                     | otherwise
                     = rn_stmt

             mk_print_result_plan stmt v
               = do { stuff@([v_id], _) <- tcGhciStmts [stmt, mk_print v]
                    ; v_ty <- liftZonkM $ zonkTcType (idType v_id)
                    ; when (isUnitTy v_ty || not (isTauTy v_ty)) failM
                    ; return stuff }

             mk_print v = L loc $ BodyStmt noExtField (nlHsApp (nlHsVar printName)
                                           (nlHsVar v))
                                           (mkRnSyntaxExpr thenIOName) noSyntaxExpr

             print_result_plan
               | opt_pr_flag                         -- The flag says "print result"
               , [v] <- collectLStmtBinders CollNoDictBinders gi_stmt  -- One binder
               = Just $ mk_print_result_plan gi_stmt v
               | otherwise = Nothing

        -- The plans are:
        --      [stmt; print v]         if one binder and not v::()
        --      [stmt]                  otherwise
       ; plan <- runPlans $ maybe id (NE.<|) print_result_plan $
                 NE.singleton $ tcGhciStmts [gi_stmt]
       ; return (plan, fix_env) }

{-
Note [GHCi Plans]
~~~~~~~~~~~~~~~~~
When a user types an expression in the repl we try to print it in three different
ways. Also, depending on whether -fno-it is set, we bind a variable called `it`
which can be used to refer to the result of the expression subsequently in the repl.

The normal plans are :
  A. [it <- e; print e]     but not if it::()
  B. [it <- e]
  C. [let it = e; print it]

When -fno-it is set, the plans are:
  A. [e >>= print]
  B. [e]
  C. [let it = e in print it]

The reason for -fno-it is explained in #14336. `it` can lead to the repl
leaking memory as it is repeatedly queried.
-}

any_lifted :: Type
any_lifted = anyTypeOfKind liftedTypeKind

-- | Typecheck the statements given and then return the results of the
-- statement in the form 'IO [Any]'.
tcGhciStmts :: [GhciLStmt GhcRn] -> TcM PlanResult
tcGhciStmts stmts
 = do { ioTyCon <- tcLookupKnownKeyTyCon ioTyConKey
      ; ret_id  <- tcLookupKnownOccId returnIOIdOcc             -- return @ IO
      ; let ret_ty      = mkListTy any_lifted
            io_ret_ty   = mkTyConApp ioTyCon [ret_ty]
            tc_io_stmts = tcStmtsAndThen (HsDoStmt GhciStmtCtxt) tcDoStmt stmts
                                         (mkCheckExpType io_ret_ty)
            names = collectLStmtsBinders CollNoDictBinders stmts

        -- OK, we're ready to typecheck the stmts
      ; traceTc "GHC.Tc.Module.tcGhciStmts: tc stmts" empty
      ; ((tc_stmts, ids), lie) <- captureTopConstraints $
                                  tc_io_stmts $ \ _ ->
                                  mapM tcLookupId names
                        -- Look up the names right in the middle,
                        -- where they will all be in scope

        -- Simplify the context
      ; traceTc "GHC.Tc.Module.tcGhciStmts: simplify ctxt" empty
      ; const_binds <- checkNoErrs (simplifyInteractive lie)
                -- checkNoErrs ensures that the plan fails if context redn fails


      ; traceTc "GHC.Tc.Module.tcGhciStmts: done" empty

      -- ret_expr is the expression
      --      returnIO @[Any] [unsafeCoerce# @Any x, ..,  unsafeCoerce# @Any z]
      --
      -- Despite the inconvenience of building the type applications etc,
      -- this *has* to be done in type-annotated post-typecheck form
      -- because we are going to return a list of *polymorphic* values
      -- coerced to type Any. If we built a *source* stmt
      --      return [coerce x, ..., coerce z]
      -- then the type checker would instantiate x..z, and we wouldn't
      -- get their *polymorphic* values.  (And we'd get ambiguity errs
      -- if they were overloaded, since they aren't applied to anything.)
      --
      -- We use Any rather than a dummy type such as () because of
      -- the rules of unsafeCoerce#; see Unsafe/Coerce.hs for the details.

      ; unsafe_coerce_id <- tcLookupKnownKeyId unsafeCoercePrimIdKey
           -- We use unsafeCoerce# here because of (U11) in
           -- Note [Implementing unsafeCoerce] in base:Unsafe.Coerce

      ; let ret_expr = nlHsApp (nlHsTyApp ret_id [ret_ty]) $
                       noLocA $ ExplicitList any_lifted $
                       map mk_item ids

            mk_item id = unsafe_coerce_id `nlHsTyApp` [ getRuntimeRep (idType id)
                                                      , getRuntimeRep any_lifted
                                                      , idType id, any_lifted]
                                          `nlHsApp` nlHsVar id
            stmts = tc_stmts ++ [noLocA (mkLastStmt ret_expr)]

      ; return (ids, mkHsDictLet (EvBinds const_binds) $
                     noLocA (HsDo io_ret_ty GhciStmtCtxt (noLocA stmts)))
    }

-- | Generate a typed ghciStepIO expression (ghciStep :: Ty a -> IO a)
getGhciStepIO :: TcM (LHsExpr GhcRn)
getGhciStepIO = do
    ghciTy <- getGHCiMonad
    a_tv <- newName (mkTyVarOccFS (fsLit "a"))
    ioTyCon <- tcLookupKnownKeyTyCon ioTyConKey
    let ghciM   = nlHsAppTy (nlHsTyVar NotPromoted ghciTy) (nlHsTyVar NotPromoted a_tv)
        ioM     = nlHsAppTy (nlHsTyVar NotPromoted (tyConName ioTyCon)) (nlHsTyVar NotPromoted a_tv)

        step_ty :: LHsSigType GhcRn
        step_ty = noLocA $ HsSig
                     { sig_bndrs = HsOuterImplicit{hso_ximplicit = [a_tv]}
                     , sig_ext = noExtField
                     , sig_body = nlHsFunTy ghciM ioM }

        stepTy :: LHsSigWcType GhcRn
        stepTy = mkEmptyWildCardBndrs step_ty

    ghciStepIoMName <- idName <$> tcLookupKnownOccId ghciStepIoMOcc
    return (noLocA $ ExprWithTySig noExtField (nlHsVar ghciStepIoMName) stepTy)

getGHCiMonad :: TcRn Name
getGHCiMonad = do { hsc <- getTopEnv
                  ; monad_id <- case ic_monad $ hsc_IC hsc of
                      ExactOcc occ -> tyConName <$> tcLookupKnownOccTyCon occ
                      ExactName nm -> pure nm
                  ; return monad_id }

getInteractivePrintName :: TcRn Name
getInteractivePrintName = do { hsc <- getTopEnv
                             ; print_id <- case ic_int_print $ hsc_IC hsc of
                                 ExactOcc occ -> idName <$> tcLookupKnownOccId occ
                                 ExactName nm -> pure nm
                             ; return print_id }

isGHCiMonad :: HscEnv -> String -> IO (Messages TcRnMessage, Maybe Name)
isGHCiMonad hsc_env ty
  = runTcInteractive NoTcMPlugins hsc_env $ do
        rdrEnv <- getGlobalRdrEnv
        let occIO = lookupOccEnv rdrEnv (mkOccName tcName ty)
        case occIO of
            Just [n] -> do
                let name = greName n
                ghciClass <- tcLookupKnownOccClass ghciIoClassOcc
                userTyCon <- tcLookupTyCon name
                let userTy = mkTyConApp userTyCon []
                _ <- tcLookupInstance ghciClass [userTy]
                return name
            _ -> failWithTc $ TcRnGhciMonadLookupFail ty occIO

-- | How should we infer a type? See Note [TcRnExprMode]
data TcRnExprMode = TM_Inst     -- ^ Instantiate inferred quantifiers only (:type)
                  | TM_Default  -- ^ Instantiate all quantifiers,
                                --   and do eager defaulting (:type +d)

-- | tcRnExpr just finds the type of an expression
--   for :type
tcRnExpr :: HscEnv
         -> TcRnExprMode
         -> LHsExpr GhcPs
         -> IO (Messages TcRnMessage, Maybe Type)
tcRnExpr hsc_env mode rdr_expr
  = runTcInteractive StartAndStopTcMPlugins hsc_env $
    do {

    (rn_expr, _fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

    -- Typecheck the expression
    ((tclvl, (_tc_expr, res_ty)), lie)
          <- captureTopConstraints $
             pushTcLevelM          $
             (if inst then tcInferRho rn_expr
                      else tcInferSigma rn_expr);

    -- Generalise
    uniq <- newUnique ;
    let { fresh_it = itName uniq (getLocA rdr_expr) } ;
    ((qtvs, dicts, _, _), residual)
         <- captureConstraints $
            simplifyInfer TopLevel tclvl infer_mode
                          []    {- No sig vars -}
                          [(fresh_it, res_ty)]
                          lie ;

    -- Ignore the dictionary bindings
    _ <- perhaps_disable_default_warnings $
         simplifyInteractive residual ;

    let { all_expr_ty = mkInfForAllTys [(unmatchableDataConTy,tv) | tv <- qtvs] $
                        mkPhiTy (map idType dicts) res_ty } ;
    ty <- liftZonkM $ zonkTcType all_expr_ty ;

    -- See Note [Normalising the type in :type]
    fam_envs <- tcGetFamInstEnvs ;
    let { normalised_type = reductionReducedType $ normaliseType fam_envs Nominal ty
          -- normaliseType returns a coercion which we discard, so the Role is irrelevant.
        ; final_type = if isSigmaTy res_ty then ty else normalised_type } ;
    return final_type }
  where
    -- Optionally instantiate the type of the expression
    -- See Note [TcRnExprMode]
    (inst, infer_mode, perhaps_disable_default_warnings) = case mode of
      TM_Inst    -> (False, NoRestrictions,  id)
      TM_Default -> (True,  EagerDefaulting, unsetWOptM Opt_WarnTypeDefaults)

{- Note [Implementing :type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   :type const

We want    forall a b. a -> b -> a
and not    forall {a}{b}. a -> b -> a

The latter is what we'd get if we eagerly instantiated and then
re-generalised with Inferred binders.  It makes a difference, because
it tells us we where we can use Visible Type Application (VTA).

And also for   :type const @Int
we want        forall b. Int -> b -> Int
and not        forall {b}. Int -> b -> Int

Solution: use tcInferSigma, which in turn uses tcInferApp, which
has a special case for application chains.

Note [Normalising the type in :type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In :t <expr> we usually normalise the type (to simplify type functions)
before displaying the result.  Reason (see #10321): otherwise we may show
types like
    <expr> :: Vec (1+2) Int
rather than the simpler
    <expr> :: Vec 3 Int
In GHC.Tc.Gen.Bind.mkInferredPolyId we normalise for a very similar reason.

However this normalisation is less helpful when <expr> is just
an identifier, whose user-written type happens to contain type-function
applications.  E.g. (#20974)
    test :: F [Monad, A, B] m => m ()
where F is a type family.  If we say `:t test`, we'd prefer to see
the type family un-expanded.

We adopt the following ad-hoc solution: if the type inferred for <expr>
(before generalisation, namely res_ty) is a SigmaType (i.e. is not
fully instantiated) then do not normalise; otherwise normalise.
This is not ideal; for example, suppose  x :: F Int.  Then
  :t x
would be normalised because `F Int` is not a SigmaType.  But
anything here is ad-hoc, and it's a user-sought improvement.
-}

--------------------------
tcRnImportDecls :: HscEnv
                -> [LImportDecl GhcPs]
                -> IO (Messages TcRnMessage, Maybe GlobalRdrEnv)
-- Find the new chunk of GlobalRdrEnv created by this list of import
-- decls.  In contract tcRnImports *extends* the TcGblEnv.
tcRnImportDecls hsc_env import_decls
 =  runTcInteractive NoTcMPlugins hsc_env $
    do { (_, gbl_env) <- updGblEnv zap_rdr_env $
                         tcRnImports hsc_env $ map (,text "is directly imported") import_decls
       ; return (tcg_rdr_env gbl_env) }
  where
    zap_rdr_env gbl_env = gbl_env { tcg_rdr_env = emptyGlobalRdrEnv }


tcRnTypeSkolemising :: HscEnv
                    -> LHsType GhcPs
                    -> IO (Messages TcRnMessage, Maybe (Type, Kind))
-- tcRnTypeSkolemising skolemisese any free unification variables,
-- and normalises the type
tcRnTypeSkolemising env ty
  = do { skol_tv_ref <- liftIO (newIORef [])
       ; tcRnType env (SkolemiseFlexi skol_tv_ref) True ty }

-- tcRnType just finds the kind of a type
tcRnType :: HscEnv
         -> ZonkFlexi
         -> Bool        -- Normalise the returned type
         -> LHsType GhcPs
         -> IO (Messages TcRnMessage, Maybe (Type, Kind))
tcRnType hsc_env flexi normalise rdr_type
  = runTcInteractive StartAndStopTcMPlugins hsc_env $
    setXOptM LangExt.PolyKinds $   -- See Note [Kind-generalise in tcRnType]
    do { (HsWC { hswc_ext = wcs, hswc_body = rn_sig_type@(L _ (HsSig{sig_bndrs = outer_bndrs, sig_body = body })) }, _fvs)
                 -- we are using 'rnHsSigWcType' to bind the unbound type variables
                 -- and in combination with 'tcOuterTKBndrs' we are able to
                 -- implicitly quantify them as if the user wrote 'forall' by
                 -- hand (see #19217). This allows kind check to work in presence
                 -- of free type variables :
                 -- ghci> :k [a]
                 -- [a] :: *
               <- rnHsSigWcType GHCiCtx (mkHsWildCardBndrs $ noLocA (mkHsImplicitSigType rdr_type))
                  -- The type can have wild cards, but no implicit
                  -- generalisation; e.g.   :kind (T _)
       ; failIfErrsM

        -- We follow Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType here

        -- Now kind-check the type
        -- It can have any rank or kind
        -- First bring into scope any wildcards
       ; traceTc "tcRnType" (vcat [ppr wcs, ppr rn_sig_type])
       ; si <- mkSkolemInfo $ SigTypeSkol (GhciCtxt True)
       ; ((_, (ty, kind)), wanted)
               <- captureTopConstraints $
                  pushTcLevelM_         $
                  bindNamedWildCardBinders wcs $ \ wcs' ->
                  do { mapM_ emitNamedTypeHole wcs'
                     ; tcOuterTKBndrs si outer_bndrs $ tcInferLHsTypeUnsaturated body }
       -- Since all the wanteds are equalities, the returned bindings will be empty
       ; empty_binds <- simplifyTop wanted
       ; massertPpr (isEmptyBag empty_binds) (ppr empty_binds)

       -- Do kind generalisation; see Note [Kind-generalise in tcRnType]
       ; kvs <- kindGeneralizeAll unkSkol kind

       ; ty  <- initZonkEnv flexi $ zonkTcTypeToTypeX ty

       -- Do validity checking on type
       ; checkValidType (GhciCtxt True) ty

       -- Optionally (:k vs :k!) normalise the type. Does two things:
       --   normaliseType: expand type-family applications
       --   expandTypeSynonyms: expand type synonyms (#18828)
       ; fam_envs <- tcGetFamInstEnvs
       ; let ty' | normalise = expandTypeSynonyms $ reductionReducedType $
                               normaliseType fam_envs Nominal ty
                 | otherwise = ty

       ; traceTc "tcRnExpr" (debugPprType ty $$ debugPprType ty')
       ; return (ty', mkInfForAllTys [(matchableDataConTy,kv) | kv <- kvs] (typeKind ty')) }


{- Note [TcRnExprMode]
~~~~~~~~~~~~~~~~~~~~~~
How should we infer a type when a user asks for the type of an expression e
at the GHCi prompt? We offer 2 different possibilities, described below. Each
considers this example, with -fprint-explicit-foralls enabled.  See also
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0179-printing-foralls.rst

:type / TM_Inst

  In this mode, we report the type obtained by instantiating only the
  /inferred/ quantifiers of e's type, solving constraints, and
  re-generalising, as discussed in #11376.

  > :type reverse
  reverse :: forall a. [a] -> [a]

  -- foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  > :type foo @Int
  forall f b. (Show Int, Num b, Foldable f) => Int -> f b -> String

  Note that Show Int is still reported, because the solver never got a chance
  to see it.

:type +d / TM_Default

  This mode is for the benefit of users who wish to see instantiations
  of generalized types, and in particular to instantiate Foldable and
  Traversable.  In this mode, all type variables (inferred or
  specified) are instantiated.  Because GHCi uses
  -XExtendedDefaultRules, this means that Foldable and Traversable are
  defaulted.

  > :type +d reverse
  reverse :: forall {a}. [a] -> [a]

  -- foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  > :type +d foo @Int
  Int -> [Integer] -> String

  Note that this mode can sometimes lead to a type error, if a type variable is
  used with a defaultable class but cannot actually be defaulted:

  bar :: (Num a, Monoid a) => a -> a
  > :type +d bar
  ** error **

  The error arises because GHC tries to default a but cannot find a concrete
  type in the defaulting list that is both Num and Monoid. (If this list is
  modified to include an element that is both Num and Monoid, the defaulting
  would succeed, of course.)

  Note that the variables and constraints are reordered here, because this
  is possible during regeneralization. Also note that the variables are
  reported as Inferred instead of Specified.

Note [Kind-generalise in tcRnType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type, even when PolyKinds is not otherwise on.
This gives the right default behaviour at the GHCi prompt, where if
you say ":k T", and T has a polymorphic kind, you'd like to see that
polymorphism. Of course.  If T isn't kind-polymorphic you won't get
anything unexpected, but the apparent *loss* of polymorphism, for
types that you know are polymorphic, is quite surprising.  See Trac
#7688 for a discussion.

Note that the goal is to generalise the *kind of the type*, not
the type itself! Example:
  ghci> data SameKind :: k -> k -> Type
  ghci> :k SameKind _

We want to get `k -> Type`, not `Any -> Type`, which is what we would
get without kind-generalisation. Note that `:k SameKind` is OK, as
GHC will not instantiate SameKind here, and so we see its full kind
of `forall k. k -> k -> Type`.

************************************************************************
*                                                                      *
                 tcRnDeclsi
*                                                                      *
************************************************************************

tcRnDeclsi exists to allow class, data, and other declarations in GHCi.
-}

tcRnDeclsi :: HscEnv
           -> [LHsDecl GhcPs]
           -> IO (Messages TcRnMessage, Maybe TcGblEnv)
tcRnDeclsi hsc_env local_decls
  -- Keep 'TcM' plugins running, so that the desugarer can invoke them
  -- without having to re-initialise them.
  --
  -- See Note [Stop TcM plugins after desugaring] in GHC.Driver.Main.
  = runTcInteractive StartAndKeepRunningTcMPlugins hsc_env $
    tcRnSrcDecls False Nothing local_decls

externaliseAndTidyId :: Module -> Id -> TcM Id
externaliseAndTidyId this_mod id
  = do { name' <- externaliseName this_mod (idName id)
       ; return $ globaliseId id
                     `setIdName` name'
                     `setIdType` tidyTopType (idType id) }


{-
************************************************************************
*                                                                      *
        More GHCi stuff, to do with browsing and getting info
*                                                                      *
************************************************************************
-}

-- | ASSUMES that the module is either in the 'HomePackageTable' or is
-- a package module with an interface on disk.  If neither of these is
-- true, then the result will be an error indicating the interface
-- could not be found.
getModuleInterface :: HscEnv -> Module -> IO (Messages TcRnMessage, Maybe ModIface)
getModuleInterface hsc_env mod
  = runTcInteractive NoTcMPlugins hsc_env $
    loadModuleInterface (text "getModuleInterface") mod

tcRnLookupRdrName :: HscEnv -> LocatedN RdrName
                  -> IO (Messages TcRnMessage, Maybe [Name])
-- ^ Find all the Names that this RdrName could mean, in GHCi
tcRnLookupRdrName hsc_env (L loc rdr_name)
  = runTcInteractive NoTcMPlugins hsc_env $
    setSrcSpanA loc $
    do {   -- If the identifier is a constructor (begins with an
           -- upper-case letter), then we need to consider both
           -- constructor and type class identifiers.
         let rdr_names = dataTcOccs rdr_name
       ; names_s <- mapM lookupInfoOccRn rdr_names
       ; let names = concat names_s
       ; when (null names) (addErrTc $ TcRnNotInScope NotInScope rdr_name)
       ; return names }

tcRnLookupName :: HscEnv -> Name -> IO (Messages TcRnMessage, Maybe TyThing)
tcRnLookupName hsc_env name
  = runTcInteractive NoTcMPlugins hsc_env $
    tcRnLookupName' name

-- To look up a name we have to look in the local environment (tcl_lcl)
-- as well as the global environment, which is what tcLookup does.
-- But we also want a TyThing, so we have to convert:

tcRnLookupName' :: Name -> TcRn TyThing
tcRnLookupName' name = do
   tcthing <- tcLookup name
   case tcthing of
     AGlobal thing    -> return thing
     ATcId{tct_id=id} -> return (AnId id)
     _ -> panic "tcRnLookupName'"

tcRnGetInfo :: HscEnv
            -> Name
            -> IO ( Messages TcRnMessage
                  , Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))

-- Used to implement :info in GHCi
--
-- Look up a RdrName and return all the TyThings it might be
-- A capitalised RdrName is given to us in the DataName namespace,
-- but we want to treat it as *both* a data constructor
--  *and* as a type or class constructor;
-- hence the call to dataTcOccs, and we return up to two results
tcRnGetInfo hsc_env name
  = runTcInteractive NoTcMPlugins hsc_env $
    do { loadUnqualIfaces hsc_env (hsc_IC hsc_env)
           -- Load the interface for all unqualified types and classes
           -- That way we will find all the instance declarations
           -- (Packages have not orphan modules, and we assume that
           --  in the home package all relevant modules are loaded.)

       ; thing  <- tcRnLookupName' name
       ; fixity <- lookupFixityRn name
       ; (cls_insts, fam_insts) <- lookupInsts thing
       ; let info = lookupKnownNameInfo name
       ; return (thing, fixity, cls_insts, fam_insts, info) }


-- Lookup all class and family instances for a type constructor.
--
-- This function filters all instances in the type environment, so there
-- is a lot of duplicated work if it is called many times in the same
-- type environment. If this becomes a problem, the NameEnv computed
-- in GHC.getNameToInstancesIndex could be cached in TcM and both functions
-- could be changed to consult that index.
lookupInsts :: TyThing -> TcM ([ClsInst],[FamInst])
lookupInsts (ATyCon tc)
  = do  { InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods } <- tcGetInstEnvs
        ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
                -- Load all instances for all classes that are
                -- in the type environment (which are all the ones
                -- we've seen in any interface file so far)

          -- Return only the instances relevant to the given thing, i.e.
          -- the instances whose head contains the thing's name.
        ; let cls_insts =
                 [ ispec        -- Search all
                 | ispec <- instEnvElts home_ie ++ instEnvElts pkg_ie
                 , instIsVisible vis_mods ispec
                 , tc_name `elemNameSet` orphNamesOfClsInst ispec ]
        ; let fam_insts =
                 [ fispec
                 | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
                 , tc_name `elemNameSet` orphNamesOfFamInst fispec ]
        ; return (cls_insts, fam_insts) }
  where
    tc_name     = tyConName tc

lookupInsts _ = return ([],[])

loadUnqualIfaces :: HscEnv -> InteractiveContext -> TcM ()
-- Load the interface for everything that is in scope unqualified
-- This is so that we can accurately report the instances for
-- something
loadUnqualIfaces hsc_env ictxt
  = initIfaceTcRn $
    mapM_ (loadSysInterface doc) (moduleSetElts (mkModuleSet unqual_mods))
  where
    home_unit = hsc_home_unit hsc_env

    unqual_mods = [ nameModule name
                  | gre <- globalRdrEnvElts (icReaderEnv ictxt)
                  , let name = greName gre
                  , nameIsFromExternalPackage home_unit name
                  , isTcOcc (nameOccName name)   -- Types and classes only
                  , unQualOK gre ]               -- In scope unqualified
    doc = text "Need interface for module whose export(s) are in scope unqualified"



{-
************************************************************************
*                                                                      *
                Debugging output
      This is what happens when you do -ddump-types
*                                                                      *
************************************************************************
-}

-- | Dump, with a banner, if -ddump-rn
rnDump :: (Outputable a, Data a) => a -> TcRn ()
rnDump rn = dumpOptTcRn Opt_D_dump_rn "Renamer" FormatHaskell (ppr rn)

tcDump :: TcGblEnv -> TcRn ()
tcDump env
 = do { unit_state <- hsc_units <$> getTopEnv ;
        logger <- getLogger ;

        -- Dump short output if -ddump-types or -ddump-tc
        when (logHasDumpFlag logger Opt_D_dump_types || logHasDumpFlag logger Opt_D_dump_tc)
          (dumpTcRn True Opt_D_dump_types
            "" FormatText (pprWithUnitState unit_state short_dump)) ;

        -- Dump bindings if -ddump-tc
        dumpOptTcRn Opt_D_dump_tc "Typechecker" FormatHaskell full_dump;

        -- Dump bindings as an hsSyn AST if -ddump-tc-ast
        dumpOptTcRn Opt_D_dump_tc_ast "Typechecker AST" FormatHaskell ast_dump
   }
  where
    short_dump = pprTcGblEnv env
    full_dump  = pprLHsBinds (tcg_binds env)
        -- NB: foreign x-d's have undefined's in their types;
        --     hence can't show the tc_fords
    ast_dump = showAstData NoBlankSrcSpan NoBlankEpAnnotations (tcg_binds env)

-- It's unpleasant having both pprModGuts and pprModDetails here
pprTcGblEnv :: TcGblEnv -> SDoc
pprTcGblEnv (TcGblEnv { tcg_type_env  = type_env,
                        tcg_insts     = insts,
                        tcg_fam_insts = fam_insts,
                        tcg_rules     = rules,
                        tcg_imports   = imports })
  = getPprDebug $ \debug ->
    vcat [ ppr_types debug type_env
         , ppr_tycons debug fam_insts type_env
         , ppr_datacons debug type_env
         , ppr_patsyns type_env
         , ppr_insts insts
         , ppr_fam_insts fam_insts
         , ppr_rules rules
         , text "Dependent modules:" <+>
                (ppr . sort . installedModuleEnvElts $ imp_direct_dep_mods imports)
         , text "Dependent packages:" <+>
                ppr (S.toList $ imp_dep_direct_pkgs imports)]
                -- The use of sort is just to reduce unnecessary
                -- wobbling in testsuite output

ppr_rules :: [LRuleDecl GhcTc] -> SDoc
ppr_rules rules
  = ppUnless (null rules) $
    hang (text "RULES")
       2 (vcat (map ppr rules))

ppr_types :: Bool -> TypeEnv -> SDoc
ppr_types debug type_env
  = ppr_things "TYPE SIGNATURES" ppr_sig
             (sortBy (comparing getOccName) ids)
  where
    ids = [id | id <- typeEnvIds type_env, want_sig id]
    want_sig id
      | debug     = True
      | otherwise = hasTopUserName id
                    && case idDetails id of
                         VanillaId              -> True
                         SourceTableId {}       -> True
                         WorkerLikeId {}        -> True
                         RecSelId {}            -> True
                         ClassOpId {}           -> True
                         FCallId {}             -> True
                         _                      -> False
             -- Data cons (workers and wrappers), pattern synonyms,
             -- etc are suppressed (unless -dppr-debug),
             -- because they appear elsewhere

    ppr_sig id = hang (pprPrefixOcc id <+> dcolon) 2 (ppr (tidyTopType (idType id)))

ppr_tycons :: Bool -> [FamInst] -> TypeEnv -> SDoc
ppr_tycons debug fam_insts type_env
  = vcat [ ppr_things "TYPE CONSTRUCTORS" ppr_tc tycons
         , ppr_things "COERCION AXIOMS" ppr_ax
                      (typeEnvCoAxioms type_env) ]
  where
    fi_tycons = famInstsRepTyCons fam_insts

    tycons = sortBy (comparing getOccName) $
             [tycon | tycon <- typeEnvTyCons type_env
                    , want_tycon tycon]
             -- Sort by OccName to reduce unnecessary changes
    want_tycon tycon | debug      = True
                     | otherwise  = isExternalName (tyConName tycon) &&
                                    not (tycon `elem` fi_tycons)
    ppr_tc tc
       = vcat [ hang (ppr (tyConFlavour tc) <+> pprPrefixOcc (tyConName tc)
                      <> braces (ppr (tyConArity tc)) <+> dcolon)
                   2 (ppr (tidyTopType (tyConKind tc)))
              , nest 2 $
                ppWhen show_roles $
                text "roles" <+> (sep (map ppr roles)) ]
       where
         show_roles = debug || not (all (== boring_role) roles)
         roles = tyConRoles tc
         boring_role | isClassTyCon tc = Nominal
                     | otherwise       = Representational
            -- Matches the choice in GHC.Iface.Syntax, calls to pprRoles

    ppr_ax ax = ppr (coAxiomToIfaceDecl ax)
      -- We go via IfaceDecl rather than using pprCoAxiom
      -- This way we get the full axiom (both LHS and RHS) with
      -- wildcard binders tidied to _1, _2, etc.

ppr_datacons :: Bool -> TypeEnv -> SDoc
ppr_datacons debug type_env
  = ppr_things "DATA CONSTRUCTORS" ppr_dc wanted_dcs
      -- The filter gets rid of class data constructors
  where
    ppr_dc dc = ppr dc <+> dcolon <+> ppr (dataConWrapperType dc)
    all_dcs    = typeEnvDataCons type_env
    wanted_dcs | debug     = all_dcs
               | otherwise = filterOut is_cls_dc all_dcs
    is_cls_dc dc = isClassTyCon (dataConTyCon dc)

ppr_patsyns :: TypeEnv -> SDoc
ppr_patsyns type_env
  = ppr_things "PATTERN SYNONYMS" ppr_ps
               (typeEnvPatSyns type_env)
  where
    ppr_ps ps = pprPrefixOcc ps <+> dcolon <+> pprPatSynType ps

ppr_insts :: [ClsInst] -> SDoc
ppr_insts ispecs
  = ppr_things "CLASS INSTANCES" pprInstance ispecs

ppr_fam_insts :: [FamInst] -> SDoc
ppr_fam_insts fam_insts
  = ppr_things "FAMILY INSTANCES" pprFamInst fam_insts

ppr_things :: String -> (a -> SDoc) -> [a] -> SDoc
ppr_things herald ppr_one things
  | null things = empty
  | otherwise   = text herald $$ nest 2 (vcat (map ppr_one things))

hasTopUserName :: NamedThing x => x -> Bool
-- A top-level thing whose name is not "derived"
-- Thus excluding things like $tcX, from Typeable boilerplate
-- and C:Coll from class-dictionary data constructors
hasTopUserName x
  = isExternalName name && not (isDerivedOccName (nameOccName name))
  where
    name = getName x

{-
********************************************************************************

                         Running plugins

********************************************************************************
-}

runRenamerPlugin :: TcGblEnv
                 -> HsGroup GhcRn
                 -> TcM (TcGblEnv, HsGroup GhcRn)
runRenamerPlugin gbl_env hs_group = do
    hsc_env <- getTopEnv
    withPlugins (hsc_plugins hsc_env)
      (\p opts (e, g) -> ( mark_plugin_unsafe (hsc_dflags hsc_env)
                            >> renamedResultAction p opts e g))
      (gbl_env, hs_group)


-- XXX: should this really be a Maybe X?  Check under which circumstances this
-- can become a Nothing and decide whether this should instead throw an
-- exception/signal an error.
type RenamedStuff =
        (Maybe (HsGroup GhcRn, [LImportDecl GhcRn], Maybe [(LIE GhcRn, Avails)],
                Maybe (LHsDoc GhcRn), Maybe (XRec GhcRn HsModuleName)))

-- | Extract the renamed information from TcGblEnv.
getRenamedStuff :: TcGblEnv -> RenamedStuff
getRenamedStuff tc_result
  = fmap (\decls -> ( decls, tcg_rn_imports tc_result
                    , tcg_rn_exports tc_result, doc_hdr, name_hdr ))
         (tcg_rn_decls tc_result)
  where (doc_hdr, name_hdr) = tcg_hdr_info tc_result

runTypecheckerPlugin :: ModSummary -> TcGblEnv -> TcM TcGblEnv
runTypecheckerPlugin sum gbl_env = do
    hsc_env <- getTopEnv
    withPlugins (hsc_plugins hsc_env)
      (\p opts env -> mark_plugin_unsafe (hsc_dflags hsc_env)
                        >> typeCheckResultAction p opts sum env)
      gbl_env

mark_plugin_unsafe :: DynFlags -> TcM ()
mark_plugin_unsafe dflags = unless (gopt Opt_PluginTrustworthy dflags) $
  recordUnsafeInfer pluginUnsafe
  where
    !diag_opts = initDiagOpts dflags
    pluginUnsafe =
      singleMessage $
      mkPlainMsgEnvelope diag_opts noSrcSpan TcRnUnsafeDueToPlugin


-- Note [Typechecking default declarations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Typechecking default declarations requires careful placement:
--
-- 1. We must check them after types (tcTyAndClassDecls) because they can refer
-- to them. E.g.
--
--    data T = MkT ...
--    default(Int, T, Integer)
--
--    -- or even (tested by T11974b and T2245)
--    default(Int, T, Integer)
--    data T = MkT ...
--
-- 2. We must check them before typechecking deriving clauses (tcInstDeclsDeriv)
-- otherwise we may lookup default default types (Integer, Double) while checking
-- deriving clauses, ignoring the default declaration.
--
-- Before this careful placement (#24566), compiling the following example
-- (T24566) with "-ddump-if-trace -ddump-tc-trace" showed a call to
-- `applyDefaultingRules` with default types set to "(Integer,Double)":
--
--     module M where
--
--     import GHC.Classes
--     default ()
--
--     data Foo a = Nothing | Just a
--       deriving (Eq, Ord)
--
-- This was an issue while building modules like M in the ghc-internal package
-- because they would spuriously fail to build if the module defining Integer
-- (ghc-bignum:GHC.Num.Integer) wasn't compiled yet and its interface not to be
-- found. The implicit dependency between M and GHC.Num.Integer isn't known to
-- the build system.
-- In addition, trying to explicitly avoid the implicit dependency with `default
-- ()` didn't work, except if *standalone* deriving was used, which was an
-- inconsistent behavior.
