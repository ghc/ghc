{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Interfaces used for interactive prompts.
--
-------------------------------------------------------------------------------

module GHC.Driver.Main.Interactive
    (

    -- * Safe Haskell
      hscCheckSafe
    , hscGetSafe

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
    , hscIsGHCiMonad
    , hscGetModuleInterface
    , hscRnImportDecls
    , hscTcRnLookupRdrName
    , hscStmt, hscParseStmtWithLocation, hscStmtWithLocation, hscParsedStmt
    , hscParseDeclsWithLocation, hscParsedDecls
    , hscParseModuleWithLocation
    , hscTcExpr, TcRnExprMode(..), hscImport, hscKcType
    , hscParseExpr
    , hscParseType
    , hscCompileCoreExpr
    , hscTidy

    ) where

import GHC.Prelude

import GHC.Driver.Main.Hsc
import {-# SOURCE #-} GHC.Driver.Main.Passes
    ( hscDesugar', hscSimplify, hscCompileCoreExpr )

import {-# SOURCE #-} GHC.Driver.Main.Compile
    ( mkCgInteractiveGuts, generateFreshByteCodeLinkable )

import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Config.Core.Lint ( endPassHscEnvIO )
import GHC.Driver.Config.Core.Lint.Interactive ( lintInteractiveExpr )
import GHC.Driver.Config.Parser   (initParserOpts)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Tidy

import GHC.Runtime.Context
import GHCi.RemoteTypes

import GHC.Linker.Loader

import GHC.Hs
import GHC.Hs.Dump

import GHC.HsToCore



import GHC.Iface.Load   ( loadSysInterface )
import GHC.Iface.Tidy

import GHC.Core
import GHC.Core.ConLike
import GHC.Core.Opt.Pipeline.Types      ( CoreToDo (..))
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Rules
import GHC.Core.Stats

import GHC.Parser.Errors.Types
import GHC.Parser
import GHC.Parser.Lexer as Lexer

import GHC.Tc.Module
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Env ( ZonkFlexi (DefaultFlexi) )

import GHC.Unit
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import GHC.Types.Id
import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.Error
import GHC.Types.Fixity.Env
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.TyThing

import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Data.StringBuffer


import Data.Data hiding (Fixity, TyCon)
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Data.IORef
import qualified Data.Set as S
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)


-- -----------------------------------------------------------------------------
-- | Lookup things in the compiler's environment

hscTcRnLookupRdrName :: HscEnv -> LocatedN RdrName -> IO (NonEmpty Name)
hscTcRnLookupRdrName hsc_env0 rdr_name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       -- tcRnLookupRdrName can return empty list only together with TcRnUnknownMessage.
       -- Once errors has been dealt with in hoistTcRnMessage, we can enforce
       -- this invariant in types by converting to NonEmpty.
       ; ioMsgMaybe $ fmap (fmap (>>= NE.nonEmpty)) $ hoistTcRnMessage $
          tcRnLookupRdrName hsc_env rdr_name }

hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
hscTcRcLookupName hsc_env0 name = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe' $ hoistTcRnMessage $ tcRnLookupName hsc_env name
      -- ignore errors: the only error we're likely to get is
      -- "name not found", and the Maybe in the return type
      -- is used to indicate that.

hscTcRnGetInfo :: HscEnv -> Name
               -> IO (Maybe (TyThing, Fixity, [ClsInst], [FamInst], SDoc))
hscTcRnGetInfo hsc_env0 name
  = runInteractiveHsc hsc_env0 $
    do { hsc_env <- getHscEnv
       ; ioMsgMaybe' $ hoistTcRnMessage $ tcRnGetInfo hsc_env name }

hscIsGHCiMonad :: HscEnv -> String -> IO Name
hscIsGHCiMonad hsc_env name
  = runHsc hsc_env $ ioMsgMaybe $ hoistTcRnMessage $ isGHCiMonad hsc_env name

hscGetModuleInterface :: HscEnv -> Module -> IO ModIface
hscGetModuleInterface hsc_env0 mod = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ hoistTcRnMessage $ getModuleInterface hsc_env mod

-- -----------------------------------------------------------------------------
-- | Rename some import declarations
hscRnImportDecls :: HscEnv -> [LImportDecl GhcPs] -> IO GlobalRdrEnv
hscRnImportDecls hsc_env0 import_decls = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  ioMsgMaybe $ hoistTcRnMessage $ tcRnImportDecls hsc_env import_decls

-- -----------------------------------------------------------------------------
-- Safe Haskell things - could be it's own module.
-- -----------------------------------------------------------------------------

-- | Check that a module is safe to import.
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an exception may be thrown first.
hscCheckSafe :: HscEnv -> Module -> SrcSpan -> IO Bool
hscCheckSafe hsc_env m l = runHsc hsc_env $ do
    dflags <- getDynFlags
    pkgs <- snd `fmap` hscCheckSafe' m l
    when (packageTrustOn dflags) $ checkPkgTrust pkgs
    errs <- getDiagnostics
    return $ isEmptyMessages errs

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
hscGetSafe :: HscEnv -> Module -> SrcSpan -> IO (Bool, Set UnitId)
hscGetSafe hsc_env m l = runHsc hsc_env $ do
    (self, pkgs) <- hscCheckSafe' m l
    good         <- isEmptyMessages `fmap` getDiagnostics
    clearDiagnostics -- don't want them printed...
    let pkgs' | Just p <- self = S.insert p pkgs
              | otherwise      = pkgs
    return (good, pkgs')

-- | Is a module trusted? If not, throw or log errors depending on the type.
-- Return (regardless of trusted or not) if the trust type requires the modules
-- own package be trusted and a list of other packages required to be trusted
-- (these later ones haven't been checked) but the own package trust has been.
hscCheckSafe' :: Module -> SrcSpan
  -> Hsc (Maybe UnitId, Set UnitId)
hscCheckSafe' m l = do
    hsc_env <- getHscEnv
    let home_unit = hsc_home_unit hsc_env
    (tw, pkgs) <- isModSafe home_unit m l
    case tw of
        False                           -> return (Nothing, pkgs)
        True | isHomeModule home_unit m -> return (Nothing, pkgs)
             -- TODO: do we also have to check the trust of the instantiation?
             -- Not necessary if that is reflected in dependencies
             | otherwise   -> return (Just $ toUnitId (moduleUnit m), pkgs)
  where
    isModSafe :: HomeUnit -> Module -> SrcSpan -> Hsc (Bool, Set UnitId)
    isModSafe home_unit m l = do
        hsc_env <- getHscEnv
        dflags <- getDynFlags
        iface <- lookup' m
        sec <- getSourceErrorContext
        let diag_opts = initDiagOpts dflags
        case iface of
            -- can't load iface to check trust!
            Nothing -> throwOneError sec $
                         mkPlainErrorMsgEnvelope l $
                         GhcDriverMessage $ DriverCannotLoadInterfaceFile m

            -- got iface, check trust
            Just iface' ->
                let trust = getSafeMode $ mi_trust iface'
                    trust_own_pkg = mi_trust_pkg iface'
                    -- check module is trusted
                    safeM = trust `elem` [Sf_Safe, Sf_SafeInferred, Sf_Trustworthy]
                    -- check package is trusted
                    safeP = packageTrusted dflags (hsc_units hsc_env) home_unit trust trust_own_pkg m
                    -- pkg trust reqs
                    pkgRs = dep_trusted_pkgs $ mi_deps iface'
                    -- warn if Safe module imports Safe-Inferred module.
                    warns = if wopt Opt_WarnInferredSafeImports dflags
                                && safeLanguageOn dflags
                                && trust == Sf_SafeInferred
                                then inferredImportWarn diag_opts
                                else emptyMessages
                    -- General errors we throw but Safe errors we log
                    errs = case (safeM, safeP) of
                        (True, True ) -> emptyMessages
                        (True, False) -> pkgTrustErr
                        (False, _   ) -> modTrustErr
                in do
                    logDiagnostics warns
                    logDiagnostics errs
                    return (trust == Sf_Trustworthy, pkgRs)

                where
                    state = hsc_units hsc_env
                    inferredImportWarn diag_opts = singleMessage
                        $ mkMsgEnvelope diag_opts l (pkgQual state)
                        $ GhcDriverMessage $ DriverInferredSafeImport m
                    pkgTrustErr = singleMessage
                      $ mkErrorMsgEnvelope l (pkgQual state)
                      $ GhcDriverMessage $ DriverCannotImportFromUntrustedPackage state m
                    modTrustErr = singleMessage
                      $ mkErrorMsgEnvelope l (pkgQual state)
                      $ GhcDriverMessage $ DriverCannotImportUnsafeModule m

    -- Check the package a module resides in is trusted. Safe compiled
    -- modules are trusted without requiring that their package is trusted. For
    -- trustworthy modules, modules in the home package are trusted but
    -- otherwise we check the package trust flag.
    packageTrusted :: DynFlags -> UnitState -> HomeUnit -> SafeHaskellMode -> Bool -> Module -> Bool
    packageTrusted dflags unit_state home_unit safe_mode trust_own_pkg mod =
        case safe_mode of
            Sf_None      -> False -- shouldn't hit these cases
            Sf_Ignore    -> False -- shouldn't hit these cases
            Sf_Unsafe    -> False -- prefer for completeness.
            _ | not (packageTrustOn dflags)     -> True
            Sf_Safe | not trust_own_pkg         -> True
            Sf_SafeInferred | not trust_own_pkg -> True
            _ | isHomeModule home_unit mod      -> True
            _ -> unitIsTrusted $ unsafeLookupUnit unit_state (moduleUnit m)

    lookup' :: Module -> Hsc (Maybe ModIface)
    lookup' m = do
        hsc_env <- getHscEnv
        iface <- liftIO $ lookupIfaceByModuleHsc hsc_env m
        -- the 'lookupIfaceByModule' method will always fail when calling from GHCi
        -- as the compiler hasn't filled in the various module tables
        -- so we need to call 'getModuleInterface' to load from disk
        case iface of
            Just _  -> return iface
            Nothing -> liftIO $ initIfaceLoad hsc_env (Just <$> loadSysInterface (text "checkSafeImports") m)


-- | Check the list of packages are trusted.
checkPkgTrust :: Set UnitId -> Hsc ()
checkPkgTrust pkgs = do
    hsc_env <- getHscEnv
    let sec = initSourceErrorContext (hsc_dflags hsc_env)
        errors = S.foldr go emptyBag pkgs
        state  = hsc_units hsc_env
        go pkg acc
            | unitIsTrusted $ unsafeLookupUnitId state pkg
            = acc
            | otherwise
            = (`consBag` acc)
                     $ mkErrorMsgEnvelope noSrcSpan (pkgQual state)
                     $ GhcDriverMessage
                     $ DriverPackageNotTrusted state pkg
    if isEmptyBag errors
      then return ()
      else liftIO $ throwErrors sec $ mkMessages errors


{- **********************************************************************
%*                                                                      *
\subsection{Compiling a do-statement}
%*                                                                      *
%********************************************************************* -}

{-
When the UnlinkedBCOExpr is linked you get an HValue of type *IO [HValue]* When
you run it you get a list of HValues that should be the same length as the list
of names; add them to the ClosureEnv.

A naked expression returns a singleton Name [it]. The stmt is lifted into the
IO monad as explained in Note [Interactively-bound Ids in GHCi] in GHC.Runtime.Context
-}

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmt :: HscEnv -> String -> IO (Maybe ([Id], ForeignHValue, FixityEnv))
hscStmt hsc_env stmt = hscStmtWithLocation hsc_env stmt "<interactive>" 1

-- | Compile a stmt all the way to an HValue, but don't run it
--
-- We return Nothing to indicate an empty statement (or comment only), not a
-- parse error.
hscStmtWithLocation :: HscEnv
                    -> String -- ^ The statement
                    -> String -- ^ The source
                    -> Int    -- ^ Starting line
                    -> IO ( Maybe ([Id]
                          , ForeignHValue {- IO [HValue] -}
                          , FixityEnv))
hscStmtWithLocation hsc_env0 stmt source linenumber =
  runInteractiveHsc hsc_env0 $ do
    maybe_stmt <- hscParseStmtWithLocation source linenumber stmt
    case maybe_stmt of
      Nothing -> return Nothing

      Just parsed_stmt -> do
        hsc_env <- getHscEnv
        liftIO $ hscParsedStmt hsc_env parsed_stmt

hscParsedStmt :: HscEnv
              -> GhciLStmt GhcPs  -- ^ The parsed statement
              -> IO ( Maybe ([Id]
                    , ForeignHValue {- IO [HValue] -}
                    , FixityEnv))
hscParsedStmt hsc_env stmt = runInteractiveHsc hsc_env $ do
  -- Rename and typecheck it
  (ids, tc_expr, fix_env) <- ioMsgMaybe $ hoistTcRnMessage $ tcRnStmt hsc_env stmt

  -- Desugar it
  ds_expr <- ioMsgMaybe $ hoistDsMessage $ deSugarExpr hsc_env tc_expr
  liftIO (lintInteractiveExpr (text "desugar expression") hsc_env ds_expr)
  handleWarnings

  -- Then code-gen, and link it
  -- It's important NOT to have package 'interactive' as thisUnitId
  -- for linking, else we try to link 'main' and can't find it.
  -- Whereas the linker already knows to ignore 'interactive'
  let src_span = srcLocSpan interactiveSrcLoc
  (hval,_,_) <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

  return $ Just (ids, hval, fix_env)

hscParseModuleWithLocation :: HscEnv -> String -> Int -> String -> IO (HsModule GhcPs)
hscParseModuleWithLocation hsc_env source line_num str = do
    L _ mod <-
      runInteractiveHsc hsc_env $
        hscParseThingWithLocation source line_num parseModule str
    return mod

hscParseDeclsWithLocation :: HscEnv -> String -> Int -> String -> IO [LHsDecl GhcPs]
hscParseDeclsWithLocation hsc_env source line_num str = do
  HsModule { hsmodDecls = decls } <- hscParseModuleWithLocation hsc_env source line_num str
  return decls

hscParsedDecls :: HscEnv -> [LHsDecl GhcPs] -> IO ([TyThing], InteractiveContext)
hscParsedDecls hsc_env decls = runInteractiveHsc hsc_env $ do
    hsc_env <- getHscEnv
    let
      interp = hscInterp hsc_env
      -- We use a basically null location for iNTERACTIVE
      iNTERACTIVELoc = OsPathModLocation
          { ml_hs_file_ospath   = Nothing,
            ml_hi_file_ospath   = panic "hsDeclsWithLocation:ml_hi_file_ospath",
            ml_obj_file_ospath  = panic "hsDeclsWithLocation:ml_obj_file_ospath",
            ml_dyn_obj_file_ospath = panic "hsDeclsWithLocation:ml_dyn_obj_file_ospath",
            ml_dyn_hi_file_ospath = panic "hsDeclsWithLocation:ml_dyn_hi_file_ospath",
            ml_hie_file_ospath  = panic "hsDeclsWithLocation:ml_hie_file_ospath",
            ml_bytecode_file_ospath = panic "hsDeclsWithLocation:ml_bytecode_file_ospath"
            }

    {- Rename and typecheck it -}
    tc_gblenv <- ioMsgMaybe $ hoistTcRnMessage $ tcRnDeclsi hsc_env decls
      -- NB: 'tcRnDeclsi' keeps 'TcM' plugins running, so that the desugarer can invoke them.
      -- See Note [Stop TcM plugins after desugaring].

    {- Desugar it -}
    ds_result <- hscDesugar' iNTERACTIVELoc tc_gblenv
      -- 'TcM' plugins are stopped at the end of 'hscDesugar'.
      -- See Note [Stop TcM plugins after desugaring].

    {- Simplify -}
    simpl_mg <- liftIO $ do
      plugins <- readIORef (tcg_th_coreplugins tc_gblenv)
      hscSimplify hsc_env plugins ds_result

    {- Tidy -}
    (tidy_cg, mod_details) <- liftIO $ hscTidy hsc_env simpl_mg

    let !CgGuts{ cg_module    = this_mod,
                 cg_binds     = core_binds
                 } = tidy_cg

        !ModDetails { md_insts     = cls_insts
                    , md_fam_insts = fam_insts } = mod_details
            -- Get the *tidied* cls_insts and fam_insts

    {- Generate byte code & foreign stubs -}
    linkable <- liftIO $ generateFreshByteCodeLinkable hsc_env
      (moduleName this_mod)
      (mkCgInteractiveGuts tidy_cg)
      iNTERACTIVELoc

    let src_span = srcLocSpan interactiveSrcLoc
    _ <- liftIO $ loadDecls interp hsc_env src_span linkable

    let tcs = filterOut isImplicitTyCon (mg_tcs simpl_mg)
        patsyns = mg_patsyns simpl_mg

        ext_ids = [ id | id <- bindersOfBinds core_binds
                       , isExternalName (idName id)
                       , not (isDFunId id || isImplicitId id) ]
            -- We only need to keep around the external bindings
            -- (as decided by GHC.Iface.Tidy), since those are the only ones
            -- that might later be looked up by name.  But we can exclude
            --    - DFunIds, which are in 'cls_insts' (see Note [ic_tythings] in GHC.Runtime.Context
            --    - Implicit Ids, which are implicit in tcs
            -- c.f. GHC.Tc.Module.runTcInteractive, which reconstructs the TypeEnv

        new_tythings = map AnId ext_ids ++ map ATyCon tcs ++ map (AConLike . PatSynCon) patsyns
        ictxt        = hsc_IC hsc_env
        -- See Note [Fixity declarations in GHCi]
        fix_env      = tcg_fix_env tc_gblenv
        defaults     = tcg_default tc_gblenv
        new_ictxt    = extendInteractiveContext ictxt new_tythings cls_insts
                                                fam_insts defaults fix_env
    return (new_tythings, new_ictxt)


{-
  Note [Fixity declarations in GHCi]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  To support fixity declarations on types defined within GHCi (as requested
  in #10018) we record the fixity environment in InteractiveContext.
  When we want to evaluate something GHC.Tc.Module.runTcInteractive pulls out this
  fixity environment and uses it to initialize the global typechecker environment.
  After the typechecker has finished its business, an updated fixity environment
  (reflecting whatever fixity declarations were present in the statements we
  passed it) will be returned from hscParsedStmt. This is passed to
  updateFixityEnv, which will stuff it back into InteractiveContext, to be
  used in evaluating the next statement.

-}

hscImport :: HscEnv -> String -> IO (ImportDecl GhcPs)
hscImport hsc_env str = runInteractiveHsc hsc_env $ do
    -- Use >>= \case instead of MonadFail desugaring to take into
    -- consideration `instance XXModule p = DataConCantHappen`.
    -- Tracked in #15681
    hscParseThing parseModule str >>= \case
      (L _ (HsModule{hsmodImports=is})) ->
        case is of
            [L _ i] -> return i
            _ -> liftIO $ throwOneError sec $
                     mkPlainErrorMsgEnvelope noSrcSpan $
                     GhcPsMessage $ PsUnknownMessage $
                     mkSimpleUnknownDiagnostic $
                      mkPlainError noHints $
                         text "parse error in import declaration"
  where
    sec = initSourceErrorContext (hsc_dflags hsc_env)

-- | Typecheck an expression (but don't run it)
hscTcExpr :: HscEnv
          -> TcRnExprMode
          -> String -- ^ The expression
          -> IO Type
hscTcExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ hoistTcRnMessage $ tcRnExpr hsc_env mode parsed_expr

-- | Find the kind of a type, after generalisation
hscKcType
  :: HscEnv
  -> Bool            -- ^ Normalise the type
  -> String          -- ^ The type as a string
  -> IO (Type, Kind) -- ^ Resulting type (possibly normalised) and kind
hscKcType hsc_env0 normalise str = runInteractiveHsc hsc_env0 $ do
    hsc_env <- getHscEnv
    ty <- hscParseType str
    ioMsgMaybe $ hoistTcRnMessage $ tcRnType hsc_env DefaultFlexi normalise ty

hscParseExpr :: String -> Hsc (LHsExpr GhcPs)
hscParseExpr expr = do
  maybe_stmt <- hscParseStmt expr
  sec <- getSourceErrorContext
  case maybe_stmt of
    Just (L _ (BodyStmt _ expr _ _)) -> return expr
    _ -> throwOneError sec $
           mkPlainErrorMsgEnvelope noSrcSpan $
           GhcPsMessage $ PsUnknownMessage
             $ mkSimpleUnknownDiagnostic
             $ mkPlainError noHints $
             text "not an expression:" <+> quotes (text expr)

hscParseStmt :: String -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmt = hscParseThing parseStmt

hscParseStmtWithLocation :: String -> Int -> String
                         -> Hsc (Maybe (GhciLStmt GhcPs))
hscParseStmtWithLocation source linenumber stmt =
    hscParseThingWithLocation source linenumber parseStmt stmt

hscParseType :: String -> Hsc (LHsType GhcPs)
hscParseType = hscParseThing parseType

hscParseIdentifier :: HscEnv -> String -> IO (LocatedN RdrName)
hscParseIdentifier hsc_env str =
    runInteractiveHsc hsc_env $ hscParseThing parseIdentifier str

hscParseThing :: (Outputable thing, Data thing)
              => Lexer.P thing -> String -> Hsc thing
hscParseThing = hscParseThingWithLocation "<interactive>" 1

hscParseThingWithLocation :: (Outputable thing, Data thing) => String -> Int
                          -> Lexer.P thing -> String -> Hsc thing
hscParseThingWithLocation source linenumber parser str = do
    dflags <- getDynFlags
    logger <- getLogger
    withTiming logger
               (text "Parser [source]")
               (const ()) $ {-# SCC "Parser" #-} do

        let buf = stringToStringBuffer str
            loc = mkRealSrcLoc (fsLit source) linenumber 1

        case unP parser (initParserState (initParserOpts dflags) buf loc) of
            PFailed pst ->
                handleWarningsThrowErrors (getPsMessages pst)
            POk pst thing -> do
                logWarningsReportErrors (getPsMessages pst)
                liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed "Parser"
                            FormatHaskell (ppr thing)
                liftIO $ putDumpFileMaybe logger Opt_D_dump_parsed_ast "Parser AST"
                            FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations thing)
                return thing

hscTidy :: HscEnv -> ModGuts -> IO (CgGuts, ModDetails)
hscTidy hsc_env guts = do
  let logger   = hsc_logger hsc_env
  let this_mod = mg_module guts

  opts <- initTidyOpts hsc_env
  (cgguts, details) <- withTiming logger
    (text "CoreTidy"<+>brackets (ppr this_mod))
    (const ())
    $! {-# SCC "CoreTidy" #-} tidyProgram opts guts

  -- post tidy pretty-printing and linting...
  let tidy_rules     = md_rules details
  let all_tidy_binds = cg_binds cgguts
  let name_ppr_ctx   = mkNamePprCtx ptc (hsc_unit_env hsc_env) (mg_rdr_env guts)
      ptc            = initPromotionTickContext (hsc_dflags hsc_env)

  endPassHscEnvIO hsc_env name_ppr_ctx CoreTidy all_tidy_binds tidy_rules

  -- If the endPass didn't print the rules, but ddump-rules is
  -- on, print now
  unless (logHasDumpFlag logger Opt_D_dump_simpl) $
    putDumpFileMaybe logger Opt_D_dump_rules
      "Tidy Core rules"
      FormatText
      (pprRulesForUser tidy_rules)

  -- Print one-line size info
  let cs = coreBindsStats all_tidy_binds
  putDumpFileMaybe logger Opt_D_dump_core_stats "Core Stats"
    FormatText
    (text "Tidy size (terms,types,coercions)"
     <+> ppr (moduleName this_mod) <> colon
     <+> int (cs_tm cs)
     <+> int (cs_ty cs)
     <+> int (cs_co cs))

  pure (cgguts, details)


