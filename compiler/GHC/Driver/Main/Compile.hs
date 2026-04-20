{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Aspects of GHC.Driver.Main that deal with complete Modules/Source files.
--
-- This includes compiling to both Hard *and* Bytecode.
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--
-------------------------------------------------------------------------------

module GHC.Driver.Main.Compile
    (

    -- * Compiling complete source files
      HscBackendAction (..), HscRecompStatus (..)
    , initWholeCoreBindings
    , loadIfaceByteCode
    , loadIfaceByteCodeLazy
    , hscMaybeWriteIface
    , hscCompileCmmFile
    , hscGenHardCode

    , hscInteractive
    , mkCgInteractiveGuts
    , CgInteractiveGuts(..)
    , generateAndWriteByteCodeLinkable
    , generateFreshByteCodeLinkable
    , doCodeGen
    , myCoreToStg
    ) where


import GHC.Prelude

import GHC.Driver.Main.Hsc
    ( add_iface_to_hpt, iface_core_bindings, ioMsgMaybe )


import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.ByteCode
import GHC.Driver.Env.KnotVars
import GHC.Driver.Errors.Types
import GHC.Driver.CodeOutput
import GHC.Driver.Config.Cmm.Parser (initCmmParserConfig)
import GHC.Driver.Config.CoreToStg
import GHC.Driver.Config.CoreToStg.Prep
import GHC.Driver.Config.Stg.Ppr  (initStgPprOpts)
import GHC.Driver.Config.Stg.Pipeline (initStgPipelineOpts)
import GHC.Driver.Config.StgToCmm  (initStgToCmmConfig)
import GHC.Driver.Config.Cmm       (initCmmConfig)
import GHC.Driver.Config.StgToJS  (initStgToJSConfig)
import GHC.Driver.Hooks
import GHC.Driver.GenerateCgIPEStub (generateCgIPEStub, lookupEstimatedTicks)

import GHC.ByteCode.Types

import GHC.Linker.Types

import GHC.Hs
import GHC.HsToCore.Coverage qualified as Coverage

import GHC.StgToByteCode    ( byteCodeGen )
import GHC.StgToJS          ( stgToJS )

import GHC.IfaceToCore  ( typecheckWholeCoreBindings )

import GHC.Iface.Load   ( writeIface, flagsToIfCompression )

import qualified GHC.ByteCode.Serialize as ByteCode

import GHC.Core
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.TyCon
import GHC.Core.LateCC
import GHC.Core.LateCC.Types

import GHC.CoreToStg.Prep( CorePrepPgmConfig, corePrepPgm )
import GHC.CoreToStg.AddImplicitBinds( addImplicitBinds )
import GHC.CoreToStg    ( coreToStg )

import GHC.Tc.Utils.Monad

import GHC.Stg.Syntax
import GHC.Stg.Pipeline ( stg2stg, StgCgInfos )

import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.StgToCmm.Types (CmmCgInfos (..), ModuleLFInfos, LambdaFormInfo(..))
import GHC.StgToCmm.CgUtils (CgStream)

import GHC.Cmm
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Info
import GHC.Cmm.Parser
import GHC.Cmm.UniqueRenamer

import GHC.Unit
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Status
import GHC.Unit.Home.ModInfo

import GHC.Types.HpcInfo (HpcInfo (..))
import GHC.Types.Id
import GHC.Types.ForeignStubs
import GHC.Types.Name.Env      ( mkNameEnv )
import GHC.Types.Var.Set
import GHC.Types.Error
import GHC.Types.CostCentre
import GHC.Types.IPE

import GHC.Utils.Encoding ( utf8EncodeShortByteString )
import GHC.Utils.Fingerprint ( Fingerprint )
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.Touch

import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.OsPath (unsafeEncodeUtf)
import qualified GHC.Data.Stream as Stream


import Data.Traversable (for)
import Control.Monad
import Data.IORef
import System.Directory
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import GHC.Unit.Module.WholeCoreBindings
import GHC.Types.TypeEnv
import Data.Time

import System.IO.Unsafe ( unsafeInterleaveIO )
import GHC.Iface.Env ( trace_if )
import GHC.Stg.EnforceEpt.TagSig (seqTagSig)
import GHC.StgToCmm.Utils (IPEStats)
import GHC.Types.Unique.FM
import GHC.Cmm.Config (CmmConfig)
import Data.Bifunctor


{- **********************************************************************
%*                                                                      *
                The main compiler pipeline
%*                                                                      *
%********************************************************************* -}

{-
                   --------------------------------
                        The compilation proper
                   --------------------------------

It's the task of the compilation proper to compile Haskell, hs-boot and core
files to either byte-code, hard-code (C, asm, LLVM, etc.) or to nothing at all
(the module is still parsed and type-checked. This feature is mostly used by
IDE's and the likes). Compilation can happen in either 'one-shot', 'batch',
'nothing', or 'interactive' mode. 'One-shot' mode targets hard-code, 'batch'
mode targets hard-code, 'nothing' mode targets nothing and 'interactive' mode
targets byte-code.

The modes are kept separate because of their different types and meanings:

 * In 'one-shot' mode, we're only compiling a single file and can therefore
 discard the new ModIface and ModDetails. This is also the reason it only
 targets hard-code; compiling to byte-code or nothing doesn't make sense when
 we discard the result.

 * 'Batch' mode is like 'one-shot' except that we keep the resulting ModIface
 and ModDetails. 'Batch' mode doesn't target byte-code since that require us to
 return the newly compiled byte-code.

 * 'Nothing' mode has exactly the same type as 'batch' mode but they're still
 kept separate. This is because compiling to nothing is fairly special: We
 don't output any interface files, we don't run the simplifier and we don't
 generate any code.

 * 'Interactive' mode is similar to 'batch' mode except that we return the
 compiled byte-code together with the ModIface and ModDetails.

Trying to compile a hs-boot file to byte-code will result in a run-time error.
This is the only thing that isn't caught by the type-system.
-}


-- | Return an 'IO' that hydrates Core bindings and compiles them to bytecode if
-- the interface contains any, using the supplied type env for typechecking.
--
-- Unlike 'initWholeCoreBindings', this does not use lazy IO.
-- Instead, the 'IO' is only evaluated (in @get_link_deps@) when it is clear
-- that it will be used immediately (because we're linking TH with
-- @-fprefer-byte-code@ in oneshot mode), and the result is cached in
-- 'LoaderState'.
--
-- 'initWholeCoreBindings' needs the laziness because it is used to populate
-- 'HomeModInfo', which is done preemptively, in anticipation of downstream
-- modules using the bytecode for TH in make mode, which might never happen.
loadIfaceByteCode ::
  HscEnv ->
  ModIface ->
  ModLocation ->
  TypeEnv ->
  Maybe (IO Linkable)
loadIfaceByteCode hsc_env iface location type_env =
  compile <$> iface_core_bindings iface location
  where
    compile decls = do
      bco <- compileWholeCoreBindings hsc_env type_env decls
      linkable $ pure $ DotGBC bco

    linkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file_ospath location)
      time <- maybe getCurrentTime pure if_time
      return $! Linkable time (mi_module iface) parts

loadIfaceByteCodeLazy ::
  HscEnv ->
  ModIface ->
  ModLocation ->
  TypeEnv ->
  IO (Maybe (LinkableWith ModuleByteCode))
loadIfaceByteCodeLazy hsc_env iface location type_env =
  case iface_core_bindings iface location of
    Nothing -> return Nothing
    Just wcb -> do
      Just <$> compile wcb
  where
    compile decls = do
      bco <- unsafeInterleaveIO $ do
          compileWholeCoreBindings hsc_env type_env decls
      linkable bco

    linkable parts = do
      if_time <- modificationTimeIfExists (ml_hi_file_ospath location)
      time <- maybe getCurrentTime pure if_time
      return $!Linkable time (mi_module iface) parts

-- | If the 'Linkable' contains Core bindings loaded from an interface, replace
-- them with a lazy IO thunk that compiles them to bytecode and foreign objects,
-- using the supplied environment for type checking.
--
-- The laziness is necessary because this value is stored purely in a
-- 'HomeModLinkable' in the home package table, rather than some dedicated
-- mutable state that would generate bytecode on demand, so we have to call this
-- function even when we don't know that we'll need the bytecode.
--
-- In addition, the laziness has to be hidden inside 'BCOs' because
-- 'Linkable' is used too generally, so that looking at the constructor to
-- decide whether to discard it when linking native code would force the thunk
-- otherwise, incurring a significant performance penalty.
--
-- This is sound because generateByteCode just depends on things already loaded
-- in the interface file.

-- TODO: We should just use loadIfaceByteCodeLazy instead of the two stage process with
-- loadByteCode and initWholeCoreBindings. The main reason it is like this is because
-- initWholeCoreBindings requires a ModDetails, which we don't have during recompilation
-- checking. We should modify recompilation checking to return a HomeModInfo directly.

initWholeCoreBindings ::
  HscEnv ->
  ModIface ->
  ModDetails ->
  RecompLinkables ->
  IO HomeModLinkable
initWholeCoreBindings hsc_env iface details (RecompLinkables bc o) = do
  bc' <- go bc
  pure $ HomeModLinkable bc' o
  where
    type_env = md_types details

    go :: RecompBytecodeLinkable -> IO (Maybe (LinkableWith ModuleByteCode))
    go (NormalLinkable l) = pure l
    go (WholeCoreBindingsLinkable wcbl) =
      fmap Just $ for wcbl $ \wcb -> do
        add_iface_to_hpt iface details hsc_env
        bco <- unsafeInterleaveIO $ do
            compileWholeCoreBindings hsc_env type_env wcb
        pure bco

-- | Hydrate interface Core bindings and compile them to bytecode.
--
-- This consists of:
--
-- 1. Running a typechecking step to insert the global names that were removed
--    when the interface was written or were unavailable due to boot import
--    cycles, converting the bindings to 'CoreBind'.
--
-- 2. Restoring the foreign build inputs from their serialized format, resulting
--    in a set of foreign import stubs and source files added via
--    'qAddForeignFilePath'.
--
-- 3. Generating bytecode and foreign objects from the results of the previous
--    steps using the usual pipeline actions.
compileWholeCoreBindings ::
  HscEnv ->
  TypeEnv ->
  WholeCoreBindings ->
  IO ModuleByteCode
compileWholeCoreBindings hsc_env type_env wcb = do
  core_binds <- typecheck
  (stubs, foreign_files) <- decode_foreign
  gen_bytecode core_binds stubs foreign_files
  where
    typecheck = do
      types_var <- newIORef type_env
      let
        tc_env = hsc_env {
          hsc_type_env_vars =
            knotVarsFromModuleEnv (mkModuleEnv [(wcb_module, types_var)])
        }
      initIfaceCheck (text "l") tc_env $
        typecheckWholeCoreBindings types_var wcb

    decode_foreign =
      decodeIfaceForeign logger (hsc_tmpfs hsc_env)
      (tmpDir (hsc_dflags hsc_env)) wcb_foreign

    gen_bytecode core_binds stubs foreign_files = do
      let cgi_guts = CgInteractiveGuts wcb_module core_binds
                      (typeEnvTyCons type_env) stubs foreign_files
                      Nothing [] NoHpcInfo
      trace_if logger (text "Generating ByteCode for" <+> ppr wcb_module)
      mkModuleByteCode hsc_env wcb_module wcb_mod_location cgi_guts

    WholeCoreBindings {wcb_module, wcb_mod_location, wcb_foreign} = wcb

    logger = hsc_logger hsc_env

{-
Note [ModDetails and --make mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An interface file consists of two parts

* The `ModIface` which ends up getting written to disk.
  The `ModIface` is a completely acyclic tree, which can be serialised
  and de-serialised completely straightforwardly.  The `ModIface` is
  also the structure that is finger-printed for recompilation control.

* The `ModDetails` which provides a more structured view that is suitable
  for usage during compilation.  The `ModDetails` is heavily cyclic:
  An `Id` contains a `Type`, which mentions a `TyCon` that contains kind
  that mentions other `TyCons`; the `Id` also includes an unfolding that
  in turn mentions more `Id`s;  And so on.

The `ModIface` can be created from the `ModDetails` and the `ModDetails` from
a `ModIface`.

During tidying, just before interfaces are written to disk,
the ModDetails is calculated and then converted into a ModIface (see GHC.Iface.Make.mkIface_).
Then when GHC needs to restart typechecking from a certain point it can read the
interface file, and regenerate the ModDetails from the ModIface (see GHC.IfaceToCore.typecheckIface).
The key part about the loading is that the ModDetails is regenerated lazily
from the ModIface, so that there's only a detailed in-memory representation
for declarations which are actually used from the interface. This mode is
also used when reading interface files from external packages.

In the old --make mode implementation, the interface was written after compiling a module
but the in-memory ModDetails which was used to compute the ModIface was retained.
The result was that --make mode used much more memory than `-c` mode, because a large amount of
information about a module would be kept in the ModDetails but never used.

The new idea is that even in `--make` mode, when there is an in-memory `ModDetails`
at hand, we re-create the `ModDetails` from the `ModIface`. Doing this means that
we only have to keep the `ModIface` decls in memory and then lazily load
detailed representations if needed. It turns out this makes a really big difference
to memory usage, halving maximum memory used in some cases.

See !5492 and #13586
-}

{-
Note [Writing interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We write one interface file per module and per compilation, except with
-dynamic-too where we write two interface files (non-dynamic and dynamic).

We can write two kinds of interfaces (see Note [Interface file stages] in
"GHC.Driver.Types"):

   * simple interface: interface generated after the core pipeline

   * full interface: simple interface completed with information from the
     backend

Depending on the situation, we write one or the other (using
`hscMaybeWriteIface`). We must be careful with `-dynamic-too` because only the
backend is run twice, so if we write a simple interface we need to write both
the non-dynamic and the dynamic interfaces at the same time (with the same
contents).

Cases for which we generate simple interfaces:

   * GHC.Driver.Main.hscDesugarAndSimplify: when a compilation does NOT require (re)compilation
   of the hard code

   * GHC.Driver.Pipeline.compileOne': when we run in One Shot mode and target
   bytecode (if interface writing is forced).

   * GHC.Driver.Backpack uses simple interfaces for indefinite units
   (units with module holes). It writes them indirectly by forcing the
   -fwrite-interface flag while setting backend to NoBackend.

Cases for which we generate full interfaces:

   * GHC.Driver.Pipeline.runPhase: when we must be compiling to regular hard
   code and/or require recompilation.

By default interface file names are derived from module file names by adding
suffixes. The interface file name can be overloaded with "-ohi", except when
`-dynamic-too` is used.

-}

-- | Write interface files
hscMaybeWriteIface
  :: Logger
  -> DynFlags
  -> Bool
  -- ^ Is this a simple interface generated after the core pipeline, or one
  -- with information from the backend? See: Note [Writing interface files]
  -> ModIface
  -> Maybe Fingerprint
  -- ^ The old interface hash, used to decide if we need to actually write the
  -- new interface.
  -> ModLocation
  -> IO ()
hscMaybeWriteIface logger dflags is_simple iface old_iface mod_location = do
    let force_write_interface = gopt Opt_WriteInterface dflags
        write_interface = backendWritesFiles (backend dflags)

        write_iface dflags' iface =
          let !iface_name = if dynamicNow dflags' then ml_dyn_hi_file mod_location else ml_hi_file mod_location
              profile     = targetProfile dflags'
          in
          {-# SCC "writeIface" #-}
          withTiming logger
              (text "WriteIface"<+>brackets (text iface_name))
              (const ())
              (writeIface logger profile (flagsToIfCompression dflags) iface_name iface)

    if (write_interface || force_write_interface) then do

      -- FIXME: with -dynamic-too, "change" is only meaningful for the
      -- non-dynamic interface, not for the dynamic one. We should have another
      -- flag for the dynamic interface. In the meantime:
      --
      --    * when we write a single full interface, we check if we are
      --    currently writing the dynamic interface due to -dynamic-too, in
      --    which case we ignore "change".
      --
      --    * when we write two simple interfaces at once because of
      --    dynamic-too, we use "change" both for the non-dynamic and the
      --    dynamic interfaces. Hopefully both the dynamic and the non-dynamic
      --    interfaces stay in sync...
      --
      let change = old_iface /= Just (mi_iface_hash iface)

      let dt = dynamicTooState dflags

      when (logHasDumpFlag logger Opt_D_dump_if_trace) $ putMsg logger $
        hang (text "Writing interface(s):") 2 $ vcat
         [ text "Kind:" <+> if is_simple then text "simple" else text "full"
         , text "Hash change:" <+> ppr change
         , text "DynamicToo state:" <+> text (show dt)
         ]

      if is_simple
         then when change $ do -- FIXME: see 'change' comment above
            write_iface dflags iface
            case dt of
               DT_Dont   -> return ()
               DT_Dyn    -> panic "Unexpected DT_Dyn state when writing simple interface"
               DT_OK     -> write_iface (setDynamicNow dflags) iface
         else case dt of
               DT_Dont | change                    -> write_iface dflags iface
               DT_OK   | change                    -> write_iface dflags iface
               -- FIXME: see change' comment above
               DT_Dyn                              -> write_iface dflags iface
               _                                   -> return ()

      when (gopt Opt_WriteHie dflags) $ do
          -- This is slightly hacky. A hie file is considered to be up to date
          -- if its modification time on disk is greater than or equal to that
          -- of the .hi file (since we should always write a .hi file if we are
          -- writing a .hie file). However, with the way this code is
          -- structured at the moment, the .hie file is often written before
          -- the .hi file; by touching the file here, we ensure that it is
          -- correctly considered up-to-date.
          --
          -- The file should exist by the time we get here, but we check for
          -- existence just in case, so that we don't accidentally create empty
          -- .hie files.
          let hie_file = ml_hie_file mod_location
          whenM (doesFileExist hie_file) $
            GHC.Utils.Touch.touch hie_file
    else
        -- See Note [Strictness in ModIface]
        forceModIface iface


--------------------------------------------------------------
-- Safe Haskell
--------------------------------------------------------------

-- Note [Safe Haskell Trust Check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Safe Haskell checks that an import is trusted according to the following
-- rules for an import of module M that resides in Package P:
--
--   * If M is recorded as Safe and all its trust dependencies are OK
--     then M is considered safe.
--   * If M is recorded as Trustworthy and P is considered trusted and
--     all M's trust dependencies are OK then M is considered safe.
--
-- By trust dependencies we mean that the check is transitive. So if
-- a module M that is Safe relies on a module N that is trustworthy,
-- importing module M will first check (according to the second case)
-- that N is trusted before checking M is trusted.
--
-- This is a minimal description, so please refer to the user guide
-- for more details. The user guide is also considered the authoritative
-- source in this matter, not the comments or code.


--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

-- | Compile to hard-code.
hscGenHardCode :: HscEnv -> CgGuts -> ModLocation -> FilePath
               -> IO (FilePath, Maybe FilePath, [(ForeignSrcLang, FilePath)], Maybe StgCgInfos, Maybe CmmCgInfos )
                -- ^ @Just f@ <=> _stub.c is f
hscGenHardCode hsc_env cgguts mod_loc output_filename = do
        let CgGuts{ cg_module   = this_mod,
                    cg_binds    = core_binds,
                    cg_ccs      = local_ccs
                    } = cgguts
            dflags = hsc_dflags hsc_env
            logger = hsc_logger hsc_env

        -------------------
        -- ADD IMPLICIT BINDINGS
        -- NB: we must feed mkImplicitBinds through corePrep too
        -- so that they are suitably cloned and eta-expanded
        let cp_pgm_cfg :: CorePrepPgmConfig
            cp_pgm_cfg = initCorePrepPgmConfig (hsc_dflags hsc_env)
                                               (interactiveInScope $ hsc_IC hsc_env)
        binds_with_implicits <- addImplicitBinds cp_pgm_cfg mod_loc (cg_tycons cgguts) core_binds

        -------------------
        -- INSERT LATE COST CENTRES, based on the provided flags.
        --
        -- If -fprof-late-inline is enabled, we will skip adding CCs on any
        -- top-level bindings here (via shortcut in `addLateCostCenters`), since
        -- it will have already added a superset of the CCs we would add here.
        let
          late_cc_config :: LateCCConfig
          late_cc_config =
            LateCCConfig
              { lateCCConfig_whichBinds =
                  if gopt Opt_ProfLateInlineCcs dflags then
                    LateCCNone
                  else if gopt Opt_ProfLateCcs dflags then
                    LateCCBinds
                  else if gopt Opt_ProfLateOverloadedCcs dflags then
                    LateCCOverloadedBinds
                  else
                    LateCCNone
              , lateCCConfig_overloadedCalls =
                  gopt Opt_ProfLateoverloadedCallsCCs dflags
              , lateCCConfig_env =
                  LateCCEnv
                    { lateCCEnv_module = this_mod
                    , lateCCEnv_file = fsLit <$> ml_hs_file mod_loc
                    , lateCCEnv_countEntries= gopt Opt_ProfCountEntries dflags
                    , lateCCEnv_collectCCs = True
                    }
              }

        (late_cc_binds, late_cc_state) <-
          addLateCostCenters logger late_cc_config binds_with_implicits

        when (dopt Opt_D_dump_late_cc dflags || dopt Opt_D_verbose_core2core dflags) $
          putDumpFileMaybe logger Opt_D_dump_late_cc "LateCC" FormatCore (vcat (map ppr late_cc_binds))

        -------------------
        -- RUN LATE PLUGINS
        -- This is the last use of the CgGuts in a compilation.
        -- From now on, we just use the bits we need.
        ( CgGuts
            { cg_tycons        = tycons,
              cg_foreign       = foreign_stubs0,
              cg_foreign_files = foreign_files,
              cg_dep_pkgs      = dependencies,
              cg_spt_entries   = spt_entries,
              cg_binds         = binds_to_prep,
              cg_ccs           = late_local_ccs
            }
          , _
          ) <-
          {-# SCC latePlugins #-}
          withTiming
            logger
            (text "LatePlugins"<+>brackets (ppr this_mod))
            (const ()) $
            withPlugins (hsc_plugins hsc_env)
              (($ hsc_env) . latePlugin)
                ( cgguts
                    { cg_binds = late_cc_binds
                    , cg_ccs = S.toList (lateCCState_ccs late_cc_state) ++ local_ccs
                    }
                , lateCCState_ccState late_cc_state
                )

        let
          hooks  = hsc_hooks hsc_env
          tmpfs  = hsc_tmpfs hsc_env
          llvm_config = hsc_llvm_config hsc_env
          profile = targetProfile dflags

        -------------------
        -- PREPARE FOR CODE GENERATION
        -- Do saturation and convert to A-normal form
        cp_cfg <- initCorePrepConfig hsc_env
        (prepd_binds) <- {-# SCC "CorePrep" #-}
                         corePrepPgm
                           (hsc_logger hsc_env) cp_cfg cp_pgm_cfg
                           this_mod binds_to_prep

        -----------------  Convert to STG ------------------
        (stg_binds_with_deps, denv, (caf_ccs, caf_cc_stacks), stg_cg_infos)
            <- {-# SCC "CoreToStg" #-}
               withTiming logger
                   (text "CoreToStg"<+>brackets (ppr this_mod))
                   (\(a, b, (c,d), tag_env) ->
                        a `seqList`
                        b `seq`
                        c `seqList`
                        d `seqList`
                        (seqEltsUFM (seqTagSig) tag_env))
                   (myCoreToStg logger dflags (interactiveInScope (hsc_IC hsc_env)) False this_mod mod_loc prepd_binds)

        let (stg_binds,_stg_deps) = unzip stg_binds_with_deps

        let cost_centre_info =
              (late_local_ccs ++ caf_ccs, caf_cc_stacks)
            platform = targetPlatform dflags
            prof_init
              | sccProfilingEnabled dflags = profilingInitCode platform this_mod cost_centre_info
              | otherwise = mempty

        ------------------  Code generation ------------------
        -- The back-end is streamed: each top-level function goes
        -- from Stg all the way to asm before dealing with the next
        -- top-level function, so withTiming isn't very useful here.
        -- Hence we have one withTiming for the whole backend, the
        -- next withTiming after this will be "Assembler" (hard code only).
        withTiming logger (text "CodeGen"<+>brackets (ppr this_mod)) (const ())
         $ case backendCodeOutput (backend dflags) of
            JSCodeOutput ->
              do
              let js_config = initStgToJSConfig dflags

                  -- The JavaScript backend does not create CmmCgInfos like the Cmm backend,
                  -- but it is needed for writing the interface file. Here we compute a very
                  -- conservative but correct value.
                  lf_infos (StgTopLifted (StgNonRec b _)) = [(idName b, LFUnknown True)]
                  lf_infos (StgTopLifted (StgRec bs))     = map (\(b,_) -> (idName b, LFUnknown True)) bs
                  lf_infos (StgTopStringLit b _)          = [(idName b, LFUnlifted)]

                  cmm_cg_infos  = CmmCgInfos
                    { cgNonCafs = mempty
                    , cgLFInfos = mkNameEnv (concatMap lf_infos stg_binds)
                    , cgIPEStub = mempty
                    }
                  stub_c_exists = Nothing
                  foreign_fps   = []

              putDumpFileMaybe logger Opt_D_dump_stg_final "Final STG:" FormatSTG
                  (pprGenStgTopBindings (initStgPprOpts dflags) stg_binds)

              -- do the unfortunately effectual business
              stgToJS logger js_config stg_binds this_mod spt_entries foreign_stubs0 cost_centre_info output_filename
              return (output_filename, stub_c_exists, foreign_fps, Just stg_cg_infos, Just cmm_cg_infos)

            _          ->
              do
              cmms <- {-# SCC "StgToCmm" #-}
                doCodeGen hsc_env this_mod denv tycons
                cost_centre_info
                stg_binds

              ------------------  Code output -----------------------
              rawcmms0 <- {-# SCC "cmmToRawCmm" #-}
                case cmmToRawCmmHook hooks of
                  Nothing -> cmmToRawCmm logger profile cmms
                  Just h  -> h dflags (Just this_mod) cmms

              let dump a = do
                    unless (null a) $ putDumpFileMaybe logger Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
                    return a
                  rawcmms1 = Stream.mapM (liftIO . dump) rawcmms0

              let foreign_stubs st = foreign_stubs0
                                     `appendStubC` prof_init
                                     `appendStubC` cgIPEStub st

              (output_filename, (_stub_h_exists, stub_c_exists), foreign_fps, cmm_cg_infos)
                  <- {-# SCC "codeOutput" #-}
                    codeOutput logger tmpfs llvm_config dflags (hsc_units hsc_env) this_mod output_filename mod_loc
                    foreign_stubs foreign_files dependencies (initDUniqSupply 'n' 0) rawcmms1
              return  ( output_filename, stub_c_exists, foreign_fps
                      , Just stg_cg_infos, Just cmm_cg_infos)


-- The part of CgGuts that we need for HscInteractive
data CgInteractiveGuts = CgInteractiveGuts { cgi_module :: Module
                                           , cgi_binds  :: CoreProgram
                                           , cgi_tycons :: [TyCon]
                                           , cgi_foreign :: ForeignStubs
                                           , cgi_foreign_files :: [(ForeignSrcLang, FilePath)]
                                           , cgi_modBreaks ::  Maybe ModBreaks
                                           , cgi_spt_entries :: [SptEntry]
                                           , cgi_hpc_info :: HpcInfo
                                           }

mkCgInteractiveGuts :: CgGuts -> CgInteractiveGuts
mkCgInteractiveGuts CgGuts{cg_module, cg_binds, cg_tycons, cg_foreign, cg_foreign_files, cg_modBreaks, cg_spt_entries, cg_hpc_info}
  = CgInteractiveGuts cg_module cg_binds cg_tycons cg_foreign cg_foreign_files cg_modBreaks cg_spt_entries cg_hpc_info


hscInteractive :: HscEnv
               -> CgInteractiveGuts
               -> ModLocation
               -> IO (Maybe FilePath, CompiledByteCode) -- ^ .c stub path (if any) and ByteCode
hscInteractive hsc_env cgguts location = do
    comp_bc <- hscGenerateByteCode hsc_env cgguts location

    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let tmpfs  = hsc_tmpfs hsc_env
    ------------------ Create f-x-dynamic C-side stuff -----
    (_istub_h_exists, istub_c_exists)
        <- outputForeignStubs logger tmpfs dflags (hsc_units hsc_env) (cgi_module cgguts) location (cgi_foreign cgguts)
    return (istub_c_exists, comp_bc)


-- | Generate 'CompiledByteCode' (only) for a given module.
hscGenerateByteCode :: HscEnv -> CgInteractiveGuts -> ModLocation -> IO CompiledByteCode
hscGenerateByteCode hsc_env cgguts location = do
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let platform = targetPlatform dflags
    let CgInteractiveGuts{ -- This is the last use of the ModGuts in a compilation.
                -- From now on, we just use the bits we need.
               cgi_module   = this_mod,
               cgi_binds    = core_binds,
               cgi_tycons   = tycons,
               cgi_modBreaks = mod_breaks,
               cgi_spt_entries = spt_entries,
               cgi_hpc_info = hpc_info } = cgguts


    -------------------
    -- ADD IMPLICIT BINDINGS
    let cp_pgm_cfg :: CorePrepPgmConfig
        cp_pgm_cfg = initCorePrepPgmConfig (hsc_dflags hsc_env)
                                           (interactiveInScope $ hsc_IC hsc_env)
    binds_to_prep <- addImplicitBinds cp_pgm_cfg location tycons core_binds

    -------------------
    -- PREPARE FOR CODE GENERATION
    -- Do saturation and convert to A-normal form
    cp_cfg <- initCorePrepConfig hsc_env
    prepd_binds <- {-# SCC "CorePrep" #-}
                   corePrepPgm (hsc_logger hsc_env) cp_cfg cp_pgm_cfg
                               this_mod binds_to_prep

    -- The stg cg info only provides a runtime benfit, but is not requires so we just
    -- omit it here
    (stg_binds_with_deps, _infotable_prov, _caf_ccs__caf_cc_stacks, _ignore_stg_cg_infos)
      <- {-# SCC "CoreToStg" #-}
          myCoreToStg logger dflags (interactiveInScope (hsc_IC hsc_env)) True this_mod location prepd_binds

    let (stg_binds,_stg_deps) = unzip stg_binds_with_deps

    -------------------
    -- Setup HPC info
    let
      -- Strict to not retain a reference to the 'cgguts' via 'hpc_info'
      !bytecodeHpcInfo = case hpc_info of
        NoHpcInfo -> Strict.Nothing
        HpcInfo{hpcInfoTickCount, hpcInfoHash} ->
          Strict.Just ByteCodeHpcInfo
            { bchi_tick_count = hpcInfoTickCount
            , bchi_hash = hpcInfoHash
            , bchi_tickbox_name = utf8EncodeShortByteString $ Coverage.mkHpcTickBoxesLabell platform this_mod
            , bchi_module_name = utf8EncodeShortByteString $ Coverage.mkHpcModuleLabel this_mod
            }

    -----------------  Generate byte code ------------------
    byteCodeGen hsc_env this_mod stg_binds tycons mod_breaks spt_entries bytecodeHpcInfo


-- | Generate a byte code object linkable and write it to a file if `-fwrite-byte-code` is enabled.
generateAndWriteByteCode :: HscEnv -> CgInteractiveGuts -> ModLocation -> IO ModuleByteCode
generateAndWriteByteCode hsc_env cgguts mod_location = do
  comp_bc <- mkModuleByteCode hsc_env (cgi_module cgguts) mod_location cgguts
  let dflags   = hsc_dflags hsc_env
  -- See Note [-fwrite-byte-code is not the default]
  when (gopt Opt_WriteByteCode dflags) $ do
    let bc_path = ml_bytecode_file mod_location
    ByteCode.writeBinByteCode bc_path comp_bc
  return comp_bc

{-
Note [-fwrite-byte-code is not the default]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`-fwrite-byte-code` is not enabled by default because previously using `-fbyte-code` would
not write anything at all to disk. For example, GHCi would not write anything to the directory
it was invoked in. Therefore it was the backwards compatible thing to do to not write anything and
make user's opt into writing the files.

-}

-- | Generate a 'ModuleByteCode' and write it to disk if `-fwrite-byte-code` is enabled.
generateAndWriteByteCodeLinkable :: HscEnv -> CgInteractiveGuts -> ModLocation -> IO (LinkableWith ModuleByteCode)
generateAndWriteByteCodeLinkable hsc_env cgguts mod_location = do
  bco_object <- generateAndWriteByteCode hsc_env cgguts mod_location
  -- Either, get the same time as the .gbc file if it exists, or just the current time.
  -- It's important the time of the linkable matches the time of the .gbc file for recompilation
  -- checking.
  bco_time <- maybe getCurrentTime pure =<< modificationTimeIfExists (ml_bytecode_file_ospath mod_location)
  return $ mkOnlyModuleByteCodeLinkable bco_time bco_object

mkModuleByteCode :: HscEnv -> Module -> ModLocation -> CgInteractiveGuts -> IO ModuleByteCode
mkModuleByteCode hsc_env mod mod_location cgguts = do
  bcos <- hscGenerateByteCode hsc_env cgguts mod_location
  objs <- outputAndCompileForeign hsc_env mod mod_location (cgi_foreign_files cgguts) (cgi_foreign cgguts)
  ByteCode.mkModuleByteCode mod bcos objs

-- | Generate a fresh 'ModuleByteCode' for a given module but do not write it to disk.
generateFreshByteCodeLinkable :: HscEnv
  -> ModuleName
  -> CgInteractiveGuts
  -> ModLocation
  -> IO Linkable
generateFreshByteCodeLinkable hsc_env mod_name cgguts mod_location = do
  bco_time <- getCurrentTime
  bco_object <- mkModuleByteCode hsc_env (mkHomeModule (hsc_home_unit hsc_env) mod_name) mod_location cgguts
  return $ mkModuleByteCodeLinkable bco_time bco_object
------------------------------

hscCompileCmmFile :: HscEnv -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
hscCompileCmmFile hsc_env original_filename filename output_filename = runHsc hsc_env $ do
    let dflags   = hsc_dflags hsc_env
        logger   = hsc_logger hsc_env
        hooks    = hsc_hooks hsc_env
        tmpfs    = hsc_tmpfs hsc_env
        profile  = targetProfile dflags
        home_unit = hsc_home_unit hsc_env
        platform  = targetPlatform dflags
        llvm_config = hsc_llvm_config hsc_env
        cmm_config = initCmmConfig dflags
        do_info_table = gopt Opt_InfoTableMap dflags
        -- Make up a module name to give the NCG. We can't pass bottom here
        -- lest we reproduce #11784.
        mod_name = mkModuleName $ "Cmm$" ++ original_filename
        cmm_mod = mkHomeModule home_unit mod_name
        cmmpConfig = initCmmParserConfig dflags
    (dcmm, ipe_ents) <- ioMsgMaybe
               $ do
                  (warns,errs,cmm) <- withTiming logger (text "ParseCmm"<+>brackets (text filename)) (\_ -> ())
                                       $ parseCmmFile cmmpConfig cmm_mod home_unit filename
                  let msgs = warns `unionMessages` errs
                  return (GhcPsMessage <$> msgs, cmm)
    -- Probably need to rename cmm here
    let cmm = removeDeterm dcmm
    liftIO $ do
        putDumpFileMaybe logger Opt_D_dump_cmm_verbose_by_proc "Parsed Cmm" FormatCMM (pdoc platform cmm)

        -- Compile decls in Cmm files one decl at a time, to avoid re-ordering
        -- them in SRT analysis.
        --
        -- Re-ordering here causes breakage when booting with C backend because
        -- in C we must declare before use, but SRT algorithm is free to
        -- re-order [A, B] (B refers to A) when A is not CAFFY and return [B, A]
        ((_,dus1), cmmgroup) <- second concat <$>
          mapAccumLM (\(msrt0, dus0) cmm -> do
            ((msrt1, cmm'), dus1) <- cmmPipeline logger cmm_config msrt0 [cmm] dus0
            return ((msrt1, dus1), cmm')) (emptySRT cmm_mod, initDUniqSupply 'u' 0) cmm

        unless (null cmmgroup) $
          putDumpFileMaybe logger Opt_D_dump_cmm "Output Cmm"
            FormatCMM (pdoc platform cmmgroup)

        rawCmms0 <- case cmmToRawCmmHook hooks of
          Nothing -> cmmToRawCmm logger profile (Stream.yield cmmgroup)
          Just h  -> h           dflags Nothing (Stream.yield cmmgroup)

        let dump a = do
              unless (null a) $ putDumpFileMaybe logger Opt_D_dump_cmm_raw "Raw Cmm" FormatCMM (pdoc platform a)
              return a
            rawCmms = Stream.mapM (liftIO . dump) rawCmms0

        let foreign_stubs _
              | not $ null ipe_ents =
                  let ip_init = ipInitCode do_info_table platform cmm_mod
                  in NoStubs `appendStubC` ip_init
              | otherwise     = NoStubs
        (_output_filename, (_stub_h_exists, stub_c_exists), _foreign_fps, _caf_infos)
          <- codeOutput logger tmpfs llvm_config dflags (hsc_units hsc_env) cmm_mod output_filename no_loc foreign_stubs [] S.empty
             dus1 rawCmms
        return stub_c_exists
  where
    no_loc = OsPathModLocation
        { ml_hs_file_ospath  = Just $ unsafeEncodeUtf original_filename,
          ml_hi_file_ospath  = panic "hscCompileCmmFile: no hi file",
          ml_obj_file_ospath = panic "hscCompileCmmFile: no obj file",
          ml_dyn_obj_file_ospath = panic "hscCompileCmmFile: no dyn obj file",
          ml_dyn_hi_file_ospath  = panic "hscCompileCmmFile: no dyn obj file",
          ml_hie_file_ospath = panic "hscCompileCmmFile: no hie file",
          ml_bytecode_file_ospath = panic "hscCompileCmmFile: no bytecode file"
          }

-------------------- Stuff for new code gen ---------------------

{-
Note [Forcing of stg_binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The two last steps in the STG pipeline are:

* Sorting the bindings in dependency order.
* Annotating them with free variables.

We want to make sure we do not keep references to unannotated STG bindings
alive, nor references to bindings which have already been compiled to Cmm.

We explicitly force the bindings to avoid this.

This reduces residency towards the end of the CodeGen phase significantly
(5-10%).
-}

doCodeGen :: HscEnv -> Module -> InfoTableProvMap -> [TyCon]
          -> CollectedCCs
          -> [CgStgTopBinding] -- ^ Bindings come already annotated with fvs
          -> IO (CgStream CmmGroupSRTs CmmCgInfos)
         -- Note we produce a 'Stream' of CmmGroups, so that the
         -- backend can be run incrementally.  Otherwise it generates all
         -- the C-- up front, which has a significant space cost.
doCodeGen hsc_env this_mod denv tycons
              cost_centre_info stg_binds_w_fvs = do
    let dflags     = hsc_dflags hsc_env
        logger     = hsc_logger hsc_env
        hooks      = hsc_hooks  hsc_env
        tmpfs      = hsc_tmpfs  hsc_env
        platform   = targetPlatform dflags
        stg_ppr_opts = (initStgPprOpts dflags)

    putDumpFileMaybe logger Opt_D_dump_stg_final "Final STG:" FormatSTG
        (pprGenStgTopBindings stg_ppr_opts stg_binds_w_fvs)

    let stg_to_cmm dflags mod a b c d = case stgToCmmHook hooks of
          Nothing -> StgToCmm.codeGen logger tmpfs (initStgToCmmConfig dflags mod) a b c d
          Just h  -> (,emptyDetUFM) <$> h          (initStgToCmmConfig dflags mod) a b c d

    let cmm_stream :: CgStream CmmGroup (ModuleLFInfos, DetUniqFM)
        -- See Note [Forcing of stg_binds]
        cmm_stream = stg_binds_w_fvs `seqList` {-# SCC "StgToCmm" #-}
            stg_to_cmm dflags this_mod denv tycons cost_centre_info stg_binds_w_fvs

        -- codegen consumes a stream of CmmGroup, and produces a new
        -- stream of CmmGroup (not necessarily synchronised: one
        -- CmmGroup on input may produce many CmmGroups on output due
        -- to proc-point splitting).

    let dump1 a = do
          unless (null a) $
            putDumpFileMaybe logger Opt_D_dump_cmm_from_stg
              "Cmm produced by codegen" FormatCMM (pdoc platform a)
          return a

        ppr_stream1 = Stream.mapM (liftIO . dump1) cmm_stream

        cmm_config = initCmmConfig dflags

        pipeline_stream :: CgStream CmmGroupSRTs CmmCgInfos
        pipeline_stream = do
          ((mod_srt_info, ipes, ipe_stats), (lf_infos, detRnEnv)) <-
            {-# SCC "cmmPipeline" #-}
            Stream.mapAccumL_ (pipeline_action logger cmm_config) (emptySRT this_mod, M.empty, mempty) ppr_stream1

          let nonCaffySet = srtMapNonCAFs (moduleSRTMap mod_srt_info)

              -- denv::InfoTableProvMap refers to symbols that no longer exist
              -- if -fobject-determinism is on, since it was created before the
              -- Cmm was renamed. Update all the symbols by renaming them with
              -- the renaming map in that case.
              (_drn, rn_denv)
                | gopt Opt_ObjectDeterminism dflags = detRenameIPEMap detRnEnv denv
                | otherwise = (detRnEnv, denv)

          cmmCgInfos <- generateCgIPEStub hsc_env this_mod rn_denv (nonCaffySet, lf_infos, ipes, ipe_stats)
          return cmmCgInfos

        pipeline_action
          :: Logger
          -> CmmConfig
          -> (ModuleSRTInfo, Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
          -> CmmGroup
          -> UniqDSMT IO ((ModuleSRTInfo, Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats), CmmGroupSRTs)
        pipeline_action logger cmm_config (mod_srt_info, ipes, stats) cmm_group = do
          (mod_srt_info', cmm_srts) <- withDUS $ cmmPipeline logger cmm_config mod_srt_info cmm_group

          -- If -finfo-table-map is enabled, we precompute a map from info
          -- tables to source locations. See Note [Mapping Info Tables to Source
          -- Positions] in GHC.Stg.Debug.
          (ipes', stats') <-
            if (gopt Opt_InfoTableMap dflags) then
              liftIO $ lookupEstimatedTicks hsc_env ipes stats cmm_srts
            else
              return (ipes, stats)

          return ((mod_srt_info', ipes', stats'), cmm_srts)

        dump2 a = do
          unless (null a) $
            putDumpFileMaybe logger Opt_D_dump_cmm "Output Cmm" FormatCMM (pdoc platform a)
          return a

    return $ Stream.mapM (liftIO . dump2) pipeline_stream

-- | Get the final STG from Core.
myCoreToStg :: Logger -> DynFlags -> [Var]
            -> Bool
            -> Module -> ModLocation -> CoreProgram
            -> IO ( [(CgStgTopBinding,IdSet)] -- output program and its dependencies
                  , InfoTableProvMap
                  , CollectedCCs -- CAF cost centre info (declared and used)
                  , StgCgInfos )
myCoreToStg logger dflags ic_inscope for_bytecode this_mod ml prepd_binds = do
    (stg_binds, denv, cost_centre_info)
       <- {-# SCC "Core2Stg" #-}
           coreToStg (initCoreToStgOpts dflags) this_mod ml prepd_binds

    (stg_binds_with_fvs,stg_cg_info)
        <- {-# SCC "Stg2Stg" #-}
           stg2stg logger ic_inscope (initStgPipelineOpts dflags for_bytecode)
                   this_mod stg_binds

    putDumpFileMaybe logger Opt_D_dump_stg_cg "CodeGenInput STG:" FormatSTG
        (pprGenStgTopBindings (initStgPprOpts dflags) (fmap fst stg_binds_with_fvs))

    return (stg_binds_with_fvs, denv, cost_centre_info, stg_cg_info)
