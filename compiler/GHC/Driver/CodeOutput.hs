{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section{Code output phase}
-}

{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Driver.CodeOutput
   ( codeOutput
   , outputForeignStubs
   , profilingInitCode
   , ipInitCode
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.ForeignSrcLang
import GHC.Data.FastString

import GHC.CmmToAsm     ( nativeCodeGen )
import GHC.CmmToLlvm    ( llvmCodeGen )

import GHC.CmmToC           ( cmmToC )
import GHC.Cmm.Lint         ( cmmLint )
import GHC.Cmm
import GHC.Cmm.CLabel

import GHC.Driver.DynFlags
import GHC.Driver.Config.Finder    ( initFinderOpts   )
import GHC.Driver.Config.CmmToAsm  ( initNCGConfig    )
import GHC.Driver.Config.CmmToLlvm ( initLlvmCgConfig )
import GHC.Driver.LlvmConfigCache  (LlvmConfigCache)
import GHC.Driver.Ppr
import GHC.Driver.Backend

import GHC.Data.OsPath
import qualified GHC.Data.ShortText as ST
import GHC.Data.Stream           ( Stream )
import qualified GHC.Data.Stream as Stream

import GHC.Utils.TmpFs


import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Exception ( bracket )
import GHC.Utils.Ppr (Mode(..))
import GHC.Utils.Panic.Plain ( pgmError )

import GHC.Unit
import GHC.Unit.Finder      ( mkStubPaths )

import GHC.Types.SrcLoc
import GHC.Types.CostCentre
import GHC.Types.ForeignStubs
import GHC.Types.Unique.DSM

import System.Directory
import System.FilePath
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
\subsection{Steering}
*                                                                      *
************************************************************************
-}

codeOutput
    :: forall a.
       Logger
    -> TmpFs
    -> LlvmConfigCache
    -> DynFlags
    -> UnitState
    -> Module
    -> FilePath
    -> ModLocation
    -> (a -> ForeignStubs)
    -> [(ForeignSrcLang, FilePath)]
    -- ^ additional files to be compiled with the C compiler
    -> Set UnitId -- ^ Dependencies
    -> Stream IO RawCmmGroup a                       -- Compiled C--
    -> IO (FilePath,
           (Bool{-stub_h_exists-}, Maybe FilePath{-stub_c_exists-}),
           [(ForeignSrcLang, FilePath)]{-foreign_fps-},
           a)
codeOutput logger tmpfs llvm_config dflags unit_state this_mod filenm location genForeignStubs foreign_fps pkg_deps
  cmm_stream
  =
    do  {

        -- Lint each CmmGroup as it goes past
        ; let linted_cmm_stream =
                 if gopt Opt_DoCmmLinting dflags
                    then Stream.mapM do_lint cmm_stream
                    else cmm_stream

              do_lint cmm = withTimingSilent logger
                  (text "CmmLint"<+>brackets (ppr this_mod))
                  (const ()) $ do
                { case cmmLint (targetPlatform dflags) cmm of
                        Just err -> do { logMsg logger
                                                   MCInfo -- See Note [MCInfo for Lint] in "GHC.Core.Lint"
                                                   noSrcSpan
                                                   $ withPprStyle defaultDumpStyle err
                                       ; ghcExit logger 1
                                       }
                        Nothing  -> return ()
                ; return cmm
                }

        ; let final_stream :: Stream IO RawCmmGroup (ForeignStubs, a)
              final_stream = do
                  { a <- linted_cmm_stream
                  ; let stubs = genForeignStubs a
                  ; emitInitializerDecls this_mod stubs
                  ; return (stubs, a) }

        ; (stubs, a) <- case backendCodeOutput (backend dflags) of
                 NcgCodeOutput  -> outputAsm logger dflags this_mod location filenm
                                             final_stream
                 ViaCCodeOutput -> outputC logger dflags filenm final_stream pkg_deps
                 LlvmCodeOutput -> outputLlvm logger llvm_config dflags filenm final_stream
                 JSCodeOutput   -> outputJS logger llvm_config dflags filenm final_stream
        ; stubs_exist <- outputForeignStubs logger tmpfs dflags unit_state this_mod location stubs
        ; return (filenm, stubs_exist, foreign_fps, a)
        }

-- | See Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini for details.
emitInitializerDecls :: Module -> ForeignStubs -> Stream IO RawCmmGroup ()
emitInitializerDecls this_mod (ForeignStubs _ cstub)
  | initializers <- getInitializers cstub
  , not $ null initializers =
      let init_array = CmmData sect statics
          lbl = mkInitializerArrayLabel this_mod
          sect = Section InitArray lbl
          statics = CmmStaticsRaw lbl
            [ CmmStaticLit $ CmmLabel fn_name
            | fn_name <- initializers
            ]
    in Stream.yield [init_array]
emitInitializerDecls _ _ = return ()

doOutput :: String -> (Handle -> IO a) -> IO a
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action

{-
************************************************************************
*                                                                      *
\subsection{C}
*                                                                      *
************************************************************************
-}

outputC :: Logger
        -> DynFlags
        -> FilePath
        -> Stream IO RawCmmGroup a
        -> Set UnitId
        -> IO a
outputC logger dflags filenm cmm_stream unit_deps =
  withTiming logger (text "C codegen") (\a -> seq a () {- FIXME -}) $ do
    let pkg_names = map unitIdString (Set.toAscList unit_deps)
    doOutput filenm $ \ h -> do
      hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
      hPutStr h "#include \"Stg.h\"\n"
      let platform = targetPlatform dflags
          writeC cmm = do
            let doc = cmmToC platform cmm
            putDumpFileMaybe logger Opt_D_dump_c_backend
                          "C backend output"
                          FormatC
                          doc
            let ctx = initSDocContext dflags PprCode
            printSDocLn ctx LeftMode h doc
      Stream.consume cmm_stream id writeC

{-
************************************************************************
*                                                                      *
\subsection{Assembler}
*                                                                      *
************************************************************************
-}

outputAsm :: Logger
          -> DynFlags
          -> Module
          -> ModLocation
          -> FilePath
          -> Stream IO RawCmmGroup a
          -> IO a
outputAsm logger dflags this_mod location filenm cmm_stream = do
  let ncg_uniqs = initDUniqSupply 'n' 0 {- See Note [Cmm Local Deterministic Uniques], or should we receive it as input?-}
  debugTraceMsg logger 4 (text "Outputing asm to" <+> text filenm)
  let ncg_config = initNCGConfig dflags this_mod
  {-# SCC "OutputAsm" #-} doOutput filenm $
    \h -> {-# SCC "NativeCodeGen" #-}
      nativeCodeGen logger (toolSettings dflags) ncg_config location h ncg_uniqs cmm_stream

{-
************************************************************************
*                                                                      *
\subsection{LLVM}
*                                                                      *
************************************************************************
-}

outputLlvm :: Logger -> LlvmConfigCache -> DynFlags -> FilePath -> Stream IO RawCmmGroup a -> IO a
outputLlvm logger llvm_config dflags filenm cmm_stream = do
  lcg_config <- initLlvmCgConfig logger llvm_config dflags
  {-# SCC "llvm_output" #-} doOutput filenm $
    \f -> {-# SCC "llvm_CodeGen" #-}
      llvmCodeGen logger lcg_config f cmm_stream

{-
************************************************************************
*                                                                      *
\subsection{JavaScript}
*                                                                      *
************************************************************************
-}
outputJS :: Logger -> LlvmConfigCache -> DynFlags -> FilePath -> Stream IO RawCmmGroup a -> IO a
outputJS _ _ _ _ _ = pgmError $ "codeOutput: Hit JavaScript case. We should never reach here!"
                              ++ "\nThe JS backend should shortcircuit to StgToJS after Stg."
                              ++ "\nIf you reached this point then you've somehow made it to Cmm!"

{-
************************************************************************
*                                                                      *
\subsection{Foreign import/export}
*                                                                      *
************************************************************************
-}

{-
Note [Packaging libffi headers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The C code emitted by GHC for libffi adjustors must depend upon the ffi_arg type,
defined in <ffi.h>. For this reason, we must ensure that <ffi.h> is available
in binary distributions. To do so, we install these headers as part of the
`rts` package.
-}

outputForeignStubs
    :: Logger
    -> TmpFs
    -> DynFlags
    -> UnitState
    -> Module
    -> ModLocation
    -> ForeignStubs
    -> IO (Bool,         -- Header file created
           Maybe FilePath) -- C file created
outputForeignStubs logger tmpfs dflags unit_state mod location stubs
 = do
   let stub_h = unsafeDecodeUtf $ mkStubPaths (initFinderOpts dflags) (moduleName mod) location
   stub_c <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "c"

   case stubs of
     NoStubs ->
        return (False, Nothing)

     ForeignStubs (CHeader h_code) (CStub c_code _ _) -> do
        let
            stub_c_output_d = pprCode c_code
            stub_c_output_w = showSDoc dflags stub_c_output_d

            -- Header file protos for "foreign export"ed functions.
            stub_h_output_d = pprCode h_code
            stub_h_output_w = showSDoc dflags stub_h_output_d

        createDirectoryIfMissing True (takeDirectory stub_h)

        putDumpFileMaybe logger Opt_D_dump_foreign
                      "Foreign export header file"
                      FormatC
                      stub_h_output_d

        -- we need the #includes from the rts package for the stub files
        let rts_includes =
               let mrts_pkg = lookupUnitId unit_state rtsUnitId
                   mk_include i = "#include \"" ++ ST.unpack i ++ "\"\n"
               in case mrts_pkg of
                    Just rts_pkg -> concatMap mk_include (unitIncludes rts_pkg)
                    -- This case only happens when compiling foreign stub for the rts
                    -- library itself. The only time we do this at the moment is for
                    -- IPE information for the RTS info tables
                    Nothing -> ""

            -- wrapper code mentions the ffi_arg type, which comes from ffi.h
            ffi_includes
              | platformMisc_libFFI $ platformMisc dflags = "#include \"rts/ghc_ffi.h\"\n"
              | otherwise = ""

        stub_h_file_exists
           <- outputForeignStubs_help stub_h stub_h_output_w
                ("#include <HsFFI.h>\n" ++ cplusplus_hdr) cplusplus_ftr

        putDumpFileMaybe logger Opt_D_dump_foreign
                      "Foreign export stubs" FormatC stub_c_output_d

        stub_c_file_exists
           <- outputForeignStubs_help stub_c stub_c_output_w
                ("#define IN_STG_CODE 0\n" ++
                 "#include <Rts.h>\n" ++
                 rts_includes ++
                 ffi_includes ++
                 cplusplus_hdr)
                 cplusplus_ftr
           -- We're adding the default hc_header to the stub file, but this
           -- isn't really HC code, so we need to define IN_STG_CODE==0 to
           -- avoid the register variables etc. being enabled.

        return (stub_h_file_exists, if stub_c_file_exists
                                       then Just stub_c
                                       else Nothing )
 where
   cplusplus_hdr = "#if defined(__cplusplus)\nextern \"C\" {\n#endif\n"
   cplusplus_ftr = "#if defined(__cplusplus)\n}\n#endif\n"


-- It is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help :: FilePath -> String -> String -> String -> IO Bool
outputForeignStubs_help _fname ""      _header _footer = return False
outputForeignStubs_help fname doc_str header footer
   = do writeFile fname (header ++ doc_str ++ '\n':footer ++ "\n")
        return True

-- -----------------------------------------------------------------------------
-- Initialising cost centres

-- We must produce declarations for the cost-centres defined in this
-- module;

-- | Generate code to initialise cost centres
profilingInitCode :: Platform -> Module -> CollectedCCs -> CStub
profilingInitCode platform this_mod (local_CCs, singleton_CCSs)
 = {-# SCC profilingInitCode #-}
   initializerCStub platform fn_name decls body
 where
   pdocC = pprCLabel platform
   fn_name = mkInitializerStubLabel this_mod (fsLit "prof_init")
   decls = vcat
        $  map emit_cc_decl local_CCs
        ++ map emit_ccs_decl singleton_CCSs
        ++ [emit_cc_list local_CCs]
        ++ [emit_ccs_list singleton_CCSs]
   body = vcat
        [ text "registerCcList" <> parens local_cc_list_label <> semi
        , text "registerCcsList" <> parens singleton_cc_list_label <> semi
        ]
   emit_cc_decl cc =
       text "extern CostCentre" <+> cc_lbl <> text "[];"
     where cc_lbl = pdocC (mkCCLabel cc)
   local_cc_list_label = text "local_cc_" <> ppr this_mod
   emit_cc_list ccs =
      text "static CostCentre *" <> local_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ pdocC (mkCCLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi

   emit_ccs_decl ccs =
       text "extern CostCentreStack" <+> ccs_lbl <> text "[];"
     where ccs_lbl = pdocC (mkCCSLabel ccs)
   singleton_cc_list_label = text "singleton_cc_" <> ppr this_mod
   emit_ccs_list ccs =
      text "static CostCentreStack *" <> singleton_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ pdocC (mkCCSLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi

-- | Generate code to initialise info pointer origin
-- See Note [Mapping Info Tables to Source Positions]
ipInitCode
  :: Bool            -- is Opt_InfoTableMap enabled or not
  -> Platform
  -> Module
  -> CStub
ipInitCode do_info_table platform this_mod
  | not do_info_table = mempty
  | otherwise = initializerCStub platform fn_nm ipe_buffer_decl body
 where
   fn_nm = mkInitializerStubLabel this_mod (fsLit "ip_init")

   body = text "registerInfoProvList" <> parens (text "&" <> ipe_buffer_label) <> semi

   ipe_buffer_label = pprCLabel platform (mkIPELabel this_mod)

   ipe_buffer_decl =
       text "extern IpeBufferListNode" <+> ipe_buffer_label <> text ";"

