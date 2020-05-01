{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section{Code output phase}
-}

{-# LANGUAGE CPP #-}

module GHC.Driver.CodeOutput
   ( codeOutput
   , outputForeignStubs
   , profilingInitCode
   )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.CmmToAsm     ( nativeCodeGen )
import GHC.CmmToLlvm    ( llvmCodeGen )

import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

import GHC.Driver.Finder    ( mkStubPaths )
import GHC.CmmToC           ( writeC )
import GHC.Cmm.Lint         ( cmmLint )
import GHC.Cmm              ( RawCmmGroup )
import GHC.Cmm.CLabel
import GHC.Driver.Types
import GHC.Driver.Session
import GHC.Data.Stream           ( Stream )
import qualified GHC.Data.Stream as Stream
import GHC.SysTools.FileCleanup

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Unit
import GHC.Types.SrcLoc
import GHC.Types.CostCentre

import Control.Exception
import System.Directory
import System.FilePath
import System.IO

{-
************************************************************************
*                                                                      *
\subsection{Steering}
*                                                                      *
************************************************************************
-}

codeOutput :: DynFlags
           -> Module
           -> FilePath
           -> ModLocation
           -> ForeignStubs
           -> [(ForeignSrcLang, FilePath)]
           -- ^ additional files to be compiled with with the C compiler
           -> [UnitId]
           -> Stream IO RawCmmGroup a                       -- Compiled C--
           -> IO (FilePath,
                  (Bool{-stub_h_exists-}, Maybe FilePath{-stub_c_exists-}),
                  [(ForeignSrcLang, FilePath)]{-foreign_fps-},
                  a)

codeOutput dflags this_mod filenm location foreign_stubs foreign_fps pkg_deps
  cmm_stream
  =
    do  {
        -- Lint each CmmGroup as it goes past
        ; let linted_cmm_stream =
                 if gopt Opt_DoCmmLinting dflags
                    then Stream.mapM do_lint cmm_stream
                    else cmm_stream

              do_lint cmm = withTimingSilent
                  dflags
                  (text "CmmLint"<+>brackets (ppr this_mod))
                  (const ()) $ do
                { case cmmLint dflags cmm of
                        Just err -> do { log_action dflags
                                                   dflags
                                                   NoReason
                                                   SevDump
                                                   noSrcSpan
                                                   $ withPprStyle defaultDumpStyle err
                                       ; ghcExit dflags 1
                                       }
                        Nothing  -> return ()
                ; return cmm
                }

        ; stubs_exist <- outputForeignStubs dflags this_mod location foreign_stubs
        ; a <- case hscTarget dflags of
                 HscAsm         -> outputAsm dflags this_mod location filenm
                                             linted_cmm_stream
                 HscC           -> outputC dflags filenm linted_cmm_stream pkg_deps
                 HscLlvm        -> outputLlvm dflags filenm linted_cmm_stream
                 HscInterpreted -> panic "codeOutput: HscInterpreted"
                 HscNothing     -> panic "codeOutput: HscNothing"
        ; return (filenm, stubs_exist, foreign_fps, a)
        }

doOutput :: String -> (Handle -> IO a) -> IO a
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action

{-
************************************************************************
*                                                                      *
\subsection{C}
*                                                                      *
************************************************************************
-}

outputC :: DynFlags
        -> FilePath
        -> Stream IO RawCmmGroup a
        -> [UnitId]
        -> IO a

outputC dflags filenm cmm_stream packages
  = do
       withTiming dflags (text "C codegen") (\a -> seq a () {- FIXME -}) $ do

         -- figure out which header files to #include in the generated .hc file:
         --
         --   * extra_includes from packages
         --   * -#include options from the cmdline and OPTIONS pragmas
         --   * the _stub.h file, if there is one.
         --
         let rts = unsafeGetUnitInfo dflags rtsUnitId

         let cc_injects = unlines (map mk_include (unitIncludes rts))
             mk_include h_file =
              case h_file of
                 '"':_{-"-} -> "#include "++h_file
                 '<':_      -> "#include "++h_file
                 _          -> "#include \""++h_file++"\""

         let pkg_names = map unitIdString packages

         doOutput filenm $ \ h -> do
            hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
            hPutStr h cc_injects
            Stream.consume cmm_stream (writeC dflags h)

{-
************************************************************************
*                                                                      *
\subsection{Assembler}
*                                                                      *
************************************************************************
-}

outputAsm :: DynFlags -> Module -> ModLocation -> FilePath
          -> Stream IO RawCmmGroup a
          -> IO a
outputAsm dflags this_mod location filenm cmm_stream
 | platformMisc_ghcWithNativeCodeGen $ platformMisc dflags
  = do ncg_uniqs <- mkSplitUniqSupply 'n'

       debugTraceMsg dflags 4 (text "Outputing asm to" <+> text filenm)

       {-# SCC "OutputAsm" #-} doOutput filenm $
           \h -> {-# SCC "NativeCodeGen" #-}
                 nativeCodeGen dflags this_mod location h ncg_uniqs cmm_stream

 | otherwise
  = panic "This compiler was built without a native code generator"

{-
************************************************************************
*                                                                      *
\subsection{LLVM}
*                                                                      *
************************************************************************
-}

outputLlvm :: DynFlags -> FilePath -> Stream IO RawCmmGroup a -> IO a
outputLlvm dflags filenm cmm_stream
  = do {-# SCC "llvm_output" #-} doOutput filenm $
           \f -> {-# SCC "llvm_CodeGen" #-}
                 llvmCodeGen dflags f cmm_stream

{-
************************************************************************
*                                                                      *
\subsection{Foreign import/export}
*                                                                      *
************************************************************************
-}

outputForeignStubs :: DynFlags -> Module -> ModLocation -> ForeignStubs
                   -> IO (Bool,         -- Header file created
                          Maybe FilePath) -- C file created
outputForeignStubs dflags mod location stubs
 = do
   let stub_h = mkStubPaths dflags (moduleName mod) location
   stub_c <- newTempName dflags TFL_CurrentModule "c"

   case stubs of
     NoStubs ->
        return (False, Nothing)

     ForeignStubs h_code c_code -> do
        let
            stub_c_output_d = pprCode CStyle c_code
            stub_c_output_w = showSDoc dflags stub_c_output_d

            -- Header file protos for "foreign export"ed functions.
            stub_h_output_d = pprCode CStyle h_code
            stub_h_output_w = showSDoc dflags stub_h_output_d

        createDirectoryIfMissing True (takeDirectory stub_h)

        dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file"
                      FormatC
                      stub_h_output_d

        -- we need the #includes from the rts package for the stub files
        let rts_includes =
               let rts_pkg = unsafeGetUnitInfo dflags rtsUnitId in
               concatMap mk_include (unitIncludes rts_pkg)
            mk_include i = "#include \"" ++ i ++ "\"\n"

            -- wrapper code mentions the ffi_arg type, which comes from ffi.h
            ffi_includes
              | platformMisc_libFFI $ platformMisc dflags = "#include <ffi.h>\n"
              | otherwise = ""

        stub_h_file_exists
           <- outputForeignStubs_help stub_h stub_h_output_w
                ("#include <HsFFI.h>\n" ++ cplusplus_hdr) cplusplus_ftr

        dumpIfSet_dyn dflags Opt_D_dump_foreign
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


-- Don't use doOutput for dumping the f. export stubs
-- since it is more than likely that the stubs file will
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
profilingInitCode :: DynFlags -> Module -> CollectedCCs -> SDoc
profilingInitCode dflags this_mod (local_CCs, singleton_CCSs)
 = if not (gopt Opt_SccProfilingOn dflags)
   then empty
   else vcat
    $  map emit_cc_decl local_CCs
    ++ map emit_ccs_decl singleton_CCSs
    ++ [emit_cc_list local_CCs]
    ++ [emit_ccs_list singleton_CCSs]
    ++ [ text "static void prof_init_" <> ppr this_mod
            <> text "(void) __attribute__((constructor));"
       , text "static void prof_init_" <> ppr this_mod <> text "(void)"
       , braces (vcat
                 [ text "registerCcList" <> parens local_cc_list_label <> semi
                 , text "registerCcsList" <> parens singleton_cc_list_label <> semi
                 ])
       ]
 where
   emit_cc_decl cc =
       text "extern CostCentre" <+> cc_lbl <> text "[];"
     where cc_lbl = ppr (mkCCLabel cc)
   local_cc_list_label = text "local_cc_" <> ppr this_mod
   emit_cc_list ccs =
      text "static CostCentre *" <> local_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ ppr (mkCCLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi

   emit_ccs_decl ccs =
       text "extern CostCentreStack" <+> ccs_lbl <> text "[];"
     where ccs_lbl = ppr (mkCCSLabel ccs)
   singleton_cc_list_label = text "singleton_cc_" <> ppr this_mod
   emit_ccs_list ccs =
      text "static CostCentreStack *" <> singleton_cc_list_label <> text "[] ="
      <+> braces (vcat $ [ ppr (mkCCSLabel cc) <> comma
                         | cc <- ccs
                         ] ++ [text "NULL"])
      <> semi
