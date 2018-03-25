{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section{Code output phase}
-}

{-# LANGUAGE CPP #-}

module CodeOutput( codeOutput, outputForeignStubs ) where

#include "HsVersions.h"

import GhcPrelude

import AsmCodeGen ( nativeCodeGen )
import LlvmCodeGen ( llvmCodeGen )

import UniqSupply       ( mkSplitUniqSupply )

import Finder           ( mkStubPaths )
import PprC             ( writeCs )
import CmmLint          ( cmmLint )
import Packages
import Cmm              ( RawCmmGroup )
import HscTypes
import DynFlags
import Config
import Stream           (Stream)
import qualified Stream
import FileCleanup

import ErrUtils
import Outputable
import Module
import SrcLoc

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
           -> [InstalledUnitId]
           -> Stream IO RawCmmGroup ()                       -- Compiled C--
           -> IO (FilePath,
                  (Bool{-stub_h_exists-}, Maybe FilePath{-stub_c_exists-}),
                  [(ForeignSrcLang, FilePath)]{-foreign_fps-})

codeOutput dflags this_mod filenm location foreign_stubs foreign_fps pkg_deps
  cmm_stream
  =
    do  {
        -- Lint each CmmGroup as it goes past
        ; let linted_cmm_stream =
                 if gopt Opt_DoCmmLinting dflags
                    then Stream.mapM do_lint cmm_stream
                    else cmm_stream

              do_lint cmm = withTiming (pure dflags)
                                       (text "CmmLint"<+>brackets (ppr this_mod))
                                       (const ()) $ do
                { case cmmLint dflags cmm of
                        Just err -> do { log_action dflags
                                                   dflags
                                                   NoReason
                                                   SevDump
                                                   noSrcSpan
                                                   (defaultDumpStyle dflags)
                                                   err
                                       ; ghcExit dflags 1
                                       }
                        Nothing  -> return ()
                ; return cmm
                }

        ; stubs_exist <- outputForeignStubs dflags this_mod location foreign_stubs
        ; case hscTarget dflags of {
             HscAsm         -> outputAsm dflags this_mod location filenm
                                         linted_cmm_stream;
             HscC           -> outputC dflags filenm linted_cmm_stream pkg_deps;
             HscLlvm        -> outputLlvm dflags filenm linted_cmm_stream;
             HscInterpreted -> panic "codeOutput: HscInterpreted";
             HscNothing     -> panic "codeOutput: HscNothing"
          }
        ; return (filenm, stubs_exist, foreign_fps)
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
        -> Stream IO RawCmmGroup ()
        -> [InstalledUnitId]
        -> IO ()

outputC dflags filenm cmm_stream packages
  = do
       -- ToDo: make the C backend consume the C-- incrementally, by
       -- pushing the cmm_stream inside (c.f. nativeCodeGen)
       rawcmms <- Stream.collect cmm_stream

       -- figure out which header files to #include in the generated .hc file:
       --
       --   * extra_includes from packages
       --   * -#include options from the cmdline and OPTIONS pragmas
       --   * the _stub.h file, if there is one.
       --
       let rts = getPackageDetails dflags rtsUnitId

       let cc_injects = unlines (map mk_include (includes rts))
           mk_include h_file =
            case h_file of
               '"':_{-"-} -> "#include "++h_file
               '<':_      -> "#include "++h_file
               _          -> "#include \""++h_file++"\""

       let pkg_names = map installedUnitIdString packages

       doOutput filenm $ \ h -> do
          hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
          hPutStr h cc_injects
          writeCs dflags h rawcmms

{-
************************************************************************
*                                                                      *
\subsection{Assembler}
*                                                                      *
************************************************************************
-}

outputAsm :: DynFlags -> Module -> ModLocation -> FilePath
          -> Stream IO RawCmmGroup ()
          -> IO ()
outputAsm dflags this_mod location filenm cmm_stream
 | cGhcWithNativeCodeGen == "YES"
  = do ncg_uniqs <- mkSplitUniqSupply 'n'

       debugTraceMsg dflags 4 (text "Outputing asm to" <+> text filenm)

       _ <- {-# SCC "OutputAsm" #-} doOutput filenm $
           \h -> {-# SCC "NativeCodeGen" #-}
                 nativeCodeGen dflags this_mod location h ncg_uniqs cmm_stream
       return ()

 | otherwise
  = panic "This compiler was built without a native code generator"

{-
************************************************************************
*                                                                      *
\subsection{LLVM}
*                                                                      *
************************************************************************
-}

outputLlvm :: DynFlags -> FilePath -> Stream IO RawCmmGroup () -> IO ()
outputLlvm dflags filenm cmm_stream
  = do ncg_uniqs <- mkSplitUniqSupply 'n'

       {-# SCC "llvm_output" #-} doOutput filenm $
           \f -> {-# SCC "llvm_CodeGen" #-}
                 llvmCodeGen dflags f ncg_uniqs cmm_stream

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
                      "Foreign export header file" stub_h_output_d

        -- we need the #includes from the rts package for the stub files
        let rts_includes =
               let rts_pkg = getPackageDetails dflags rtsUnitId in
               concatMap mk_include (includes rts_pkg)
            mk_include i = "#include \"" ++ i ++ "\"\n"

            -- wrapper code mentions the ffi_arg type, which comes from ffi.h
            ffi_includes | cLibFFI   = "#include \"ffi.h\"\n"
                         | otherwise = ""

        stub_h_file_exists
           <- outputForeignStubs_help stub_h stub_h_output_w
                ("#include \"HsFFI.h\"\n" ++ cplusplus_hdr) cplusplus_ftr

        dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export stubs" stub_c_output_d

        stub_c_file_exists
           <- outputForeignStubs_help stub_c stub_c_output_w
                ("#define IN_STG_CODE 0\n" ++
                 "#include \"Rts.h\"\n" ++
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
   cplusplus_hdr = "#ifdef __cplusplus\nextern \"C\" {\n#endif\n"
   cplusplus_ftr = "#ifdef __cplusplus\n}\n#endif\n"


-- Don't use doOutput for dumping the f. export stubs
-- since it is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help :: FilePath -> String -> String -> String -> IO Bool
outputForeignStubs_help _fname ""      _header _footer = return False
outputForeignStubs_help fname doc_str header footer
   = do writeFile fname (header ++ doc_str ++ '\n':footer ++ "\n")
        return True

