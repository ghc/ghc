%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Code output phase}

\begin{code}
module CodeOutput( codeOutput, outputForeignStubs ) where

#include "HsVersions.h"

#ifndef OMIT_NATIVE_CODEGEN
import AsmCodeGen	( nativeCodeGen )
#endif
import LlvmCodeGen ( llvmCodeGen )

import UniqSupply	( mkSplitUniqSupply )

#ifdef JAVA
import JavaGen		( javaGen )
import qualified PrintJava
import OccurAnal	( occurAnalyseBinds )
#endif

import Finder		( mkStubPaths )
import PprC		( writeCs )
import CmmLint		( cmmLint )
import Packages
import Util
import Cmm		( RawCmm )
import HscTypes
import DynFlags
import Config

import ErrUtils		( dumpIfSet_dyn, showPass, ghcExit )
import Outputable
import Module
import Maybes		( firstJusts )

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.IO
\end{code}

%************************************************************************
%*									*
\subsection{Steering}
%*									*
%************************************************************************

\begin{code}
codeOutput :: DynFlags
	   -> Module
	   -> ModLocation
	   -> ForeignStubs
	   -> [PackageId]
	   -> [RawCmm]			-- Compiled C--
	   -> IO (Bool{-stub_h_exists-}, Bool{-stub_c_exists-})

codeOutput dflags this_mod location foreign_stubs pkg_deps flat_abstractC
  = 
    -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    -- Dunno if the above comment is still meaningful now.  JRS 001024.

    do	{ when (dopt Opt_DoCmmLinting dflags) $ do
		{ showPass dflags "CmmLint"
		; let lints = map cmmLint flat_abstractC
		; case firstJusts lints of
			Just err -> do { printDump err
				       ; ghcExit dflags 1
				       }
			Nothing  -> return ()
		}

	; showPass dflags "CodeOutput"
	; let filenm = hscOutName dflags 
	; stubs_exist <- outputForeignStubs dflags this_mod location foreign_stubs
	; case hscTarget dflags of {
             HscInterpreted -> return ();
             HscAsm         -> outputAsm dflags filenm flat_abstractC;
             HscC           -> outputC dflags filenm flat_abstractC pkg_deps;
             HscLlvm        -> outputLlvm dflags filenm flat_abstractC;
             HscJava        -> 
#ifdef JAVA
			       outputJava dflags filenm mod_name tycons core_binds;
#else
                               panic "Java support not compiled into this ghc";
#endif
             HscNothing     -> panic "codeOutput: HscNothing"
	  }
	; return stubs_exist
	}

doOutput :: String -> (Handle -> IO ()) -> IO ()
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action
\end{code}


%************************************************************************
%*									*
\subsection{C}
%*									*
%************************************************************************

\begin{code}
outputC :: DynFlags
        -> FilePath
        -> [RawCmm]
        -> [PackageId]
        -> IO ()

outputC dflags filenm flat_absC packages
  = do 
       -- figure out which header files to #include in the generated .hc file:
       --
       --   * extra_includes from packages
       --   * -#include options from the cmdline and OPTIONS pragmas
       --   * the _stub.h file, if there is one.
       --
       let rts = getPackageDetails (pkgState dflags) rtsPackageId
                       
       let cc_injects = unlines (map mk_include (includes rts))
       	   mk_include h_file = 
       	    case h_file of 
       	       '"':_{-"-} -> "#include "++h_file
       	       '<':_      -> "#include "++h_file
       	       _          -> "#include \""++h_file++"\""

       pkg_configs <- getPreloadPackagesAnd dflags packages
       let pkg_names = map (display.sourcePackageId) pkg_configs

       doOutput filenm $ \ h -> do
	  hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
	  hPutStr h cc_injects
	  writeCs dflags h flat_absC
\end{code}


%************************************************************************
%*									*
\subsection{Assembler}
%*									*
%************************************************************************

\begin{code}
outputAsm :: DynFlags -> FilePath -> [RawCmm] -> IO ()

#ifndef OMIT_NATIVE_CODEGEN

outputAsm dflags filenm flat_absC
  = do ncg_uniqs <- mkSplitUniqSupply 'n'

       {-# SCC "OutputAsm" #-} doOutput filenm $
	   \f -> {-# SCC "NativeCodeGen" #-}
	         nativeCodeGen dflags f ncg_uniqs flat_absC
  where

#else /* OMIT_NATIVE_CODEGEN */

outputAsm _ _ _
  = pprPanic "This compiler was built without a native code generator"
	     (text "Use -fvia-C instead")

#endif
\end{code}


%************************************************************************
%*									*
\subsection{LLVM}
%*									*
%************************************************************************

\begin{code}
outputLlvm :: DynFlags -> FilePath -> [RawCmm] -> IO ()
outputLlvm dflags filenm flat_absC
  = do ncg_uniqs <- mkSplitUniqSupply 'n'
       doOutput filenm $ \f -> llvmCodeGen dflags f ncg_uniqs flat_absC
\end{code}


%************************************************************************
%*									*
\subsection{Java}
%*									*
%************************************************************************

\begin{code}
#ifdef JAVA
outputJava dflags filenm mod tycons core_binds
  = doOutput filenm (\ f -> printForUser f alwaysQualify pp_java)
	-- User style printing for now to keep indentation
  where
    occ_anal_binds = occurAnalyseBinds core_binds
	-- Make sure we have up to date dead-var information
    java_code = javaGen mod [{- Should be imports-}] tycons occ_anal_binds
    pp_java   = PrintJava.compilationUnit java_code
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Foreign import/export}
%*									*
%************************************************************************

\begin{code}
outputForeignStubs :: DynFlags -> Module -> ModLocation -> ForeignStubs
		   -> IO (Bool, 	-- Header file created
			  Bool)		-- C file created
outputForeignStubs dflags mod location stubs
 = case stubs of
   NoStubs -> do
	-- When compiling External Core files, may need to use stub
	-- files from a previous compilation
	stub_c_exists <- doesFileExist stub_c
	stub_h_exists <- doesFileExist stub_h
	return (stub_h_exists, stub_c_exists)

   ForeignStubs h_code c_code -> do
	let
	    stub_c_output_d = pprCode CStyle c_code
	    stub_c_output_w = showSDoc stub_c_output_d
	
	    -- Header file protos for "foreign export"ed functions.
	    stub_h_output_d = pprCode CStyle h_code
	    stub_h_output_w = showSDoc stub_h_output_d
	-- in

	createDirectoryHierarchy (takeDirectory stub_c)

	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file" stub_h_output_d

	-- we need the #includes from the rts package for the stub files
	let rts_includes = 
	       let rts_pkg = getPackageDetails (pkgState dflags) rtsPackageId in
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

        return (stub_h_file_exists, stub_c_file_exists)
  where
   (stub_c, stub_h, _) = mkStubPaths dflags (moduleName mod) location

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
\end{code}

