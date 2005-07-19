%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Code output phase}

\begin{code}
module CodeOutput( codeOutput, outputForeignStubs ) where

#include "HsVersions.h"

#ifndef OMIT_NATIVE_CODEGEN
import UniqSupply	( mkSplitUniqSupply )
import AsmCodeGen	( nativeCodeGen )
#endif

#ifdef ILX
import IlxGen		( ilxGen )
#endif

#ifdef JAVA
import JavaGen		( javaGen )
import qualified PrintJava
import OccurAnal	( occurAnalyseBinds )
#endif

import Distribution.Package	( showPackageId )
import PprC		( writeCs )
import CmmLint		( cmmLint )
import Packages
import Util		( filenameOf )
import FastString	( unpackFS )
import Cmm		( Cmm )
import HscTypes
import DynFlags
import ErrUtils		( dumpIfSet_dyn, showPass, ghcExit )
import Outputable
import Pretty		( Mode(..), printDoc )
import Module		( Module )
import List		( nub )
import Maybes		( firstJust )

import Directory	( doesFileExist )
import Monad		( when )
import IO
\end{code}

%************************************************************************
%*									*
\subsection{Steering}
%*									*
%************************************************************************

\begin{code}
codeOutput :: DynFlags
	   -> Module
	   -> ForeignStubs
	   -> [PackageId]
	   -> [Cmm]			-- Compiled C--
	   -> IO (Bool{-stub_h_exists-}, Bool{-stub_c_exists-})

codeOutput dflags this_mod foreign_stubs pkg_deps flat_abstractC
  = 
    -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    -- Dunno if the above comment is still meaningful now.  JRS 001024.

    do	{ when (dopt Opt_DoCmmLinting dflags) $ do
		{ showPass dflags "CmmLint"
		; let lints = map cmmLint flat_abstractC
		; case firstJust lints of
			Just err -> do { printDump err
				       ; ghcExit 1
				       }
			Nothing  -> return ()
		}

	; showPass dflags "CodeOutput"
	; let filenm = hscOutName dflags 
	; stubs_exist <- outputForeignStubs dflags foreign_stubs
	; case hscTarget dflags of {
             HscInterpreted -> return ();
             HscAsm         -> outputAsm dflags filenm flat_abstractC;
             HscC           -> outputC dflags filenm flat_abstractC stubs_exist
					pkg_deps foreign_stubs;
             HscJava        -> 
#ifdef JAVA
			       outputJava dflags filenm mod_name tycons core_binds;
#else
                               panic "Java support not compiled into this ghc";
#endif
	     HscILX         -> 
#ifdef ILX
			       let tycons = typeEnvTyCons type_env in
	                       outputIlx dflags filenm mod_name tycons stg_binds;
#else
                               panic "ILX support not compiled into this ghc";
#endif
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
outputC dflags filenm flat_absC 
	(stub_h_exists, _) packages foreign_stubs
  = do 
       -- figure out which header files to #include in the generated .hc file:
       --
       --   * extra_includes from packages
       --   * -#include options from the cmdline and OPTIONS pragmas
       --   * the _stub.h file, if there is one.
       --
       pkg_configs <- getExplicitPackagesAnd dflags packages
       let pkg_names = map (showPackageId.package) pkg_configs

       c_includes <- getPackageCIncludes pkg_configs
       let cmdline_includes = cmdlineHcIncludes dflags -- -#include options
       
	   ffi_decl_headers 
	      = case foreign_stubs of
		  NoStubs 		  -> []
		  ForeignStubs _ _ fdhs _ -> map unpackFS (nub fdhs)
			-- Remove duplicates, because distinct foreign import decls
			-- may cite the same #include.  Order doesn't matter.

           all_headers =  c_includes
		       ++ reverse cmdline_includes
		       ++ ffi_decl_headers

       let cc_injects = unlines (map mk_include all_headers)
       	   mk_include h_file = 
       	    case h_file of 
       	       '"':_{-"-} -> "#include "++h_file
       	       '<':_      -> "#include "++h_file
       	       _          -> "#include \""++h_file++"\""

       doOutput filenm $ \ h -> do
	  hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
	  hPutStr h cc_injects
	  when stub_h_exists $ 
	     hPutStrLn h ("#include \"" ++ (filenameOf (hscStubHOutName dflags)) ++ "\"")
	  writeCs dflags h flat_absC
\end{code}


%************************************************************************
%*									*
\subsection{Assembler}
%*									*
%************************************************************************

\begin{code}
outputAsm dflags filenm flat_absC

#ifndef OMIT_NATIVE_CODEGEN

  = do ncg_uniqs <- mkSplitUniqSupply 'n'
       ncg_output_d <- _scc_ "NativeCodeGen" 
			  nativeCodeGen dflags flat_absC ncg_uniqs
       dumpIfSet_dyn dflags Opt_D_dump_asm "Asm code" (docToSDoc ncg_output_d)
       _scc_ "OutputAsm" doOutput filenm $
	   \f -> printDoc LeftMode f ncg_output_d
  where

#else /* OMIT_NATIVE_CODEGEN */

  = pprPanic "This compiler was built without a native code generator"
	     (text "Use -fvia-C instead")

#endif
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
\subsection{Ilx}
%*									*
%************************************************************************

\begin{code}
#ifdef ILX
outputIlx dflags filename mod tycons stg_binds
  =  doOutput filename (\ f -> printForC f pp_ilx)
  where
    pp_ilx = ilxGen mod tycons stg_binds
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Foreign import/export}
%*									*
%************************************************************************

\begin{code}
outputForeignStubs :: DynFlags -> ForeignStubs
		   -> IO (Bool, 	-- Header file created
			  Bool)		-- C file created
outputForeignStubs dflags NoStubs = do
-- When compiling External Core files, may need to use stub files from a 
-- previous compilation
   hFileExists <- doesFileExist (hscStubHOutName dflags)
   cFileExists <- doesFileExist (hscStubCOutName dflags)
   return (hFileExists, cFileExists)
outputForeignStubs dflags (ForeignStubs h_code c_code _ _)
  = do
	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file" stub_h_output_d

	-- we need the #includes from the rts package for the stub files
	let rtsid = rtsPackageId (pkgState dflags)
 	    rts_includes 
		| ExtPackage pid <- rtsid = 
			let rts_pkg = getPackageDetails (pkgState dflags) pid in
			concatMap mk_include (includes rts_pkg)
		| otherwise = []
	    mk_include i = "#include \"" ++ i ++ "\"\n"

	stub_h_file_exists
           <- outputForeignStubs_help (hscStubHOutName dflags) stub_h_output_w
		("#include \"HsFFI.h\"\n" ++ cplusplus_hdr) cplusplus_ftr

	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export stubs" stub_c_output_d

	stub_c_file_exists
           <- outputForeignStubs_help (hscStubCOutName dflags) stub_c_output_w
		("#define IN_STG_CODE 0\n" ++ 
		 "#include \"Rts.h\"\n" ++
		 rts_includes ++
		 cplusplus_hdr)
		 cplusplus_ftr
	   -- We're adding the default hc_header to the stub file, but this
	   -- isn't really HC code, so we need to define IN_STG_CODE==0 to
	   -- avoid the register variables etc. being enabled.

        return (stub_h_file_exists, stub_c_file_exists)
  where
    -- C stubs for "foreign export"ed functions.
    stub_c_output_d = pprCode CStyle c_code
    stub_c_output_w = showSDoc stub_c_output_d

    -- Header file protos for "foreign export"ed functions.
    stub_h_output_d = pprCode CStyle h_code
    stub_h_output_w = showSDoc stub_h_output_d

cplusplus_hdr = "#ifdef __cplusplus\nextern \"C\" {\n#endif\n"
cplusplus_ftr = "#ifdef __cplusplus\n}\n#endif\n"

-- Don't use doOutput for dumping the f. export stubs
-- since it is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help fname ""      header footer = return False
outputForeignStubs_help fname doc_str header footer
   = do writeFile fname (header ++ doc_str ++ '\n':footer ++ "\n")
        return True
\end{code}

