%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Code output phase}

\begin{code}
module CodeOutput( codeOutput ) where

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
#endif

import DriverState	( v_HCHeader )
import TyCon		( TyCon )
import Id		( Id )
import CoreSyn		( CoreBind )
import OccurAnal	( occurAnalyseBinds )
import StgSyn		( StgBinding )
import AbsCSyn		( AbstractC )
import PprAbsC		( dumpRealC, writeRealC )
import Module		( Module )
import CmdLineOpts
import ErrUtils		( dumpIfSet_dyn, showPass )
import Outputable
import Pretty		( Mode(..), printDoc )
import CmdLineOpts	( DynFlags, HscLang(..), dopt_OutName )

import IOExts
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
	   -> [TyCon]			-- Local tycons
	   -> [CoreBind]		-- Core bindings
	   -> [(StgBinding,[Id])]	-- The STG program with SRTs
	   -> SDoc 		-- C stubs for foreign exported functions
	   -> SDoc		-- Header file prototype for foreign exported functions
	   -> AbstractC		-- Compiled abstract C
	   -> IO (Bool{-stub_h_exists-}, Bool{-stub_c_exists-})
codeOutput dflags mod_name tycons core_binds stg_binds 
	   c_code h_code flat_abstractC
  = -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    -- Dunno if the above comment is still meaningful now.  JRS 001024.

    do	{ showPass dflags "CodeOutput"
	; let filenm = dopt_OutName dflags 
	; stub_names <- outputForeignStubs dflags c_code h_code
	; case dopt_HscLang dflags of
             HscInterpreted -> return stub_names
             HscAsm         -> outputAsm dflags filenm flat_abstractC
          		       >> return stub_names
             HscC           -> outputC dflags filenm flat_abstractC stub_names
          		       >> return stub_names
             HscJava        -> 
#ifdef JAVA
			       outputJava dflags filenm mod_name tycons core_binds
          		       >> return stub_names
#else
                               panic "Java support not compiled into this ghc"
#endif
	     HscILX         -> 
#ifdef ILX
	                       outputIlx dflags filenm mod_name tycons stg_binds
			       >> return stub_names
#else
                               panic "ILX support not compiled into this ghc"
#endif
	}

doOutput :: String -> (Handle -> IO ()) -> IO ()
doOutput filenm io_action
  = (do	handle <- openFile filenm WriteMode
	io_action handle
	hClose handle)
    `catch` (\err -> pprPanic "Failed to open or write code output file" 
			      (text filenm))
\end{code}


%************************************************************************
%*									*
\subsection{C}
%*									*
%************************************************************************

\begin{code}
outputC dflags filenm flat_absC (stub_h_exists, _)
  = do dumpIfSet_dyn dflags Opt_D_dump_realC "Real C" (dumpRealC flat_absC)
       header <- readIORef v_HCHeader
       doOutput filenm $ \ h -> do
	  hPutStr h header
	  when stub_h_exists $ 
	     hPutStrLn h ("#include \"" ++ (hscStubHOutName dflags) ++ "\"")
	  writeRealC h flat_absC
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
       let (stix_final, ncg_output_d) = _scc_ "NativeCodeGen" 
				        nativeCodeGen flat_absC ncg_uniqs
       dumpIfSet_dyn dflags Opt_D_dump_stix "Final stix code" stix_final
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
outputForeignStubs dflags c_code h_code
  = do
	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file" stub_h_output_d

	stub_h_file_exists
           <- outputForeignStubs_help (hscStubHOutName dflags) stub_h_output_w
		("#include \"HsFFI.h\"\n" ++ cplusplus_hdr) cplusplus_ftr

	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export stubs" stub_c_output_d

        hc_header <- readIORef v_HCHeader

	stub_c_file_exists
           <- outputForeignStubs_help (hscStubCOutName dflags) stub_c_output_w
		("#define IN_STG_CODE 0\n" ++ 
		 hc_header ++
		 "#include \"RtsAPI.h\"\n" ++
		 cplusplus_hdr)
		 cplusplus_ftr
	   -- we're adding the default hc_header to the stub file, but this
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

