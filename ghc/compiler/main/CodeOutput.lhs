%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Code output phase}

\begin{code}
module CodeOutput( codeOutput ) where

#include "HsVersions.h"

#ifndef OMIT_NATIVE_CODEGEN
import AsmCodeGen	( nativeCodeGen )
#endif

#ifdef ILX
import IlxGen		( ilxGen )
#endif

import JavaGen		( javaGen )
import qualified PrintJava

import TyCon		( TyCon )
import Id		( Id )
import Class		( Class )
import CoreSyn		( CoreBind )
import StgSyn		( StgBinding )
import AbsCSyn		( AbstractC, absCNop )
import PprAbsC		( dumpRealC, writeRealC )
import UniqSupply	( UniqSupply )
import Module		( Module, moduleString )
import CmdLineOpts
import Maybes		( maybeToBool )
import ErrUtils		( doIfSet, dumpIfSet )
import Outputable
import IO		( IOMode(..), hPutStr, hClose, openFile	)
\end{code}


%************************************************************************
%*									*
\subsection{Steering}
%*									*
%************************************************************************

\begin{code}
codeOutput :: Module
	   -> [TyCon] -> [Class]	-- Local tycons and classes
	   -> [CoreBind]		-- Core bindings
	   -> [(StgBinding,[Id])]	-- The STG program with SRTs
	   -> SDoc 		-- C stubs for foreign exported functions
	   -> SDoc		-- Header file prototype for foreign exported functions
	   -> AbstractC		-- Compiled abstract C
	   -> UniqSupply
	   -> IO ()
codeOutput mod_name tycons classes core_binds stg_binds 
	   c_code h_code flat_abstractC ncg_uniqs
  = -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    do  {
	outputForeignStubs c_code h_code ;
	case opt_OutputLanguage of {
	  Nothing     -> return ()	-- No -olang=xxx flag; so no-op
	; Just "asm"  -> outputAsm flat_abstractC ncg_uniqs	
	; Just "C"    -> outputC flat_abstractC	
	; Just "java" -> outputJava mod_name tycons core_binds
	; Just foo    -> pprPanic "Don't understand output language" (quotes (text foo))
	} }


doOutput io_action
  = (do	handle <- openFile opt_OutputFile WriteMode
	io_action handle
	hClose handle)
    `catch` (\err -> pprPanic "Failed to open or write code output file" (text opt_OutputFile))
\end{code}


%************************************************************************
%*									*
\subsection{C}
%*									*
%************************************************************************

\begin{code}
outputC flat_absC
  = do 
       dumpIfSet opt_D_dump_realC "Real C" (dumpRealC flat_absC)
       doOutput (\ h -> writeRealC h flat_absC)
\end{code}


%************************************************************************
%*									*
\subsection{Assembler}
%*									*
%************************************************************************

\begin{code}
outputAsm flat_absC ncg_uniqs
#ifndef OMIT_NATIVE_CODEGEN

  = do	dumpIfSet opt_D_dump_stix "Final stix code" stix_final
	dumpIfSet opt_D_dump_asm "Asm code" ncg_output_d
	doOutput (\ f -> printForAsm f ncg_output_d)
  where
    (stix_final, ncg_output_d) = nativeCodeGen flat_absC ncg_uniqs

#else /* OMIT_NATIVE_CODEGEN */

  = do 	hPutStrLn stderr "This compiler was built without a native code generator"
	hPutStrLn stderr "Use -fvia-C instead"

#endif
\end{code}


%************************************************************************
%*									*
\subsection{Java}
%*									*
%************************************************************************

\begin{code}
outputJava mod tycons core_binds
  = doOutput (\ f -> printForUser f pp_java)
	-- User style printing for now to keep indentation
  where
    java_code = javaGen mod [{- Should be imports-}] tycons core_binds
    pp_java   = PrintJava.compilationUnit java_code
\end{code}


%************************************************************************
%*									*
\subsection{Foreign import/export}
%*									*
%************************************************************************

\begin{code}
outputForeignStubs c_code h_code
  = do
	dumpIfSet opt_D_dump_foreign "Foreign export header file" stub_h_output_d
	outputForeignStubs_help True{-.h output-} opt_ProduceExportHStubs stub_h_output_w

	dumpIfSet opt_D_dump_foreign "Foreign export stubs" stub_c_output_d
	outputForeignStubs_help False{-not .h-}   opt_ProduceExportCStubs stub_c_output_w
  where
    -- C stubs for "foreign export"ed functions.
    stub_c_output_d = pprCode CStyle c_code
    stub_c_output_w = showSDoc stub_c_output_d

    -- Header file protos for "foreign export"ed functions.
    stub_h_output_d = pprCode CStyle h_code
    stub_h_output_w = showSDoc stub_h_output_d


-- Don't use doOutput for dumping the f. export stubs
-- since it is more than likely that the stubs file will
-- turn out to be empty, in which case no file should be created.
outputForeignStubs_help is_header switch ""      = return ()
outputForeignStubs_help is_header switch doc_str =
  case switch of
    Nothing    -> return ()
    Just fname -> writeFile fname (include_prefix ++ doc_str)
 where
  include_prefix
   | is_header   = "#include \"Rts.h\"\n"
   | otherwise   = "#include \"RtsAPI.h\"\n"
\end{code}

