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
import AbsCSyn		( AbstractC )
import PprAbsC		( dumpRealC, writeRealC )
import UniqSupply	( UniqSupply )
import Module		( Module )
import CmdLineOpts
import ErrUtils		( dumpIfSet_dyn )
import Outputable
import CmdLineOpts	( DynFlags, HscLang(..), dopt_OutName )
import TmpFiles		( newTempName )

import IO		( IOMode(..), hClose, openFile, Handle )
\end{code}


%************************************************************************
%*									*
\subsection{Steering}
%*									*
%************************************************************************

\begin{code}
codeOutput :: DynFlags
	   -> Module
	   -> [TyCon] -> [Class]	-- Local tycons and classes
	   -> [CoreBind]		-- Core bindings
	   -> [(StgBinding,[Id])]	-- The STG program with SRTs
	   -> SDoc 		-- C stubs for foreign exported functions
	   -> SDoc		-- Header file prototype for foreign exported functions
	   -> AbstractC		-- Compiled abstract C
	   -> UniqSupply
	   -> IO (Maybe FilePath, Maybe FilePath)
codeOutput dflags mod_name tycons classes core_binds stg_binds 
	   c_code h_code flat_abstractC ncg_uniqs
  = -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    -- Dunno if the above comment is still meaningful now.  JRS 001024.

    do let filenm = dopt_OutName dflags 
       stub_names <- outputForeignStubs dflags c_code h_code
       case dopt_HscLang dflags of
          HscInterpreted -> return stub_names
          HscAsm         -> outputAsm dflags filenm flat_abstractC ncg_uniqs
                            >> return stub_names
          HscC           -> outputC dflags filenm flat_abstractC	
                            >> return stub_names
          HscJava        -> outputJava dflags filenm mod_name tycons core_binds
                            >> return stub_names

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
outputC dflags filenm flat_absC
  = do dumpIfSet_dyn dflags Opt_D_dump_realC "Real C" (dumpRealC flat_absC)
       doOutput filenm (\ h -> writeRealC h flat_absC)
\end{code}


%************************************************************************
%*									*
\subsection{Assembler}
%*									*
%************************************************************************

\begin{code}
outputAsm dflags filenm flat_absC ncg_uniqs

#ifndef OMIT_NATIVE_CODEGEN

  = do dumpIfSet_dyn dflags Opt_D_dump_stix "Final stix code" stix_final
       dumpIfSet_dyn dflags Opt_D_dump_asm "Asm code" ncg_output_d
       doOutput filenm ( \f -> printForAsm f ncg_output_d)
  where
    (stix_final, ncg_output_d) = nativeCodeGen flat_absC ncg_uniqs

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
outputJava dflags filenm mod tycons core_binds
  = doOutput filenm (\ f -> printForUser f pp_java)
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
outputForeignStubs dflags c_code h_code
  = do
	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export header file" stub_h_output_d

	maybe_stub_h_file
           <- outputForeignStubs_help True{-.h output-} stub_h_output_w

	dumpIfSet_dyn dflags Opt_D_dump_foreign
                      "Foreign export stubs" stub_c_output_d

        maybe_stub_c_file
           <- outputForeignStubs_help False{-not .h-} stub_c_output_w

        return (maybe_stub_h_file, maybe_stub_c_file)
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
outputForeignStubs_help is_header ""      = return Nothing
outputForeignStubs_help is_header doc_str 
   = newTempName suffix >>= \ fname ->
     writeFile fname (include_prefix ++ doc_str) >>
     return (Just suffix)
  where
    suffix
       | is_header   = "h_stub"
       | otherwise   = "c_stub"
    include_prefix
       | is_header   = "#include \"Rts.h\"\n"
       | otherwise   = "#include \"RtsAPI.h\"\n"
\end{code}

