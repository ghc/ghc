%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Code output phase}

\begin{code}
module CodeOutput( codeOutput ) where

#include "HsVersions.h"

#if ! OMIT_NATIVE_CODEGEN
import AsmCodeGen	( nativeCodeGen )
#endif

import AbsCSyn		( AbstractC, absCNop )
import PprAbsC		( dumpRealC, writeRealC )
import UniqSupply	( UniqSupply )
import Module		( Module, moduleString )
import CmdLineOpts
import Maybes		( maybeToBool )
import ErrUtils		( doIfSet, dumpIfSet )
import Outputable
import IO		( IOMode(..), hPutStr, hClose, openFile, stderr	)
\end{code}


\begin{code}
codeOutput :: Module
	   -> SDoc 		-- C stubs for foreign exported functions
	   -> SDoc		-- Header file prototype for foreign exported functions
	   -> AbstractC		-- Compiled abstract C
	   -> UniqSupply
	   -> IO ()
codeOutput mod_name c_code h_code flat_abstractC ncg_uniqs
  = -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]

    dumpIfSet opt_D_dump_stix "Final stix code" stix_final >>

    dumpIfSet opt_D_dump_asm "Asm code" ncg_output_d 	>>
    doOutput opt_ProduceS ncg_output_w 			>>

    dumpIfSet opt_D_dump_foreign "Foreign export header file" stub_h_output_d >>
    outputForeignStubs True{-.h output-} opt_ProduceExportHStubs stub_h_output_w	>>

    dumpIfSet opt_D_dump_foreign "Foreign export stubs" stub_c_output_d >>
    outputForeignStubs False{-not .h-}   opt_ProduceExportCStubs stub_c_output_w	>>

    dumpIfSet opt_D_dump_realC "Real C" c_output_d 	>>
    doOutput opt_ProduceC c_output_w

  where
    (flat_absC_c, flat_absC_ncg) =
	 case (maybeToBool opt_ProduceC || opt_D_dump_realC,
	       maybeToBool opt_ProduceS || opt_D_dump_asm) of
	     (True,  False) -> (flat_abstractC, absCNop)
	     (False, True)  -> (absCNop, flat_abstractC)
	     (False, False) -> (absCNop, absCNop)
	     (True,  True)  -> error "ERROR: Can't do both .hc and .s at the same time"

    -- C stubs for "foreign export"ed functions.
    stub_c_output_d = pprCode CStyle c_code
    stub_c_output_w = showSDoc stub_c_output_d

    -- Header file protos for "foreign export"ed functions.
    stub_h_output_d = pprCode CStyle h_code
    stub_h_output_w = showSDoc stub_h_output_d

    c_output_d = dumpRealC flat_absC_c
    c_output_w = (\ f -> writeRealC f flat_absC_c)

	-- Native code generation done here!
#if OMIT_NATIVE_CODEGEN
    ncg_output_d = error "*** GHC not built with a native-code generator ***"
    ncg_output_w = ncg_output_d
#else
    (stix_final, ncg_output_d)
       = nativeCodeGen flat_absC_ncg ncg_uniqs
    ncg_output_w = (\ f -> printForAsm f ncg_output_d)
#endif


    -- don't use doOutput for dumping the f. export stubs
    -- since it is more than likely that the stubs file will
    -- turn out to be empty, in which case no file should be created.
outputForeignStubs is_header switch ""      = return ()
outputForeignStubs is_header switch doc_str =
  case switch of
    Nothing    -> return ()
    Just fname -> writeFile fname (include_prefix ++ doc_str)
 where
  include_prefix
   | is_header   = "#include \"Rts.h\"\n"
   | otherwise   = "#include \"RtsAPI.h\"\n"

doOutput switch io_action
  = case switch of
	  Nothing    -> return ()
	  Just fname ->
	    openFile fname WriteMode	>>= \ handle ->
	    io_action handle		>>
	    hClose handle
\end{code}

