%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[Argv]{@Argv@: direct (non-standard) access to command-line arguments}

\begin{code}
#include "HsVersions.h"

module Argv ( argv ) where

import PreludeGlaST	( indexAddrOffAddr )

CHK_Ubiq() -- debugging consistency check

argv :: [FAST_STRING]
argv = unpackArgv ``prog_argv'' (``prog_argc''::Int)

unpackArgv :: _Addr -> Int -> [FAST_STRING] -- argv[1 .. argc-1]

unpackArgv argv argc = unpack 1
  where
    unpack :: Int -> [FAST_STRING]
    unpack n
      = if (n >= argc)
	then ([] :: [FAST_STRING])
	else case (indexAddrOffAddr argv n) of { item ->
	     _packCString item : unpack (n + 1)
	     }
\end{code}
