%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[Argv]{@Argv@: direct (non-standard) access to command-line arguments}

\begin{code}
module Argv ( argv ) where

#include "HsVersions.h"

import FastString

import GlaExts		( Addr )
import ByteArray 	( indexAddrOffAddr )

argv :: [FAST_STRING]
argv = unpackArgv ``prog_argv'' (``prog_argc''::Int)

unpackArgv :: Addr -> Int -> [FAST_STRING] -- argv[1 .. argc-1]

unpackArgv argv argc = unpack 1
  where
    unpack :: Int -> [FAST_STRING]
    unpack n
      = if (n >= argc)
	then ([] :: [FAST_STRING])
	else case (indexAddrOffAddr argv n) of { item ->
	     mkFastCharString item : unpack (n + 1)
	     }
\end{code}
