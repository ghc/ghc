%
% (c) The AQUA Project, Glasgow University, 1993-2000
%
\section[CmSummarise]{Module summariser for GHCI}

\begin{code}
module CmSummarise ( ModSummary(..), summarise )
where

#include "HsVersions.h"

import CmFind	 	( ModName, ModLocation )

\end{code}

\begin{code}


data ModSummary
   = ModSummary ModLocation                    -- location and kind
                (Maybe (String, Fingerprint))  -- source and sig if .hs
                [ModName]                      -- imports

type Fingerprint = Int

summarise :: ModLocation -> IO ModSummary
summarise loc = return (error "summarise:unimp")
\end{code}
