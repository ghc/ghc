%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

\begin{code}
module IParam (
        ipFastString, ipTyConName, ipTyCon, ipCoAxiom
    ) where

#include "HsVersions.h"

import Name
import TyCon    (CoAxiom, TyCon, newTyConCo_maybe)
import Type

import BasicTypes (IPName(..), ipNameName)
import FastString
import Outputable
\end{code}

\begin{code}
ipFastString :: IPName Name -> FastString
ipFastString = occNameFS . nameOccName . ipTyConName

ipTyConName :: IPName Name -> Name
ipTyConName = ipNameName

ipTyCon :: IPName Name -> TyCon
ipTyCon ip = case wiredInNameTyThing_maybe (ipTyConName ip) of
    Just (ATyCon tc) -> tc
    _                -> pprPanic "ipTyCon" (ppr ip)

ipCoAxiom :: IPName Name -> CoAxiom
ipCoAxiom ip = case newTyConCo_maybe (ipTyCon ip) of
    Just ax -> ax
    _       -> pprPanic "ipCoAxiom" (ppr ip)

-- The IParam DataCon never gets any code generated for it, so it's
-- a bit dangerous to actually make use of it, hence no ipDataCon function
\end{code}
