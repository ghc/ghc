%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
%************************************************************************
%*									*
\section[HsPragmas]{Pragmas in Haskell interface files}
%*									*
%************************************************************************

See also: @Sig@ (``signatures'') which is where user-supplied pragmas
for values show up; ditto @SpecInstSig@ (for instances) and
@SpecDataSig@ (for data types).

\begin{code}
module HsPragmas where

#include "HsVersions.h"

import IdInfo
import Outputable
\end{code}

All the pragma stuff has changed.  Here are some placeholders!

\begin{code}
data GenPragmas name  = NoGenPragmas
data DataPragmas name = NoDataPragmas
data InstancePragmas name = NoInstancePragmas
data ClassOpPragmas name  = NoClassOpPragmas
data ClassPragmas name  = NoClassPragmas

noClassPragmas = NoClassPragmas
isNoClassPragmas NoClassPragmas = True

noDataPragmas = NoDataPragmas
isNoDataPragmas NoDataPragmas = True

noGenPragmas = NoGenPragmas
isNoGenPragmas NoGenPragmas = True

noInstancePragmas = NoInstancePragmas
isNoInstancePragmas NoInstancePragmas = True

noClassOpPragmas = NoClassOpPragmas
isNoClassOpPragmas NoClassOpPragmas = True

instance Outputable name => Outputable (ClassPragmas name) where
    ppr NoClassPragmas = empty

instance Outputable name => Outputable (ClassOpPragmas name) where
    ppr NoClassOpPragmas = empty

instance Outputable name => Outputable (InstancePragmas name) where
    ppr NoInstancePragmas = empty

instance Outputable name => Outputable (GenPragmas name) where
    ppr NoGenPragmas = empty
\end{code}
