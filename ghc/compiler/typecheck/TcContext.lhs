%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcContext]{Typecheck a type-class context}

\begin{code}
module TcContext ( tcContext ) where

#include "HsVersions.h"

import TcMonad		-- typechecking monadic machinery
import AbsSyn		-- the stuff being typechecked

import CE		( lookupCE, CE(..) )
import Errors		( naughtyCCallContextErr )
import TCE		( TCE(..), UniqFM )
import TVE		( TVE(..) )
import TcMonoType	( tcMonoType )
import Unique		( cCallableClassKey, cReturnableClassKey )
import Util

tcContext :: CE -> TCE -> TVE -> RenamedContext -> Baby_TcM ThetaType

tcContext ce tce tve context
  = mapB_Tc (tcClassAssertion ce tce tve) context

tcClassAssertion ce tce tve (class_name, tyname)
  | canBeUsedInContext class_name
  = tcMonoType ce tce tve (MonoTyVar tyname) `thenB_Tc` \ ty ->
    returnB_Tc (lookupCE ce class_name, ty)

  | otherwise
  = getSrcLocB_Tc `thenB_Tc` \ locn ->
    failB_Tc (naughtyCCallContextErr class_name locn)
\end{code}

HACK warning: Someone discovered that @_CCallable_@ and @_CReturnable@
could be used in contexts such as:
\begin{verbatim}
foo :: _CCallable a => a -> PrimIO Int
\end{verbatim}

Doing this utterly wrecks the whole point of introducing these
classes so we specifically check that this isn't being done.

\begin{code}
canBeUsedInContext :: Name -> Bool

canBeUsedInContext class_name
  = class_name /= cCallableClass && class_name /= cReturnableClass
 where
  cCallableClass   = PreludeClass cCallableClassKey   bottom
  cReturnableClass = PreludeClass cReturnableClassKey bottom
  bottom	   = panic "canBeUsedInContext"
\end{code}
