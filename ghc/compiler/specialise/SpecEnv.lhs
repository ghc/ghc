%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[SpecEnv]{Specialisation info about an @Id@}

\begin{code}
#include "HsVersions.h"

module SpecEnv (
	SYN_IE(SpecEnv), MatchEnv,
	nullSpecEnv, isNullSpecEnv,
	addOneToSpecEnv, lookupSpecEnv
    ) where

IMP_Ubiq()

import MatchEnv
import Type		--( matchTys, isTyVarTy )
import Usage		( SYN_IE(UVar) )
import OccurAnal	( occurAnalyseGlobalExpr )
import CoreSyn		( SYN_IE(CoreExpr), SYN_IE(SimplifiableCoreExpr) )
import Maybes		( MaybeErr(..) )
import TyVar --ToDo:rm
\end{code}


A @SpecEnv@ holds details of an @Id@'s specialisations.  It should be
a newtype (ToDo), but for 1.2 compatibility we make it a data type.
It can't be a synonym because there's an IdInfo instance of it
that doesn't work if it's (MatchEnv a b).
Furthermore, making it a data type makes it easier to break the IdInfo loop.

\begin{code}
data SpecEnv = SpecEnv (MatchEnv [Type] SimplifiableCoreExpr)
\end{code}

For example, if \tr{f}'s @SpecEnv@ contains the mapping:
\begin{verbatim}
	[List a, b]  ===>  (\d -> f' a b)
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
	f (List Int) Bool ===>  (\d -> f' Int Bool)
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
SpecEnv contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

	pi :: forall a. Num a => a

might have a specialisation

	[Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.


\begin{code}
nullSpecEnv :: SpecEnv
nullSpecEnv = SpecEnv nullMEnv

isNullSpecEnv :: SpecEnv -> Bool
isNullSpecEnv (SpecEnv env) = null (mEnvToList env)

addOneToSpecEnv :: SpecEnv -> [Type] -> CoreExpr -> MaybeErr SpecEnv ([Type], SimplifiableCoreExpr)
addOneToSpecEnv (SpecEnv env) tys rhs 
  = --pprTrace "addOneToSpecEnv" (($$) (ppr PprDebug tys) (ppr PprDebug rhs)) $
    case (insertMEnv matchTys env tys (occurAnalyseGlobalExpr rhs)) of
	Succeeded menv -> Succeeded (SpecEnv menv)
	Failed err     -> Failed err

lookupSpecEnv :: SpecEnv -> [Type] -> Maybe (SimplifiableCoreExpr, ([(TyVar,Type)], [Type]))
lookupSpecEnv (SpecEnv env) tys 
  | all isTyVarTy tys = Nothing	-- Short cut: no specialisation for simple tyvars
  | otherwise	      = --pprTrace "lookupSpecEnv" (ppr PprDebug tys) $
			lookupMEnv matchTys env tys
\end{code}


