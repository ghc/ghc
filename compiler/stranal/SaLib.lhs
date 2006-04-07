%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[SaLib]{Basic datatypes, functions for the strictness analyser}

See also: the ``library'' for the ``back end'' (@SaBackLib@).

\begin{code}
#ifndef OLD_STRICTNESS
module SaLib () where
#else

module SaLib (
	AbsVal(..),
	AnalysisKind(..),
	AbsValEnv{-abstract-}, StrictEnv, AbsenceEnv,
	mkAbsApproxFun,
	nullAbsValEnv, addOneToAbsValEnv, growAbsValEnvList,
	lookupAbsValEnv,
	absValFromStrictness
    ) where

#include "HsVersions.h"

import Type		( Type )
import VarEnv
import IdInfo		( StrictnessInfo(..) )
import Demand		( Demand )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[AbsVal-datatype]{@AbsVal@: abstract values (and @AbsValEnv@)}
%*									*
%************************************************************************

@AnalysisKind@ tells what kind of analysis is being done.

\begin{code}
data AnalysisKind
  = StrAnal 	-- We're doing strictness analysis
  | AbsAnal	-- We're doing absence analysis
  deriving Show
\end{code}

@AbsVal@ is the data type of HNF abstract values.

\begin{code}
data AbsVal
  = AbsTop		    -- AbsTop is the completely uninformative
			    -- value

  | AbsBot		    -- An expression whose abstract value is
			    -- AbsBot is sure to fail to terminate.
			    -- AbsBot represents the abstract
			    --  *function* bottom too.

  | AbsProd [AbsVal]	    -- (Lifted) product of abstract values
			    -- "Lifted" means that AbsBot is *different* from
			    --    AbsProd [AbsBot, ..., AbsBot]

  | AbsFun	    	    -- An abstract function, with the given:
	    Type	   	 -- Type of the *argument* to the function
	    (AbsVal -> AbsVal)	-- The function

  | AbsApproxFun	    -- This is used to represent a coarse
	    [Demand]	    -- approximation to a function value.  It's an
	    AbsVal	    -- abstract function which is strict in its
			    -- arguments if the  Demand so indicates.
	-- INVARIANT: the [Demand] is non-empty

	-- AbsApproxFun has to take a *list* of demands, no just one,
	-- because function spaces are now lifted.  Hence, (f bot top)
	-- might be bot, but the partial application (f bot) is a *function*,
	-- not bot.

mkAbsApproxFun :: Demand -> AbsVal -> AbsVal
mkAbsApproxFun d (AbsApproxFun ds val) = AbsApproxFun (d:ds) val
mkAbsApproxFun d val	   	       = AbsApproxFun [d]    val

instance Outputable AbsVal where
    ppr AbsTop = ptext SLIT("AbsTop")
    ppr AbsBot = ptext SLIT("AbsBot")
    ppr (AbsProd prod) = hsep [ptext SLIT("AbsProd"), ppr prod]
    ppr (AbsFun bndr_ty body) = ptext SLIT("AbsFun")
    ppr (AbsApproxFun demands val)
      = ptext SLIT("AbsApprox") <+> brackets (interpp'SP demands) <+> ppr val
\end{code}

%-----------

An @AbsValEnv@ maps @Ids@ to @AbsVals@.  Any unbound @Ids@ are
implicitly bound to @AbsTop@, the completely uninformative,
pessimistic value---see @absEval@ of a @Var@.

\begin{code}
newtype AbsValEnv = AbsValEnv (IdEnv AbsVal)

type StrictEnv  = AbsValEnv	-- Environment for strictness analysis
type AbsenceEnv = AbsValEnv	-- Environment for absence analysis

nullAbsValEnv -- this is the one and only way to create AbsValEnvs
  = AbsValEnv emptyVarEnv

addOneToAbsValEnv (AbsValEnv idenv) y z = AbsValEnv (extendVarEnv idenv y z)
growAbsValEnvList (AbsValEnv idenv) ys  = AbsValEnv (extendVarEnvList idenv ys)

lookupAbsValEnv (AbsValEnv idenv) y
  = lookupVarEnv idenv y
\end{code}

\begin{code}
absValFromStrictness :: AnalysisKind -> StrictnessInfo -> AbsVal

absValFromStrictness anal NoStrictnessInfo = AbsTop
absValFromStrictness anal (StrictnessInfo args_info bot_result)
  = case args_info of	-- Check the invariant that the arg list on 
	[] -> res	-- AbsApproxFun is non-empty
	_  -> AbsApproxFun args_info res
  where
    res | not bot_result = AbsTop
	| otherwise      = case anal of
				StrAnal -> AbsBot
				AbsAnal -> AbsTop
\end{code}

\begin{code}
#endif /* OLD_STRICTNESS */
\end{code}
