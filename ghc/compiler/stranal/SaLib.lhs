%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[SaLib]{Basic datatypes, functions for the strictness analyser}

See also: the ``library'' for the ``back end'' (@SaBackLib@).

\begin{code}
#include "HsVersions.h"

module SaLib (
	AbsVal(..),
	AnalysisKind(..),
	AbsValEnv{-abstract-}, StrictEnv(..), AbsenceEnv(..),
	StrAnalFlags(..), getStrAnalFlags,
	nullAbsValEnv, addOneToAbsValEnv, growAbsValEnvList,
	lookupAbsValEnv,
	absValFromStrictness,

	-- and to make the interface self-sufficient...
	CoreExpr, Id, IdEnv(..), UniqFM, Unique,
	Demand, PlainCoreExpr(..)
    ) where

import IdEnv
import IdInfo
--import FiniteMap	-- debugging only
import Outputable
import PlainCore
import Pretty
import Util		-- for pragmas only
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
  deriving Text
\end{code}

@AbsVal@ is the data type of HNF abstract values.

\begin{code}
data AbsVal
  = AbsTop		    -- AbsTop is the completely uninformative
			    -- value

  | AbsBot		    -- An expression whose abstract value is
			    -- AbsBot is sure to fail to terminate.
			    -- AbsBot represents the abstract
			    -- *function* bottom too.

  | AbsProd [AbsVal]	    -- (Lifted) product of abstract values
			    -- "Lifted" means that AbsBot is *different* from
			    --    AbsProd [AbsBot, ..., AbsBot]

  | AbsFun	    	    -- An abstract function, with the given:
	    [Id]	    -- arguments
	    PlainCoreExpr   -- body
	    AbsValEnv	    -- and environment

  | AbsApproxFun	    -- This is used to represent a coarse
	    [Demand]	    -- approximation to a function value.  It's an
	    		    -- abstract function which is strict in its i'th
			    -- argument if the i'th element of the Demand
			    -- list so indicates.
			    -- The list of arguments is always non-empty.
			    -- In effect, AbsApproxFun [] = AbsTop 

instance Outputable AbsVal where
    ppr sty AbsTop = ppStr "AbsTop"
    ppr sty AbsBot = ppStr "AbsBot"
    ppr sty (AbsProd prod) = ppCat [ppStr "AbsProd", ppr sty prod]
    ppr sty (AbsFun args body env)
      = ppCat [ppStr "AbsFun{", ppr sty args,
	       ppStr "???", -- ppStr "}{env:", ppr sty (keysFM env `zip` eltsFM env),
	       ppStr "}" ]
    ppr sty (AbsApproxFun demands)
      = ppCat [ppStr "AbsApprox{", ppr sty demands, ppStr "}" ]
\end{code}

%-----------

An @AbsValEnv@ maps @Ids@ to @AbsVals@.  Any unbound @Ids@ are
implicitly bound to @AbsTop@, the completely uninformative,
pessimistic value---see @absEval@ of a @CoVar@.

\begin{code}
data AbsValEnv = AbsValEnv StrAnalFlags (IdEnv AbsVal)

type StrAnalFlags
  = (Bool,	-- True <=> AllStrict flag is set
     Bool)	-- True <=> NumbersStrict flag is set

type StrictEnv  = AbsValEnv	-- Environment for strictness analysis
type AbsenceEnv = AbsValEnv	-- Environment for absence analysis

nullAbsValEnv flags -- this is the one and only way to create AbsValEnvs
  = AbsValEnv flags nullIdEnv

addOneToAbsValEnv (AbsValEnv x idenv) y z = AbsValEnv x (addOneToIdEnv idenv y z)
growAbsValEnvList (AbsValEnv x idenv) ys  = AbsValEnv x (growIdEnvList idenv ys)

lookupAbsValEnv (AbsValEnv _ idenv) y
  = lookupIdEnv idenv y

getStrAnalFlags (AbsValEnv flags _) = flags
\end{code}

\begin{code}
absValFromStrictness :: AnalysisKind -> StrictnessInfo -> AbsVal

absValFromStrictness anal NoStrictnessInfo 	       = AbsTop

absValFromStrictness StrAnal BottomGuaranteed 	       = AbsBot -- Guaranteed bottom
absValFromStrictness AbsAnal BottomGuaranteed 	       = AbsTop	-- Check for poison in
								-- arguments (if any)
absValFromStrictness anal (StrictnessInfo []        _) = AbsTop
absValFromStrictness anal (StrictnessInfo args_info _) = AbsApproxFun args_info
\end{code}
