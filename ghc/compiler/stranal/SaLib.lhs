%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[SaLib]{Basic datatypes, functions for the strictness analyser}

See also: the ``library'' for the ``back end'' (@SaBackLib@).

\begin{code}
#include "HsVersions.h"

module SaLib (
	AbsVal(..),
	AnalysisKind(..),
	AbsValEnv{-abstract-}, SYN_IE(StrictEnv), SYN_IE(AbsenceEnv),
	nullAbsValEnv, addOneToAbsValEnv, growAbsValEnvList,
	lookupAbsValEnv,
	absValFromStrictness
    ) where

IMP_Ubiq(){-uitous-}

import CoreSyn		( SYN_IE(CoreExpr) )
import Id		( nullIdEnv, addOneToIdEnv, growIdEnvList,
			  lookupIdEnv, SYN_IE(IdEnv),
			  GenId{-instance Outputable-}, SYN_IE(Id)
			)
import IdInfo		( StrictnessInfo(..) )
import Demand		( Demand{-instance Outputable-} )
import Outputable	( Outputable(..){-instance * []-} )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ptext, hsep, char )
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
	    Id		    -- argument
	    CoreExpr	    -- body
	    AbsValEnv	    -- and environment

  | AbsApproxFun	    -- This is used to represent a coarse
	    Demand	    -- approximation to a function value.  It's an
	    AbsVal	    -- abstract function which is strict in its
			    -- argument if the  Demand so indicates.

instance Outputable AbsVal where
    ppr sty AbsTop = ptext SLIT("AbsTop")
    ppr sty AbsBot = ptext SLIT("AbsBot")
    ppr sty (AbsProd prod) = hsep [ptext SLIT("AbsProd"), ppr sty prod]
    ppr sty (AbsFun arg body env)
      = hsep [ptext SLIT("AbsFun{"), ppr sty arg,
	       ptext SLIT("???"), -- text "}{env:", ppr sty (keysFM env `zip` eltsFM env),
	       char '}' ]
    ppr sty (AbsApproxFun demand val)
      = hsep [ptext SLIT("AbsApprox "), ppr sty demand, ppr sty val ]
\end{code}

%-----------

An @AbsValEnv@ maps @Ids@ to @AbsVals@.  Any unbound @Ids@ are
implicitly bound to @AbsTop@, the completely uninformative,
pessimistic value---see @absEval@ of a @Var@.

\begin{code}
data AbsValEnv = AbsValEnv (IdEnv AbsVal)

type StrictEnv  = AbsValEnv	-- Environment for strictness analysis
type AbsenceEnv = AbsValEnv	-- Environment for absence analysis

nullAbsValEnv -- this is the one and only way to create AbsValEnvs
  = AbsValEnv nullIdEnv

addOneToAbsValEnv (AbsValEnv idenv) y z = AbsValEnv (addOneToIdEnv idenv y z)
growAbsValEnvList (AbsValEnv idenv) ys  = AbsValEnv (growIdEnvList idenv ys)

lookupAbsValEnv (AbsValEnv idenv) y
  = lookupIdEnv idenv y
\end{code}

\begin{code}
absValFromStrictness :: AnalysisKind -> StrictnessInfo bdee -> AbsVal

absValFromStrictness anal NoStrictnessInfo 	       = AbsTop

absValFromStrictness StrAnal BottomGuaranteed 	       = AbsBot -- Guaranteed bottom
absValFromStrictness AbsAnal BottomGuaranteed 	       = AbsTop	-- Check for poison in
								-- arguments (if any)
absValFromStrictness anal (StrictnessInfo args_info _) = foldr AbsApproxFun AbsTop args_info
\end{code}
