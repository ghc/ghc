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
	SYN_IE(StrAnalFlags), getStrAnalFlags,
	nullAbsValEnv, addOneToAbsValEnv, growAbsValEnvList,
	lookupAbsValEnv,
	absValFromStrictness
    ) where

IMP_Ubiq(){-uitous-}

import CoreSyn		( SYN_IE(CoreExpr) )
import Id		( nullIdEnv, addOneToIdEnv, growIdEnvList,
			  lookupIdEnv, SYN_IE(IdEnv),
			  GenId{-instance Outputable-}
			)
import IdInfo		( StrictnessInfo(..) )
import Demand		( Demand{-instance Outputable-} )
import Outputable	( Outputable(..){-instance * []-} )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ppPStr, ppCat, ppChar )
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
    ppr sty AbsTop = ppPStr SLIT("AbsTop")
    ppr sty AbsBot = ppPStr SLIT("AbsBot")
    ppr sty (AbsProd prod) = ppCat [ppPStr SLIT("AbsProd"), ppr sty prod]
    ppr sty (AbsFun arg body env)
      = ppCat [ppPStr SLIT("AbsFun{"), ppr sty arg,
	       ppPStr SLIT("???"), -- ppStr "}{env:", ppr sty (keysFM env `zip` eltsFM env),
	       ppChar '}' ]
    ppr sty (AbsApproxFun demand val)
      = ppCat [ppPStr SLIT("AbsApprox "), ppr sty demand, ppr sty val ]
\end{code}

%-----------

An @AbsValEnv@ maps @Ids@ to @AbsVals@.  Any unbound @Ids@ are
implicitly bound to @AbsTop@, the completely uninformative,
pessimistic value---see @absEval@ of a @Var@.

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
absValFromStrictness :: AnalysisKind -> StrictnessInfo bdee -> AbsVal

absValFromStrictness anal NoStrictnessInfo 	       = AbsTop

absValFromStrictness StrAnal BottomGuaranteed 	       = AbsBot -- Guaranteed bottom
absValFromStrictness AbsAnal BottomGuaranteed 	       = AbsTop	-- Check for poison in
								-- arguments (if any)
absValFromStrictness anal (StrictnessInfo args_info _) = foldr AbsApproxFun AbsTop args_info
\end{code}
