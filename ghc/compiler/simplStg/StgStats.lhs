%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[StgStats]{Gathers statistical information about programs}


The program gather statistics about
\begin{enumerate}
\item number of boxed cases
\item number of unboxed cases
\item number of let-no-escapes
\item number of non-updatable lets
\item number of updatable lets
\item number of applications
\item number of primitive applications
\item number of closures (does not include lets bound to constructors)
\item number of free variables in closures
%\item number of top-level functions
%\item number of top-level CAFs
\item number of constructors
\end{enumerate}

\begin{code}
#include "HsVersions.h"

module StgStats ( showStgStats ) where

import StgSyn

import FiniteMap

import Util
\end{code}

\begin{code}
data CounterType
  = AlgCases
  | PrimCases
  | LetNoEscapes
  | NonUpdatableLets
  | UpdatableLets
  | Applications
  | PrimitiveApps
  | FreeVariables
  | Closures	-- does not include lets bound to constructors
--| UpdatableTopLevelDefs
--| NonUpdatableTopLevelDefs
  | Constructors
  deriving (Eq, Ord, Text)

type Count	= Int
type StatEnv	= FiniteMap CounterType Count
\end{code}

\begin{code}
emptySE	:: StatEnv
emptySE	= emptyFM

combineSE :: StatEnv -> StatEnv -> StatEnv
combineSE = plusFM_C (+)

combineSEs :: [StatEnv] -> StatEnv
combineSEs = foldr combineSE emptySE

countOne :: CounterType -> StatEnv
countOne c = singletonFM c 1

countN :: CounterType -> Int -> StatEnv
countN = singletonFM
\end{code}

%************************************************************************
%*									*
\subsection{Top-level list of bindings (a ``program'')}
%*									*
%************************************************************************

\begin{code}
showStgStats :: PlainStgProgram -> String
showStgStats prog = concat (map showc (fmToList (gatherStgStats prog)))
  where
    showc (AlgCases,n)         = "AlgCases               " ++ show n ++ "\n"
    showc (PrimCases,n)        = "PrimCases              " ++ show n ++ "\n"
    showc (LetNoEscapes,n)     = "LetNoEscapes           " ++ show n ++ "\n"
    showc (NonUpdatableLets,n) = "NonUpdatableLets       " ++ show n ++ "\n"
    showc (UpdatableLets,n)    = "UpdatableLets          " ++ show n ++ "\n"
    showc (Applications,n)     = "Applications           " ++ show n ++ "\n"
    showc (PrimitiveApps,n)    = "PrimitiveApps          " ++ show n ++ "\n"
    showc (Closures,n)         = "Closures               " ++ show n ++ "\n"
    showc (FreeVariables,n)    = "Free Vars in Closures  " ++ show n ++ "\n"
    showc (Constructors,n)     = "Constructors           " ++ show n ++ "\n"

gatherStgStats :: PlainStgProgram -> StatEnv

gatherStgStats binds 
  = combineSEs (map statBinding binds)
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
statBinding :: PlainStgBinding -> StatEnv

statBinding (StgNonRec b rhs)
  = statRhs (b, rhs)

statBinding (StgRec pairs)
  = combineSEs (map statRhs pairs)

statRhs :: (Id, PlainStgRhs) -> StatEnv

statRhs (b, StgRhsCon cc con args)
  = countOne Constructors		`combineSE` 
    countOne NonUpdatableLets

statRhs (b, StgRhsClosure cc bi fv u args body)
  = statExpr body			`combineSE` 
    countN FreeVariables (length fv)	`combineSE`
    countOne Closures			`combineSE` 
    (case u of
       Updatable -> countOne UpdatableLets
       _         -> countOne NonUpdatableLets)

\end{code}

%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}    
statExpr :: PlainStgExpr -> StatEnv

statExpr (StgApp _ [] lvs) 
  = emptySE
statExpr (StgApp _ _ lvs) 
  = countOne Applications

statExpr (StgConApp con as lvs)
  = countOne Constructors

statExpr (StgPrimApp op as lvs)
  = countOne PrimitiveApps

statExpr (StgSCC ty l e)
  = statExpr e

statExpr (StgLetNoEscape lvs_whole lvs_rhss binds body)
  = statBinding binds	`combineSE`
    statExpr body	`combineSE` 
    countOne LetNoEscapes

statExpr (StgLet binds body)
  = statBinding binds	`combineSE` 
    statExpr body

statExpr (StgCase expr lve lva uniq alts)
  = statExpr expr	`combineSE`
    stat_alts alts
    where
      stat_alts (StgAlgAlts ty alts def)
	= combineSEs (map stat_alg_alt alts)	`combineSE` 
	  stat_deflt def			`combineSE`
	  countOne AlgCases
	where
	  stat_alg_alt (id, bs, use_mask, e)
	    = statExpr e

      stat_alts (StgPrimAlts ty alts def)
	= combineSEs (map stat_prim_alt alts) 	`combineSE`
	  stat_deflt def			`combineSE`
	  countOne PrimCases
	where
	  stat_prim_alt (l, e)
	    = statExpr e

      stat_deflt StgNoDefault
	= emptySE

      stat_deflt (StgBindDefault b u expr)
	= statExpr expr	
\end{code}

