%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
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
module StgStats ( showStgStats ) where

#include "HsVersions.h"

import StgSyn

import Const		( Con(..) )
import FiniteMap	( emptyFM, plusFM_C, unitFM, fmToList, FiniteMap )
import Id (Id)
\end{code}

\begin{code}
data CounterType
  = Literals
  | Applications
  | ConstructorApps
  | PrimitiveApps
  | LetNoEscapes
  | AlgCases
  | PrimCases
  | FreeVariables
  | ConstructorBinds Bool{-True<=>top-level-}
  | ReEntrantBinds   Bool{-ditto-}
  | SingleEntryBinds Bool{-ditto-}
  | UpdatableBinds   Bool{-ditto-}
  deriving (Eq, Ord)

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
countOne c = unitFM c 1

countN :: CounterType -> Int -> StatEnv
countN = unitFM
\end{code}

%************************************************************************
%*									*
\subsection{Top-level list of bindings (a ``program'')}
%*									*
%************************************************************************

\begin{code}
showStgStats :: [StgBinding] -> String

showStgStats prog
  = "STG Statistics:\n\n"
    ++ concat (map showc (fmToList (gatherStgStats prog)))
  where
    showc (x,n) = (showString (s x) . shows n) "\n"

    s Literals		      = "Literals                   "
    s Applications	      = "Applications               "
    s ConstructorApps	      = "ConstructorApps            "
    s PrimitiveApps	      = "PrimitiveApps              "
    s LetNoEscapes	      = "LetNoEscapes               "
    s AlgCases		      = "AlgCases                   "
    s PrimCases		      = "PrimCases                  "
    s FreeVariables	      = "FreeVariables              "
    s (ConstructorBinds True) = "ConstructorBinds_Top       "
    s (ReEntrantBinds True)   = "ReEntrantBinds_Top         "
    s (SingleEntryBinds True) = "SingleEntryBinds_Top       "
    s (UpdatableBinds True)   = "UpdatableBinds_Top         "
    s (ConstructorBinds _)    = "ConstructorBinds_Nested    "
    s (ReEntrantBinds _)      = "ReEntrantBindsBinds_Nested "
    s (SingleEntryBinds _)    = "SingleEntryBinds_Nested    "
    s (UpdatableBinds _)      = "UpdatableBinds_Nested      "

gatherStgStats :: [StgBinding] -> StatEnv

gatherStgStats binds
  = combineSEs (map (statBinding True{-top-level-}) binds)
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
statBinding :: Bool -- True <=> top-level; False <=> nested
	    -> StgBinding
	    -> StatEnv

statBinding top (StgNonRec b rhs)
  = statRhs top (b, rhs)

statBinding top (StgRec pairs)
  = combineSEs (map (statRhs top) pairs)

statRhs :: Bool -> (Id, StgRhs) -> StatEnv

statRhs top (b, StgRhsCon cc con args)
  = countOne (ConstructorBinds top)

statRhs top (b, StgRhsClosure cc bi srt fv u args body)
  = statExpr body			`combineSE`
    countN FreeVariables (length fv)	`combineSE`
    countOne (
      case u of
	ReEntrant   -> ReEntrantBinds   top
	Updatable   -> UpdatableBinds   top
	SingleEntry -> SingleEntryBinds top
    )
\end{code}

%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}
statExpr :: StgExpr -> StatEnv

statExpr (StgApp _ _)
  = countOne Applications

statExpr (StgCon (DataCon _) as _)
  = countOne ConstructorApps

statExpr (StgCon (PrimOp _) as _)
  = countOne PrimitiveApps

statExpr (StgCon (Literal _) as _)
  = countOne Literals

statExpr (StgSCC l e)
  = statExpr e

statExpr (StgLetNoEscape lvs_whole lvs_rhss binds body)
  = statBinding False{-not top-level-} binds	`combineSE`
    statExpr body				`combineSE`
    countOne LetNoEscapes

statExpr (StgLet binds body)
  = statBinding False{-not top-level-} binds	`combineSE`
    statExpr body

statExpr (StgCase expr lve lva bndr srt alts)
  = statExpr expr	`combineSE`
    stat_alts alts
    where
      stat_alts (StgAlgAlts ty alts def)
	= combineSEs (map statExpr [ e | (_,_,_,e) <- alts ])
					`combineSE`
	  stat_deflt def		`combineSE`
	  countOne AlgCases

      stat_alts (StgPrimAlts ty alts def)
	= combineSEs (map statExpr [ e | (_,e) <- alts ])
					`combineSE`
	  stat_deflt def		`combineSE`
	  countOne PrimCases

      stat_deflt StgNoDefault = emptySE

      stat_deflt (StgBindDefault expr) = statExpr expr
\end{code}

