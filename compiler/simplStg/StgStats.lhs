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
{-# LANGUAGE CPP #-}

module StgStats ( showStgStats ) where

#include "HsVersions.h"

import StgSyn

import Id (Id)
import Panic

import Data.Map (Map)
import qualified Data.Map as Map
\end{code}

\begin{code}
data CounterType
  = Literals
  | Applications
  | ConstructorApps
  | PrimitiveApps
  | LetNoEscapes
  | StgCases
  | FreeVariables
  | ConstructorBinds Bool{-True<=>top-level-}
  | ReEntrantBinds   Bool{-ditto-}
  | SingleEntryBinds Bool{-ditto-}
  | UpdatableBinds   Bool{-ditto-}
  deriving (Eq, Ord)

type Count      = Int
type StatEnv    = Map CounterType Count
\end{code}

\begin{code}
emptySE :: StatEnv
emptySE = Map.empty

combineSE :: StatEnv -> StatEnv -> StatEnv
combineSE = Map.unionWith (+)

combineSEs :: [StatEnv] -> StatEnv
combineSEs = foldr combineSE emptySE

countOne :: CounterType -> StatEnv
countOne c = Map.singleton c 1

countN :: CounterType -> Int -> StatEnv
countN = Map.singleton
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Top-level list of bindings (a ``program'')}
%*                                                                      *
%************************************************************************

\begin{code}
showStgStats :: [StgBinding] -> String

showStgStats prog
  = "STG Statistics:\n\n"
    ++ concat (map showc (Map.toList (gatherStgStats prog)))
  where
    showc (x,n) = (showString (s x) . shows n) "\n"

    s Literals                = "Literals                   "
    s Applications            = "Applications               "
    s ConstructorApps         = "ConstructorApps            "
    s PrimitiveApps           = "PrimitiveApps              "
    s LetNoEscapes            = "LetNoEscapes               "
    s StgCases                = "StgCases                   "
    s FreeVariables           = "FreeVariables              "
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
%*                                                                      *
\subsection{Bindings}
%*                                                                      *
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

statRhs top (_, StgRhsCon _ _ _)
  = countOne (ConstructorBinds top)

statRhs top (_, StgRhsClosure _ _ fv u _ _ body)
  = statExpr body                       `combineSE`
    countN FreeVariables (length fv)    `combineSE`
    countOne (
      case u of
        ReEntrant   -> ReEntrantBinds   top
        Updatable   -> UpdatableBinds   top
        SingleEntry -> SingleEntryBinds top
    )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Expressions}
%*                                                                      *
%************************************************************************

\begin{code}
statExpr :: StgExpr -> StatEnv

statExpr (StgApp _ _)     = countOne Applications
statExpr (StgLit _)       = countOne Literals
statExpr (StgConApp _ _)  = countOne ConstructorApps
statExpr (StgOpApp _ _ _) = countOne PrimitiveApps
statExpr (StgSCC _ _ _ e) = statExpr e
statExpr (StgTick _ _ e)  = statExpr e

statExpr (StgLetNoEscape _ _ binds body)
  = statBinding False{-not top-level-} binds    `combineSE`
    statExpr body                               `combineSE`
    countOne LetNoEscapes

statExpr (StgLet binds body)
  = statBinding False{-not top-level-} binds    `combineSE`
    statExpr body

statExpr (StgCase expr _ _ _ _ _ alts)
  = statExpr expr       `combineSE`
    stat_alts alts      `combineSE`
    countOne StgCases
  where
    stat_alts alts
        = combineSEs (map statExpr [ e | (_,_,_,e) <- alts ])

statExpr (StgLam {}) = panic "statExpr StgLam"
\end{code}

