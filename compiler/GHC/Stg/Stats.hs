{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

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
-}


module GHC.Stg.Stats ( showStgStats ) where

import GHC.Prelude

import GHC.Stg.Syntax

import GHC.Types.Id (Id)

import Data.Map (Map)
import qualified Data.Map as Map

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
  | JoinPointBinds   Bool{-ditto-}
  deriving (Eq, Ord)

type Count      = Int
type StatEnv    = Map CounterType Count

emptySE :: StatEnv
emptySE = Map.empty

combineSE :: StatEnv -> StatEnv -> StatEnv
combineSE = Map.unionWith (+)

combineSEs :: [StatEnv] -> StatEnv
combineSEs = foldr combineSE emptySE

countOne :: CounterType -> StatEnv
countOne c = Map.singleton c 1

{-
************************************************************************
*                                                                      *
\subsection{Top-level list of bindings (a ``program'')}
*                                                                      *
************************************************************************
-}

showStgStats :: [StgTopBinding] -> String

showStgStats prog
  = "STG Statistics:\n\n"
    ++ concatMap showc (Map.toList (gatherStgStats prog))
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
    s (JoinPointBinds _)      = "JoinPointBinds_Nested      "

gatherStgStats :: [StgTopBinding] -> StatEnv
gatherStgStats binds = combineSEs (map statTopBinding binds)

{-
************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************
-}

statTopBinding :: StgTopBinding -> StatEnv
statTopBinding (StgTopStringLit _ _) = countOne Literals
statTopBinding (StgTopLifted bind) = statBinding True bind

statBinding :: Bool -- True <=> top-level; False <=> nested
            -> StgBinding
            -> StatEnv

statBinding top (StgNonRec b rhs)
  = statRhs top (b, rhs)

statBinding top (StgRec pairs)
  = combineSEs (map (statRhs top) pairs)

statRhs :: Bool -> (Id, StgRhs) -> StatEnv

statRhs top (_, StgRhsCon _ _ _ _ _ _)
  = countOne (ConstructorBinds top)

statRhs top (_, StgRhsClosure _ _ u _ body _)
  = statExpr body `combineSE`
    countOne (
      case u of
        ReEntrant   -> ReEntrantBinds   top
        Updatable   -> UpdatableBinds   top
        SingleEntry -> SingleEntryBinds top
        JumpedTo    -> JoinPointBinds   top
    )

{-
************************************************************************
*                                                                      *
\subsection{Expressions}
*                                                                      *
************************************************************************
-}

statExpr :: StgExpr -> StatEnv

statExpr (StgApp _ _)     = countOne Applications
statExpr (StgLit _)       = countOne Literals
statExpr (StgConApp {})   = countOne ConstructorApps
statExpr (StgOpApp _ _ _) = countOne PrimitiveApps
statExpr (StgTick _ e)    = statExpr e

statExpr (StgLetNoEscape _ binds body)
  = statBinding False{-not top-level-} binds    `combineSE`
    statExpr body                               `combineSE`
    countOne LetNoEscapes

statExpr (StgLet _ binds body)
  = statBinding False{-not top-level-} binds    `combineSE`
    statExpr body

statExpr (StgCase expr _ _ alts)
  = statExpr expr       `combineSE`
    stat_alts alts      `combineSE`
    countOne StgCases
  where
    stat_alts = combineSEs . fmap (statExpr . alt_rhs)
