{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

\section[BasicTypes]{Miscellaneous types}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-
Above flag is necessary for these instances:
  * Binary OverlapMode
  * Outputable OverlapMode
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Types.OverlapFlag (
        -- * OverlapFlag
        -- ** Data-type
        OverlapFlag(..),
        -- ** Smart Constructor
        makeOverlapFlag,
        -- ** Setter
        setOverlapModeMaybe,
   ) where

import GHC.Prelude

import GHC.Hs.Extension

import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Types.OverlapMode (changeOverlapModePass)

import Language.Haskell.Syntax.Decls.Overlap (OverlapMode(..))

import Control.DeepSeq (NFData(..))

{-
************************************************************************
*                                                                      *
                Instance overlap flag
*                                                                      *
************************************************************************
-}

-- | The semantics allowed for overlapping instances for a particular
-- instance. See Note [Safe Haskell isSafeOverlap] in GHC.Core.InstEnv for a
-- explanation of the `isSafeOverlap` field.
--

data OverlapFlag = OverlapFlag
  { isSafeOverlap :: Bool
  , overlapMode   :: OverlapMode GhcPs
  } deriving (Eq)

makeOverlapFlag :: Bool -> OverlapMode (GhcPass p) -> OverlapFlag
makeOverlapFlag safe = OverlapFlag safe . changeOverlapModePass

setOverlapModeMaybe :: OverlapFlag -> Maybe (OverlapMode (GhcPass p)) -> OverlapFlag
setOverlapModeMaybe f Nothing  = f
setOverlapModeMaybe f (Just m) = makeOverlapFlag (isSafeOverlap f) m

instance Binary OverlapFlag where
    put_ bh flag = do put_ bh (overlapMode flag)
                      put_ bh (isSafeOverlap flag)
    get bh = do
        h <- get bh
        b <- get bh
        return OverlapFlag { overlapMode = h, isSafeOverlap = b }

instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

instance NFData OverlapFlag where
  rnf (OverlapFlag mode safe) = rnf mode `seq` rnf safe

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty
