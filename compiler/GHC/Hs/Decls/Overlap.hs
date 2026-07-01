{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- XOverlapMode, XXOverlapMode
{-# OPTIONS_GHC -fno-warn-orphans #-} -- XOverlapMode, XXOverlapMode

{- |
Data-types describing the overlap annotations for instances as well as
interpreting the instances usage within the Safe Haskell context.
-}
module GHC.Hs.Decls.Overlap (
        -- * OverlapFlag
        -- ** Data-type
        OverlapFlag(..),

        -- * OverlapMode
        -- ** Data-type
        OverlapMode(..),
        -- ** Queries
        hasOverlappableFlag,
        hasOverlappingFlag,
        hasIncoherentFlag,
        hasNonCanonicalFlag,
    ) where

import GHC.Prelude

import GHC.Hs.Extension

import GHC.Parser.Annotation ( AnnPragma )

import Language.Haskell.Syntax.Decls.Overlap
import Language.Haskell.Syntax.Extension

import GHC.Types.SourceText
import GHC.Utils.Binary
import GHC.Utils.Outputable

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
data OverlapFlag = OverlapFlag
  { isSafeOverlap :: Bool
  , overlapMode   :: OverlapMode GhcTc
  } deriving (Eq)

instance Binary OverlapFlag where
    put_ bh flag = do put_ bh (overlapMode flag)
                      put_ bh (isSafeOverlap flag)
    get bh = do
        h <- get bh
        b <- get bh
        return OverlapFlag { isSafeOverlap = b, overlapMode = h }

instance NFData OverlapFlag where
    rnf (OverlapFlag mode safe) = rnf mode `seq` rnf safe

instance Outputable OverlapFlag where
    ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

type instance XOverlapMode  GhcPs = (SourceText, AnnPragma)
type instance XOverlapMode  GhcRn = (SourceText, AnnPragma)
type instance XOverlapMode  GhcTc = SourceText

type instance XXOverlapMode (GhcPass _) = DataConCantHappen

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty
