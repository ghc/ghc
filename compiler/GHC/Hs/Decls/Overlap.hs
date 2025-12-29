{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- XOverlapMode, XXOverlapMode

{-# OPTIONS_GHC -fno-warn-orphans #-}
{- Necessary for the following instances:
  * (type class):  Binary OverlapMode
  * (type class):  NFData OverlapMode
-}

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

type instance XOverlapMode  (GhcPass _) = SourceText

type instance XXOverlapMode (GhcPass _) = DataConCantHappen

instance NFData (OverlapMode (GhcPass p)) where
    rnf = \case
        NoOverlap    s -> rnf s
        Overlappable s -> rnf s
        Overlapping  s -> rnf s
        Overlaps     s -> rnf s
        Incoherent   s -> rnf s
        NonCanonical s -> rnf s

instance Binary (OverlapMode (GhcPass p)) where
    put_ bh = \case
        NoOverlap    s -> putByte bh 0 >> put_ bh s
        Overlaps     s -> putByte bh 1 >> put_ bh s
        Incoherent   s -> putByte bh 2 >> put_ bh s
        Overlapping  s -> putByte bh 3 >> put_ bh s
        Overlappable s -> putByte bh 4 >> put_ bh s
        NonCanonical s -> putByte bh 5 >> put_ bh s

    get bh = do
        h <- getByte bh
        case h of
            0 -> get bh >>= \s -> return $ NoOverlap    s
            1 -> get bh >>= \s -> return $ Overlaps     s
            2 -> get bh >>= \s -> return $ Incoherent   s
            3 -> get bh >>= \s -> return $ Overlapping  s
            4 -> get bh >>= \s -> return $ Overlappable s
            _ -> get bh >>= \s -> return $ NonCanonical s

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty
