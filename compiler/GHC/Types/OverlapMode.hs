{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998
-}

{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
  * (type class):  Binary OverlapMode
  * (type family): XOverlapMode  (GhcPass p)
  * (type family): XXOverlapMode (GhcPass p)
-}

module GHC.Types.OverlapMode (
        -- * OverlapMode
        -- ** Data-type
        OverlapMode(..),
        -- ** Queries
        hasOverlappableFlag,
        hasOverlappingFlag,
        hasIncoherentFlag,
        hasNonCanonicalFlag,
        -- ** GHC Pass Alteration
        changeOverlapModePass,
        changeOverlapModeType
   ) where

import GHC.Prelude

import GHC.Hs.Extension (GhcPass)
import GHC.Types.SourceText
import GHC.Utils.Binary

import Language.Haskell.Syntax.Decls.Overlap (OverlapMode(..))
import Language.Haskell.Syntax.Extension

type instance XOverlapMode  (GhcPass _) = SourceText
type instance XXOverlapMode (GhcPass _) = DataConCantHappen

changeOverlapModePass :: OverlapMode (GhcPass p) -> OverlapMode (GhcPass q)
changeOverlapModePass = changeOverlapModeType id

changeOverlapModeType :: forall p q.
  (XOverlapMode (GhcPass p) -> XOverlapMode q) ->
  OverlapMode (GhcPass p) ->
  OverlapMode q
changeOverlapModeType f = \case
  NoOverlap    s -> NoOverlap    $ f s
  Overlappable s -> Overlappable $ f s
  Overlapping  s -> Overlapping  $ f s
  Overlaps     s -> Overlaps     $ f s
  Incoherent   s -> Incoherent   $ f s
  NonCanonical s -> NonCanonical $ f s

hasIncoherentFlag :: OverlapMode (GhcPass p) -> Bool
hasIncoherentFlag mode =
  case mode of
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasOverlappableFlag :: OverlapMode (GhcPass p) -> Bool
hasOverlappableFlag mode =
  case mode of
    Overlappable _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasOverlappingFlag :: OverlapMode (GhcPass p) -> Bool
hasOverlappingFlag mode =
  case mode of
    Overlapping  _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    NonCanonical _ -> True
    _              -> False

hasNonCanonicalFlag :: OverlapMode (GhcPass p) -> Bool
hasNonCanonicalFlag = \case
  NonCanonical{} -> True
  _              -> False

instance Binary (OverlapMode (GhcPass p)) where
    put_ bh (NoOverlap    s) = putByte bh 0 >> put_ bh s
    put_ bh (Overlaps     s) = putByte bh 1 >> put_ bh s
    put_ bh (Incoherent   s) = putByte bh 2 >> put_ bh s
    put_ bh (Overlapping  s) = putByte bh 3 >> put_ bh s
    put_ bh (Overlappable s) = putByte bh 4 >> put_ bh s
    put_ bh (NonCanonical s) = putByte bh 5 >> put_ bh s

    get bh = do
        h <- getByte bh
        case h of
            0 -> (get bh) >>= \s -> return $ NoOverlap s
            1 -> (get bh) >>= \s -> return $ Overlaps s
            2 -> (get bh) >>= \s -> return $ Incoherent s
            3 -> (get bh) >>= \s -> return $ Overlapping s
            4 -> (get bh) >>= \s -> return $ Overlappable s
            _ -> (get bh) >>= \s -> return $ NonCanonical s
