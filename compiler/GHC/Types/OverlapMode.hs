{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998
-}

{-# LANGUAGE TypeFamilies #-}

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

import {-# SOURCE #-} GHC.Hs.Extension (GhcPass)

import GHC.Types.SourceText

import Language.Haskell.Syntax.Basic (OverlapMode(..))
import Language.Haskell.Syntax.Extension

import Control.DeepSeq ( NFData(..) )

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
