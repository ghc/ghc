{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Hs.OverlapPragma(
  module Language.Haskell.Syntax.OverlapPragma
  , NonCanonical(..)
  , OverlapFlag(..)
  , hasIncoherentFlag
  , hasOverlappableFlag
  , hasOverlappingFlag
  , hasNonCanonicalFlag
  , pprSafeOverlap
  , convertOverlapMode
) where

import GHC.Prelude

import Language.Haskell.Syntax.OverlapPragma
import Language.Haskell.Syntax.Extension

import GHC.Hs.Extension (GhcPass, GhcTc)

import GHC.Types.SourceText

import GHC.Utils.Binary
import GHC.Utils.Outputable

import GHC.Utils.Panic (panic)

------------------------
-- type family instances

type instance XNoOverlap    (GhcPass _) = SourceText
type instance XOverlappable (GhcPass _) = SourceText
type instance XOverlapping  (GhcPass _) = SourceText
type instance XOverlaps     (GhcPass _) = SourceText
type instance XIncoherent   (GhcPass _) = SourceText
type instance XXOverlapMode (GhcPass _) = NonCanonical
newtype NonCanonical = NonCanonical SourceText
  deriving (Eq)
    -- ^ Behave like Incoherent, but the instance choice is observable
    -- by the program behaviour. See Note [Coherence and specialisation: overview].
    --
    -- We don't have surface syntax for the distinction between
    -- Incoherent and NonCanonical instances; instead, the flag
    -- `-f{no-}specialise-incoherents` (on by default) controls
    -- whether `INCOHERENT` instances are regarded as Incoherent or
    -- NonCanonical.


-----------------------
-- converting
convertOverlapMode :: OverlapMode (GhcPass p) -> OverlapMode (GhcPass p')
convertOverlapMode = \case
  NoOverlap s    -> NoOverlap s
  Overlappable s -> Overlappable s
  Overlapping s  -> Overlapping s
  Overlaps s     -> Overlaps s
  Incoherent s   -> Incoherent s
  XOverlapMode s -> XOverlapMode s

------------------------
-- overlap flag
data OverlapFlag = OverlapFlag
  { overlapMode   :: OverlapMode GhcTc
  , isSafeOverlap :: Bool
  } deriving (Eq)

------------------------
-- deriving instances
deriving instance Eq (OverlapMode (GhcPass p))

------------------------
-- hand rolled instances
instance Outputable (OverlapMode (GhcPass p)) where
  ppr (NoOverlap    _)                = empty
  ppr (Overlappable _)                = text "[overlappable]"
  ppr (Overlapping  _)                = text "[overlapping]"
  ppr (Overlaps     _)                = text "[overlap ok]"
  ppr (Incoherent   _)                = text "[incoherent]"
  ppr (XOverlapMode (NonCanonical _)) = text "[noncanonical]"


instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

-- might want to make an explicit IfaceOverlapMode, I guess
instance Binary (OverlapMode (GhcPass p)) where
    put_ bh (NoOverlap    s)                = putByte bh 0 >> put_ bh s
    put_ bh (Overlaps     s)                = putByte bh 1 >> put_ bh s
    put_ bh (Incoherent   s)                = putByte bh 2 >> put_ bh s
    put_ bh (Overlapping  s)                = putByte bh 3 >> put_ bh s
    put_ bh (Overlappable s)                = putByte bh 4 >> put_ bh s
    put_ bh (XOverlapMode (NonCanonical s)) = putByte bh 5 >> put_ bh s
    get bh = do
        h <- getByte bh
        case h of
            0 -> (get bh) >>= \s -> return $ NoOverlap s
            1 -> (get bh) >>= \s -> return $ Overlaps s
            2 -> (get bh) >>= \s -> return $ Incoherent s
            3 -> (get bh) >>= \s -> return $ Overlapping s
            4 -> (get bh) >>= \s -> return $ Overlappable s
            5 -> (get bh) >>= \s -> return $ XOverlapMode (NonCanonical s)
            _ -> panic ("get OverlapMode" ++ show h)


instance Binary OverlapFlag where
    put_ bh flag = do put_ bh (overlapMode flag)
                      put_ bh (isSafeOverlap flag)
    get bh = do
        h <- get bh
        b <- get bh
        return OverlapFlag { overlapMode = h, isSafeOverlap = b }


------------------------
-- helper functions
hasIncoherentFlag :: OverlapMode (GhcPass p) -> Bool
hasIncoherentFlag = \case
  Incoherent   _                -> True
  XOverlapMode (NonCanonical _) -> True
  _                             -> False

hasOverlappableFlag :: OverlapMode (GhcPass p) -> Bool
hasOverlappableFlag = \case
  Overlappable _                -> True
  Overlaps     _                -> True
  Incoherent   _                -> True
  XOverlapMode (NonCanonical _) -> True
  _                             -> False

hasOverlappingFlag :: OverlapMode (GhcPass p) -> Bool
hasOverlappingFlag = \case
  Overlapping  _                -> True
  Overlaps     _                -> True
  Incoherent   _                -> True
  XOverlapMode (NonCanonical _) -> True
  _                             -> False

hasNonCanonicalFlag :: OverlapMode (GhcPass p) -> Bool
hasNonCanonicalFlag = \case
  XOverlapMode (NonCanonical _) -> True
  _                             -> False

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty
