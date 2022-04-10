{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Orphans where

-- import Data.Default
import GHC hiding (EpaComment)

-- ---------------------------------------------------------------------

class Default a where
  def :: a

-- ---------------------------------------------------------------------
-- Orphan Default instances. See https://gitlab.haskell.org/ghc/ghc/-/issues/20372

instance Default [a] where
  def = []

instance Default NameAnn where
  def = mempty

instance Default AnnList where
  def = mempty

instance Default AnnListItem where
  def = mempty

instance Default AnnPragma where
  def = AnnPragma def def def

instance Semigroup EpAnnImportDecl where
  (<>) = error "unimplemented"
instance Default EpAnnImportDecl where
  def = EpAnnImportDecl def  Nothing  Nothing  Nothing  Nothing  Nothing

instance Default HsRuleAnn where
  def = HsRuleAnn Nothing Nothing def

instance Default AnnSig where
  def = AnnSig def  def

instance Default GrhsAnn where
  def = GrhsAnn Nothing  def

instance Default EpAnnUnboundVar where
  def = EpAnnUnboundVar def  def

instance (Default a, Default b) => Default (a, b) where
  def = (def, def)

instance Default NoEpAnns where
  def = NoEpAnns

instance Default AnnParen where
  def = AnnParen AnnParens def  def

instance Default AnnExplicitSum where
  def = AnnExplicitSum def  def  def  def

instance Default EpAnnHsCase where
  def = EpAnnHsCase def def def

instance Default AnnsIf where
  def = AnnsIf def def def def def

instance Default (Maybe a) where
  def = Nothing

instance Default AnnProjection where
  def = AnnProjection def def

instance Default AnnFieldLabel where
  def = AnnFieldLabel Nothing

instance Default EpaLocation where
  def = EpaDelta (SameLine 0) []

instance Default AddEpAnn where
  def = AddEpAnn def def

instance Default AnnKeywordId where
  def = Annlarrowtail  {- gotta pick one -}

instance Default AnnContext where
  def = AnnContext Nothing [] []

instance Default EpAnnSumPat where
  def = EpAnnSumPat def  def  def

instance Default AnnsModule where
  def = AnnsModule [] mempty
