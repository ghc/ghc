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

instance NoAnn [a] where
  noAnn = []

instance NoAnn AnnPragma where
  noAnn = AnnPragma noAnn noAnn noAnn

instance Semigroup EpAnnImportDecl where
  (<>) = error "unimplemented"
instance NoAnn EpAnnImportDecl where
  noAnn = EpAnnImportDecl noAnn  Nothing  Nothing  Nothing  Nothing  Nothing

instance NoAnn AnnParen where
  noAnn = AnnParen AnnParens noAnn noAnn

instance NoAnn HsRuleAnn where
  noAnn = HsRuleAnn Nothing Nothing noAnn

instance NoAnn AnnSig where
  noAnn = AnnSig noAnn  noAnn

instance NoAnn GrhsAnn where
  noAnn = GrhsAnn Nothing  noAnn

instance NoAnn EpAnnUnboundVar where
  noAnn = EpAnnUnboundVar noAnn  noAnn

instance (NoAnn a, NoAnn b) => NoAnn (a, b) where
  noAnn = (noAnn, noAnn)

instance NoAnn AnnExplicitSum where
  noAnn = AnnExplicitSum noAnn  noAnn  noAnn  noAnn

instance NoAnn EpAnnHsCase where
  noAnn = EpAnnHsCase noAnn noAnn noAnn

instance NoAnn AnnsIf where
  noAnn = AnnsIf noAnn noAnn noAnn noAnn noAnn

instance NoAnn (Maybe a) where
  noAnn = Nothing

instance NoAnn AnnProjection where
  noAnn = AnnProjection noAnn noAnn

instance NoAnn AnnFieldLabel where
  noAnn = AnnFieldLabel Nothing

instance NoAnn EpaLocation where
  noAnn = EpaDelta (SameLine 0) []

instance NoAnn AddEpAnn where
  noAnn = AddEpAnn noAnn noAnn

instance NoAnn AnnKeywordId where
  noAnn = Annlarrowtail  {- gotta pick one -}

instance NoAnn AnnContext where
  noAnn = AnnContext Nothing [] []

instance NoAnn EpAnnSumPat where
  noAnn = EpAnnSumPat noAnn  noAnn  noAnn

instance NoAnn AnnsModule where
  noAnn = AnnsModule [] mempty Nothing
