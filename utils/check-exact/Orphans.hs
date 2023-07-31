{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Orphans where

import GHC hiding (EpaComment)

-- -- ---------------------------------------------------------------------

instance NoAnn [a] where
  noAnn = []

instance (NoAnn a, NoAnn b) => NoAnn (a, b) where
  noAnn = (noAnn, noAnn)

instance NoAnn EpaLocation where
  noAnn = EpaDelta (SameLine 0) []

instance NoAnn EpAnnSumPat where
  noAnn = EpAnnSumPat [] [] []

instance NoAnn AnnPragma where
  noAnn = AnnPragma noAnn noAnn []

instance NoAnn AddEpAnn where
  noAnn = AddEpAnn noAnn noAnn

instance NoAnn AnnKeywordId where
  noAnn = Annlarrowtail  {- gotta pick one -}

instance NoAnn AnnParen where
  noAnn = AnnParen AnnParens noAnn noAnn

instance NoAnn AnnsIf where
  noAnn = AnnsIf noAnn noAnn noAnn Nothing Nothing

instance NoAnn EpAnnHsCase where
  noAnn = EpAnnHsCase noAnn noAnn noAnn

instance NoAnn AnnFieldLabel where
  noAnn = AnnFieldLabel Nothing

instance NoAnn AnnProjection where
  noAnn = AnnProjection noAnn noAnn

instance NoAnn AnnExplicitSum where
  noAnn = AnnExplicitSum noAnn noAnn noAnn noAnn

instance NoAnn EpAnnUnboundVar where
  noAnn = EpAnnUnboundVar noAnn  noAnn

instance NoAnn GrhsAnn where
  noAnn = GrhsAnn Nothing noAnn

instance NoAnn HsRuleAnn where
  noAnn = HsRuleAnn Nothing Nothing noAnn

instance NoAnn AnnSig where
  noAnn = AnnSig noAnn noAnn

instance NoAnn EpAnnImportDecl where
  noAnn = EpAnnImportDecl noAnn  Nothing  Nothing  Nothing  Nothing  Nothing

instance NoAnn AnnsModule where
  noAnn = AnnsModule [] [] Nothing
