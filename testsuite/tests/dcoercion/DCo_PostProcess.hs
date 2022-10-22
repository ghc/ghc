{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module DCo_PostProcess where

data RdrName
data SrcSpanAnnN

type family Anno a
type instance Anno RdrName = SrcSpanAnnN

data Pass = Parsed
data GhcPass (c :: Pass) where
  GhcPs :: GhcPass 'Parsed
type GhcPs = GhcPass 'Parsed

type family IdP p
type instance IdP (GhcPass p) = IdGhcP p
type family IdGhcP pass where
  IdGhcP 'Parsed = RdrName

mkHsOpTy :: (Anno (IdGhcP p) ~ SrcSpanAnnN)
         => IdP (GhcPass p) -> GhcPass p
mkHsOpTy = mkHsOpTy

mkLHsOpTy :: RdrName -> GhcPs
mkLHsOpTy = mkHsOpTy
