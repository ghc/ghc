{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Pat where

import GHC.Utils.Outputable
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )

import Language.Haskell.Syntax.Pat

instance (OutputableBndrId p) => Outputable (Pat (GhcPass p))
instance (OutputableBndrId p) => Outputable (MatchPat (GhcPass p))

pprLPat :: (OutputableBndrId p) => LPat (GhcPass p) -> SDoc
pprLMatchPat :: (OutputableBndrId p) => LMatchPat (GhcPass p) -> SDoc
