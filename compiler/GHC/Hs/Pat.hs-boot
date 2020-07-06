{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Pat where

import GHC.Utils.Outputable
import GHC.Hs.Extension (OutputableBndrId, GhcPass, XRec )
import Data.Kind

type role Pat nominal
data Pat (i :: Type)
type LPat i = XRec i (Pat i)

instance (OutputableBndrId p) => Outputable (Pat (GhcPass p))

pprLPat :: (OutputableBndrId p) => LPat (GhcPass p) -> SDoc
