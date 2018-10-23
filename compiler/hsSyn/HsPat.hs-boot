{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module HsPat where

import Outputable
import HsExtension      ( OutputableBndrId, GhcPass )

type role Pat nominal
data Pat (i :: *)
type LPat p = Pat p

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (Pat p)
