{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Pat where

import Outputable
import GHC.Hs.Extension ( OutputableBndrId, GhcPass, WrapL )

type role Pat nominal
data Pat (i :: *)
type LPat i = WrapL i Pat

instance OutputableBndrId (GhcPass p) => Outputable (Pat (GhcPass p))
