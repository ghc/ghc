{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Pat where

import Outputable
import GHC.Hs.Extension ( OutputableBndrId, GhcPass, Loc )

type role Pat nominal
data Pat (i :: *)
type LPat i = Pat (Loc i)

instance OutputableBndrId (GhcPass p) => Outputable (LPat (GhcPass p))
instance OutputableBndrId (GhcPass p) => Outputable (Pat (GhcPass p))
