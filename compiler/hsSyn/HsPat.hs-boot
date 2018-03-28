{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)
import Outputable
import HsExtension      ( DataId, OutputableBndrId, GhcPass )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId p) => Data (Pat p)
instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (Pat p)
