{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleInstances #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)
import Outputable
import HsExtension      ( SourceTextX, DataId, OutputableBndrId, GhcPass )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId p) => Data (Pat p)
instance (SourceTextX (GhcPass p), OutputableBndrId (GhcPass p))
       => Outputable (Pat (GhcPass p))
