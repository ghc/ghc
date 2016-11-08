{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsPat where
import SrcLoc( Located )

import Data.Data hiding (Fixity)
import Outputable
import PlaceHolder      ( DataId, OutputableBndrId,HasOccNameId )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId id) => Data (Pat id)
instance (OutputableBndrId name, HasOccNameId name) => Outputable (Pat name)
