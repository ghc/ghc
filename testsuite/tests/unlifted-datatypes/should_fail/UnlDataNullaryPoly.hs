{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE PolyKinds #-}

module UnlDataNullaryPoly where

import GHC.Exts
import GHC.Types

type T :: TYPE (BoxedRep l)
data T = MkT -- Not OK, we get (MkT :: forall l. T @l :: TYPE (BoxedRep l)) which is ill-kinded.
