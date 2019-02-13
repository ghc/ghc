{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module Bug where

import GHC.Exts

type Bad = (forall (v1 :: RuntimeRep) (a1 :: TYPE v). a1) :: TYPE v

-- should be accepted because GHC will generalize over v. Note v /= v1.
