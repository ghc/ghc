{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module Test23887 where
-- based on T13343.hs
import GHC.Exts

type Bad :: forall v . TYPE v
type Bad @v = (forall (v1 :: RuntimeRep) (a1 :: TYPE v). a1) :: TYPE v

-- Note v /= v1.
