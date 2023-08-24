{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeAbstractions #-}

module Bug where

import GHC.Exts

type Bad :: forall v . TYPE v
type Bad @v = (forall (v1 :: RuntimeRep) (a1 :: TYPE v). a1) :: TYPE v

-- Note v /= v1.
