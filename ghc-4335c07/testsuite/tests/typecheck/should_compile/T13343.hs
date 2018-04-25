{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Bug where

import GHC.Exts

type Bad = forall (v1 :: RuntimeRep) (a1 :: TYPE v). a1
