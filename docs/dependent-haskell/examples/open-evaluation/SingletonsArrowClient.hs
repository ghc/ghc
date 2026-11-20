{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module SingletonsArrowClient where

import SingletonsArrowProvider

type instance PromoteX Maybe = TyCon1 Maybe
