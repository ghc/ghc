{-# LANGUAGE TypeFamilies, PartialTypeSignatures #-}
module RepPolyPartialSig where

import GHC.Exts

type family A :: RuntimeRep

f x = x :: (_ :: TYPE A)
