{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Worphans #-}
module T22717_fam_orph where

import T22717_fam_orph_a

data T

type instance F Int = T -- Orphan instance!
