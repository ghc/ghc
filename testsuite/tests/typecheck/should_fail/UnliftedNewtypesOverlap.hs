{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module UnliftedNewtypesOverlap where

import GHC.Exts

data family DF :: TYPE r
data instance DF @LiftedRep = MkDF4 | MkDF5
newtype instance DF @LiftedRep = MkDF6 Int
