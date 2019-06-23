{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module UnliftedNewtypesOverlap where

import GHC.Exts (TYPE)

data family DF :: TYPE r
data instance DF = MkDF4 | MkDF5
newtype instance DF = MkDF6 Int
