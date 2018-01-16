{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module T5939 where

import GHC.Generics

data T a = T a
deriving instance Generic (T Bool)

data family TFam a b c
data instance TFam Int b c
deriving instance Generic (TFam Int Bool c)
