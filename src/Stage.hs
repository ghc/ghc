{-# LANGUAGE DeriveGeneric #-}
module Stage (Stage (..)) where

import Base
import GHC.Generics (Generic)

-- TODO: rename to something more meaningful, e.g. 'Stage0' -> 'Boot'.
-- TODO: explain stages
data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum, Generic)

instance Show Stage where
    show = show . fromEnum

-- Instances for storing in the Shake database
instance Binary Stage
instance Hashable Stage
