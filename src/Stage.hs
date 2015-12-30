{-# LANGUAGE DeriveGeneric #-}
module Stage (Stage (..), stageString) where

import Base
import GHC.Generics (Generic)

-- TODO: explain stages
data Stage = Stage0 | Stage1 | Stage2 | Stage3
           deriving (Show, Eq, Ord, Enum, Generic)

stageString :: Stage -> String
stageString stage = "stage" ++ show (fromEnum stage)

-- Instances for storing in the Shake database
instance Binary Stage
instance Hashable Stage
instance NFData Stage
