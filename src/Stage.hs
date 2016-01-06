{-# LANGUAGE DeriveGeneric #-}
module Stage (Stage (..), stageString) where

import Development.Shake.Classes
import GHC.Generics (Generic)

-- | A stage refers to a certain compiler in GHC's build process.
--
-- * Stage 0 is the bootstrapping compiler, i.e. the one already installed on
--   the user's system.
--
-- * Stage 1 is built using the stage 0 compiler, using GHC's source code.
--   The result is a compiler that was built by the bootstrapping compiler,
--   with all the features of the new compiler.
--
-- * Stage 2 is built using the stage 1 compiler and GHC's source code. The
--   result is a compiler "built by itself", commonly referred to as
--   /bootstrapping/.
--
-- * Stage 3 uses stage 2 to build from source again. The result should have
--   the same object code as stage 2, which is a good test for the compiler.
--   Since it serves no other purpose than that, the stage 3 build is usually
--   omitted in the build process.
data Stage = Stage0 | Stage1 | Stage2 | Stage3
           deriving (Show, Eq, Ord, Enum, Generic, Bounded)

-- | Prettyprint a 'Stage'.
stageString :: Stage -> String
stageString stage = "stage" ++ show (fromEnum stage)

-- Instances for storing in the Shake database
instance Binary Stage
instance Hashable Stage
instance NFData Stage
