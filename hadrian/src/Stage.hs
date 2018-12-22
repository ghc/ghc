module Stage (Stage (..), stageString) where

import Development.Shake.Classes
import GHC.Generics

-- | A stage refers to a certain compiler in GHC's build process.
--
-- * Stage 0 is built with the bootstrapping compiler, i.e. the one already
--   installed on the user's system. The compiler that is produced during
--   stage 0 is called /stage 1 compiler/.
--
-- * Stage 1 is built using the stage 1 compiler and all GHC sources. The result
--   is called /stage 2 compiler/ and it has all features of the new GHC.
--
-- * Stage 2 is built using the stage 2 compiler. The result is a compiler
--   fully "built by itself", commonly referred to as /bootstrapping/.
--
-- * Stage 3 is built as a self test. The resulting compiler should have
--   the same object code as the one built in stage 2, which is a good test
--   for the compiler. Since it serves no other purpose than that, the stage 3
--   build is usually omitted in the build process.
data Stage = Stage0 | Stage1 | Stage2 | Stage3
    deriving (Show, Eq, Ord, Enum, Generic, Bounded)

instance Binary   Stage
instance Hashable Stage
instance NFData   Stage

-- | Prettyprint a 'Stage'.
stageString :: Stage -> String
stageString stage = "stage" ++ show (fromEnum stage)
