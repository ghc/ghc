module Hadrian.Target (Target, target, context, builder, inputs, outputs) where

import Development.Shake.Classes
import GHC.Generics

-- | Each invocation of a builder is fully described by a 'Target', which
-- comprises a build context (type variable @c@), a builder (type variable @b@),
-- a list of input files and a list of output files. For example:
--
-- @
-- preludeTarget = Target (GHC.Context) (GHC.Builder)
--     { context = Context Stage1 base profiling
--     , builder = Ghc Stage1
--     , inputs = ["libraries/base/Prelude.hs"]
--     , outputs = ["build/stage1/libraries/base/Prelude.p_o"] }
-- @
data Target c b = Target
    { context :: c          -- ^ Current build context
    , builder :: b          -- ^ Builder to be invoked
    , inputs  :: [FilePath] -- ^ Input files for the builder
    , outputs :: [FilePath] -- ^ Files to be produced
    } deriving (Eq, Generic, Show)

target :: c -> b -> [FilePath] -> [FilePath] -> Target c b
target = Target

instance (Binary   c, Binary   b) => Binary   (Target c b)
instance (Hashable c, Hashable b) => Hashable (Target c b)
instance (NFData   c, NFData   b) => NFData   (Target c b)
