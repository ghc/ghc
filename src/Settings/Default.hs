module Settings.Default (defaultSplitObjects) where

import Base
import Expression
import GHC
import Oracles.Config.Flag
import Predicates (notStage0)

defaultSplitObjects :: Predicate
defaultSplitObjects = do
    goodStage <- notStage0 -- We don't split bootstrap (stage 0) packages
    pkg       <- getPackage
    supported <- lift supportsSplitObjects
    let goodPackage = isLibrary pkg && pkg /= compiler && pkg /= rts
    return $ goodStage && goodPackage && supported
