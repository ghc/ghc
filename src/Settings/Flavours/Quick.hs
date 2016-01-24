module Settings.Flavours.Quick (quickFlavourArgs) where

import Expression
import Predicates (builderGhc)

-- TODO: consider putting all flavours in a single file
-- TODO: handle other, non Args, settings affected by flavours
quickFlavourArgs :: Args
quickFlavourArgs = builderGhc ? arg "-O0"
