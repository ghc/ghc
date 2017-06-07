module Rules.Clean (cleanRules) where

import Base
import Settings.Path
import Stage
import UserSettings
import Util

cleanRules :: Rules ()
cleanRules = do
    "clean" ~> do
        forM_ [Stage0 ..] $ removeDirectory . (buildRootPath -/-) . stageString
        removeDirectory generatedPath
        removeDirectory inplaceBinPath
        removeDirectory inplaceLibPath
        removeDirectory "sdistprep"
        putBuild $ "| Remove Hadrian files..."
        removeFilesAfter buildRootPath ["//*"]
        putSuccess $ "| Done. "
