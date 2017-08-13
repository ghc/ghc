module Rules.Clean (clean, cleanSourceTree, cleanRules) where

import Base
import Settings.Path
import UserSettings
import Utilities

clean :: Action ()
clean = do
    cleanSourceTree
    putBuild $ "| Remove Hadrian files..."
    removeDirectory generatedPath
    removeFilesAfter buildRootPath ["//*"]
    putSuccess $ "| Done. "

cleanSourceTree :: Action ()
cleanSourceTree = do
    forM_ [Stage0 ..] $ removeDirectory . (buildRootPath -/-) . stageString
    removeDirectory inplaceBinPath
    removeDirectory inplaceLibPath
    removeDirectory "sdistprep"

cleanRules :: Rules ()
cleanRules = "clean" ~> clean
