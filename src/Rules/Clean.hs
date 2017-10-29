module Rules.Clean (clean, cleanSourceTree, cleanRules) where

import Base

clean :: Action ()
clean = do
    cleanSourceTree
    putBuild "| Remove Hadrian files..."
    path <- buildRoot
    removeDirectory $ path -/- generatedDir
    removeFilesAfter path ["//*"]
    putSuccess "| Done. "

cleanSourceTree :: Action ()
cleanSourceTree = do
    path <- buildRoot
    forM_ [Stage0 ..] $ removeDirectory . (path -/-) . stageString
    removeDirectory inplaceBinPath
    removeDirectory inplaceLibPath
    removeDirectory "sdistprep"

cleanRules :: Rules ()
cleanRules = "clean" ~> clean
