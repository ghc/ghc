module Rules.Clean (clean, cleanRules) where

import Base

clean :: Action ()
clean = do
    putBuild "| Removing Hadrian files..."
    cleanSourceTree
    path <- buildRoot
    removeDirectory path
    putSuccess "| Done. "

cleanSourceTree :: Action ()
cleanSourceTree = do
    path <- buildRoot
    forM_ [Stage0 ..] $ removeDirectory . (path -/-) . stageString
    removeDirectory "sdistprep"
    cleanFsUtils

-- Clean all temporary fs files copied by configure into the source folder
cleanFsUtils :: Action ()
cleanFsUtils = do
    let dirs = [ "utils/lndir/"
               , "utils/unlit/"
               , "rts/"
               , "libraries/base/include/"
               , "libraries/base/cbits/"
               ]
    liftIO $ forM_ dirs (flip removeFiles ["fs.*"])

cleanRules :: Rules ()
cleanRules = "clean" ~> clean
