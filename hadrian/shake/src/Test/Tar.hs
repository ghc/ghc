
module Test.Tar(main) where

import Development.Shake
import System.FilePath
import Test.Type


main = testBuild defaultTest $ do
    want ["result.tar"]
    "result.tar" %> \out -> do
        contents <- fmap (map (shakeRoot </>)) $ readFileLines $ shakeRoot </> "src/Test/Tar/list.txt"
        need contents
        cmd "tar -cf" [out] contents
