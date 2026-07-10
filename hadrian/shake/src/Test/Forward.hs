
module Test.Forward(main) where

import Data.Char
import Data.List.Extra
import Development.Shake
import System.Info.Extra
import Development.Shake.Forward
import Development.Shake.FilePath
import Test.Type
import System.IO.Extra as IO


main = testBuild test $ forwardRule $ do
    cs <- getDirectoryFiles "" ["*.c"]
    os <- forP cs $ \c -> do
        let o = c <.> "o"
        cache $ cmd "gcc -c" [c] "-o" [o]
        pure o
    cache $ cmd "gcc -o" ["Main" <.> exe] os
    cache $ cmd ["." </> "Main" <.> exe] (FileStdout "output.txt")

    -- Doing this way to test cacheAction with arguments
    -- any real code should use a tracked readFile and avoid passing arguments to the closure
    src <- liftIO $ IO.readFile' "output.txt"
    cacheActionWith "reducer" src $ writeFile' "out.txt" $ filter isUpper src


checkVaild act = do
    b <- hasTracker
    if not b then
        putStrLn "Warning: Not running forward test (no tracker)"
     else if isMac then
        putStrLn "Warning: Not running forward test (doesn't work on Mac)"
     else
        act

test build = checkVaild $ do
    -- first clean then copy the source files over
    build ["clean"]
    copyDirectoryChanged (shakeRoot </> "src/Test/C") "."

    -- build and rebuild
    build ["--forward"]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HSU"

    -- check that cacheAction doesn't rerun when it shouldn't
    writeFile "out.txt" "HHH"
    build ["-j2","--forward"]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HHH"

    -- modify the constants
    orig <- IO.readFile' "constants.c"
    writeFile "constants.c" $ replace "Shake" "Rattle" orig
    build ["-j2","--forward"]
    assertContents "output.txt" "Hello Rattle Users!\n"
    assertContents "out.txt" "HRU"

    -- put it back
    writeFile "constants.c" orig
    build ["-j2","--forward"]
    assertContents "output.txt" "Hello Shake Users!\n"
    assertContents "out.txt" "HSU"
