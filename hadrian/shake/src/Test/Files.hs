
module Test.Files(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Test.Type
import Control.Monad
import Data.List


main = testBuild test $ do
    want ["even.txt","odd.txt"]

    "A1-plus-B" %> \out -> do
        a1 <- readFileLines "A1"
        b  <- readFileLines "B"
        writeFileLines out $ a1 ++ b

    ["A1", "A2"] &%> \[o1, o2] -> do
        writeFileLines o1 ["This is", "A1"]
        writeFileLines o2 ["This is", "A2"]

    "B" %> \out ->
        writeFileLines out ["This is", "B"]

    ["even.txt","odd.txt"] &?%> \[evens,odds] -> do
        src <- readFileLines "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os

    ["dir1/out.txt","dir2/out.txt"] &?%> \[a,b] -> do
        writeFile' a "a"
        writeFile' b "b"

    ["or1.txt","or2.txt","or*.txt"] |%> \x ->
        writeFile' x x

    (\x -> let dir = takeDirectory x in
           if takeFileName dir /= "pred" then Nothing else Just [dir </> "a.txt",dir </> "b.txt"]) &?> \outs ->
        mapM_ (`writeFile'` "") outs


test build = do
    forM_ [[],["--usepredicate"]] $ \args -> do
        let nums = unlines . map show
        writeFile "numbers.txt" $ nums [1,2,4,5,2,3,1]
        build ("--sleep":args)
        assertContents "even.txt" $ nums [2,4,2]
        assertContents "odd.txt"  $ nums [1,5,3,1]
        build ["clean"]
        build ["--no-build","--report=-"]
        build ["dir1/out.txt"]

    build ["pred/a.txt"]

    -- Test #496
    build ["A1-plus-B"]
    removeFile "A2"
    build ["A1-plus-B"]

    build ["or2.txt","or4.txt"]
    assertContents "or4.txt" "or4.txt"
