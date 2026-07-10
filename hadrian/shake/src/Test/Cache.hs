
module Test.Cache(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Data.Char
import Test.Type


main = testBuild test $ do
    vowels <- newCache $ \file -> do
        src <- readFile' file
        liftIO $ appendFile "trace.txt" "1"
        pure $ length $ filter isDigit src
    "*.out*" %> \x ->
        writeFile' x . show =<< vowels (dropExtension x <.> "txt")

    startCompiler <- newCache $ \() -> do
        liftIO $ writeFile "compiler.txt" "on"
        runAfter $ writeFile "compiler.txt" "off"

    "*.lang" %> \out -> do
        startCompiler ()
        liftIO $ copyFile "compiler.txt" out

    -- Bug fixed in https://github.com/ndmitchell/shake/pull/796
    bug796_2 <- newCache $ \() -> do
        readFile' "bug796.2"
    "bug796" %> \out -> do
        a <- readFile' "bug796.1"
        b <- bug796_2 ()
        writeFile' out $ a ++ b


test build = do
    build ["clean"]
    writeFile "trace.txt" ""
    writeFile "vowels.txt" "abc123a"
    build ["vowels.out1","vowels.out2","-j3","--sleep"]
    assertContents "trace.txt" "1"
    assertContents "vowels.out1" "3"
    assertContents "vowels.out2" "3"

    build ["vowels.out2","-j3"]
    assertContents "trace.txt" "1"
    assertContents "vowels.out1" "3"

    writeFile "vowels.txt" "12xyz34"
    build ["vowels.out2","-j3","--sleep"]
    assertContents "trace.txt" "11"
    assertContents "vowels.out2" "4"

    build ["vowels.out1","-j3","--sleep"]
    assertContents "trace.txt" "111"
    assertContents "vowels.out1" "4"

    build ["foo.lang","bar.lang"]
    assertContents "foo.lang" "on"
    assertContents "compiler.txt" "off"
    writeFile "compiler.txt" "unstarted"
    build ["foo.lang","bar.lang"]
    assertContents "compiler.txt" "unstarted"

    writeFile "bug796.1" "a"
    writeFile "bug796.2" "b"
    build ["bug796", "--sleep"]
    assertContents "bug796" "ab"
    writeFile "bug796.1" "A"
    build ["bug796", "--sleep"]
    assertContents "bug796" "Ab"
    writeFile "bug796.2" "B"
    build ["bug796", "--sleep"]
    assertContents "bug796" "AB"
