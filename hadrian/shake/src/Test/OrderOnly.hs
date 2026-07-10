
module Test.OrderOnly(main) where

import Development.Shake
import Test.Type
import System.Directory(removeFile)
import Control.Exception.Extra


main = testBuild test $ do
    "bar.txt" %> \out -> do
        alwaysRerun
        writeFile' out =<< liftIO (readFile "bar.in")

    "foo.txt" %> \out -> do
        let src = "bar.txt"
        orderOnly [src]
        writeFile' out =<< liftIO (readFile src)
        need [src]

    "baz.txt" %> \out -> do
        let src = "bar.txt"
        orderOnly [src]
        liftIO $ appendFile out "x"

    "primary.txt" %> \out -> do
        need ["source.txt"]
        orderOnly ["intermediate.txt"]
        writeFile' out =<< liftIO (readFile "intermediate.txt")

    "intermediate.txt" %> \out ->
        copyFile' "source.txt" out


test build = do
    writeFile "bar.in" "in"
    build ["foo.txt","--sleep"]
    assertContents "foo.txt" "in"
    writeFile "bar.in" "out"
    build ["foo.txt","--sleep"]
    assertContents "foo.txt" "out"

    writeFile "baz.txt" ""
    writeFile "bar.in" "in"
    build ["baz.txt","--sleep"]
    assertContents "baz.txt" "x"
    writeFile "bar.in" "out"
    build ["baz.txt"]
    assertContents "baz.txt" "x"

    ignore $ removeFile "intermediate.txt"
    writeFile "source.txt" "x"
    build ["primary.txt","--sleep"]
    assertContents "intermediate.txt" "x"
    removeFile "intermediate.txt"
    build ["primary.txt","--sleep"]
    assertMissing "intermediate.txt"
    writeFile "source.txt" "y"
    build ["primary.txt","--sleep"]
    assertContents "intermediate.txt" "y"
