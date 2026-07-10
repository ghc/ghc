
module Test.Live(main) where

import Development.Shake
import Test.Type


main = testBuild test $ do
    "foo" %> \ out -> do
        need ["bar"]
        writeFile' out ""

    "bar" %> \out -> writeFile' out ""
    "baz" %> \out -> writeFile' out ""


test build = do
    build ["clean"]
    build ["foo","baz","--live=live.txt"]
    assertContentsUnordered "live.txt" $ words "foo bar baz"
    build ["foo","baz","--live=live.txt"]
    assertContentsUnordered "live.txt" $ words "foo bar baz"
    build ["foo","--live=live.txt"]
    assertContentsUnordered "live.txt" $ words "foo bar"
    build ["bar","--live=live.txt"]
    assertContentsUnordered "live.txt" $ words "bar"
