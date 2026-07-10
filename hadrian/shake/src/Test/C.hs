
module Test.C(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type

main = testBuild defaultTest $ do
    let src = shakeRoot </> "src/Test/C"
    want ["Main.exe"]

    "Main.exe" %> \out -> do
        cs <- getDirectoryFiles src ["*.c"]
        let os = map (<.> "o") cs
        need os
        cmd "gcc -o" [out] os

    "*.c.o" %> \out -> do
        let c = src </> takeBaseName out
        need [c]
        headers <- cIncludes c
        need $ map ((</>) src . takeFileName) headers
        cmd "gcc -o" [out] "-c" [c]

cIncludes :: FilePath -> Action [FilePath]
cIncludes x = do
    Stdout stdout <- cmd "gcc" ["-MM",x]
    pure $ drop 2 $ words stdout
