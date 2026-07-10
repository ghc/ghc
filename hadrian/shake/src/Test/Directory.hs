{-# LANGUAGE TupleSections #-}

module Test.Directory(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Data.List
import Data.Function
import Control.Monad
import General.Extra
import System.Directory(createDirectory)
import qualified System.Directory as IO
import qualified System.IO.Extra as IO


-- Use escape characters, _o=* _l=/ __=<space>
readEsc ('_':'o':xs) = '*' : readEsc xs
readEsc ('_':'l':xs) = '/' : readEsc xs
readEsc ('_':'_':xs) = ' ' : readEsc xs
readEsc (x:xs) = x : readEsc xs
readEsc [] = []

showEsc = concatMap f
    where f '*' = "_o"
          f '/' = "_l"
          f ' ' = "__"
          f x = [x]


main = testBuild test $ do
    "*.contents" %> \out ->
        writeFileLines out =<< getDirectoryContents (readEsc $ dropExtension out)
    "*.dirs" %> \out ->
        writeFileLines out =<< getDirectoryDirs (readEsc $ dropExtension out)
    "*.files" %> \out -> do
        let pats = readEsc $ dropExtension out
        let (x:xs) = ["" | " " `isPrefixOf` pats] ++ words pats
        writeFileLines out . map toStandard =<< getDirectoryFiles x xs

    "*.exist" %> \out -> do
        let xs = words $ readEsc $ dropExtension out
        fs <- mapM doesFileExist xs
        ds <- mapM doesDirectoryExist xs
        let bool x = if x then "1" else "0"
        writeFileLines out $ zipWith ((++) `on` bool) fs ds

    "dots" %> \out -> do
        b1 <- liftM2 (==) (getDirectoryContents ".") (getDirectoryContents "")
        b2 <- liftM2 (==) (getDirectoryDirs ".") (getDirectoryDirs "")
        b3 <- liftM2 (==) (getDirectoryFiles "." ["*.txt"]) (getDirectoryFiles "" ["*.txt"])
        b4 <- liftM2 (==) (getDirectoryFiles "." ["C.txt/*.txt"]) (getDirectoryFiles "" ["C.txt/*.txt"])
        b5 <- liftM2 (==) (getDirectoryFiles "." ["//*.txt"]) (getDirectoryFiles "" ["//*.txt"])
        writeFileLines out $ map show [b1,b2,b3,b4,b5]

test build = do
    let demand x ys = let f = showEsc x in do build [f]; assertContents f $ unlines $ words ys
    build ["clean"]
    demand " *.txt.files" ""
    demand " //*.txt.files" ""
    demand ".dirs" ""
    demand "A.txt B.txt C.txt.exist" "00 00 00"

    writeFile "A.txt" ""
    writeFile "B.txt" ""
    createDirectory "C.txt"
    writeFile "C.txt/D.txt" ""
    writeFile "C.txt/E.xtx" ""
    demand " *.txt.files" "A.txt B.txt"
    demand ".dirs" "C.txt"
    demand "A.txt B.txt C.txt.exist" "10 10 01"
    demand " //*.txt.files" "A.txt B.txt C.txt/D.txt"
    demand "C.txt *.txt.files" "D.txt"
    demand " *.txt //*.xtx.files" "A.txt B.txt C.txt/E.xtx"
    demand " C.txt/*.files" "C.txt/D.txt C.txt/E.xtx"

    demand " missing_dir/*.files" ""
    demand " missing_dir/bar/*.files" ""
    demand " //missing_dir/*.files" ""
    assertException ["missing_dir","does not exist"] $ build ["--quiet",showEsc "missing_dir *.files"]

    build ["dots","--no-lint"]
    assertContents "dots" $ unlines $ words "True True True True True"

    let removeTest pat del keep =
            IO.withTempDir $ \dir -> do
                forM_ (del ++ keep) $ \s -> do
                    createDirectoryRecursive $ dir </> takeDirectory s
                    unless (hasTrailingPathSeparator s) $
                        writeFile (dir </> s) ""
                removeFiles dir pat
                createDirectoryRecursive dir
                forM_ (map (False,) del ++ map (True,) keep) $ \(b,s) -> do
                    b2 <- (if hasTrailingPathSeparator s then IO.doesDirectoryExist else IO.doesFileExist) $ dir </> s
                    when (b /= b2) $ do
                        let f b = if b then "present" else "missing"
                        error $ "removeFiles mismatch: with pattern " ++ show pat ++ ", " ++ s ++
                                " should be " ++ f b ++ " but is " ++ f b2

    removeTest ["//bob"] ["test/bob","more/bob"] ["extra/obo"]
    removeTest ["bob"] ["bob/"] ["bar/"]
    removeTest ["*.hs"] ["test.hs"] ["extra/more.hs","new.txt"]
    removeTest ["baz"] ["baz"] ["foo","bar/bob"]
    removeTest ["baz"] ["baz/bob","baz/"] ["foo","bar/bob"]
    removeTest ["Foo//*"] ["Foo/bar","Foo/Quux/bar","Foo/Quux/"] []
    removeTest ["Foo//"] ["Foo/"] ["bar"]
    removeTest ["baz"] [] ["test.hs","bar/","foo/"]
    removeTest ["bob//*"] [] ["test/bob/"]
    removeTest ["//bob"] ["test/bob/"] ["test/"]
    removeTest ["//*.txt"] ["more/a.txt"] ["more/"]
    removeTest ["//*.txt"] ["more/a.txt/"] ["more/"]
    removeTest ["//*.txt"] ["more/a.txt/","more/b.txt"] ["more/"]
    removeTest ["//*.txt"] [] ["more/"]
    removeTest ["a//b"] ["a/c/b"] []
    removeFiles "non-existing-directory" ["*"]
