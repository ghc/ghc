
module Test.Ninja(main) where

import Development.Shake
import qualified Development.Shake.Config as Config
import System.Directory(copyFile, removeFile)
import Control.Monad
import General.GetOpt
import General.Extra
import Test.Type
import qualified Data.HashMap.Strict as Map
import Data.List.Extra
import System.IO.Extra
import qualified Run
import System.Environment


opts = Option "" ["arg"] (ReqArg Right "") ""

-- | Set to True to test with real Ninja
--   On Windows doesn't work because echo foo > 1 isn't supported
real_ninja = False

main = testBuildArgs test [opts] $ \opts -> do
    let real = "real" `elem` opts
    action $ if real || real_ninja
        then cmd "ninja" opts
        else liftIO $ withArgs ("--lint":"--report=report.html":opts) Run.main


test build = do
    let runEx ninja shake = build $ "--exception" : "--no-report" : map ("--arg=" ++) (words ninja) ++ words shake
    let run ninja = runEx ninja []
    let runFail ninja bad = assertException [bad] $ runEx ninja "--quiet"

    build ["clean"]
    run "-f../../src/Test/Ninja/test1.ninja"
    assertExists "out1.txt"

    run "-f../../src/Test/Ninja/test2.ninja"
    assertExists "out2.2"
    assertMissing "out2.1"
    build ["clean"]
    run "-f../../src/Test/Ninja/test2.ninja out2.1"
    assertExists "out2.1"
    assertMissing "out2.2"

    copyFile "../../src/Test/Ninja/test3-sub.ninja" "test3-sub.ninja"
    copyFile "../../src/Test/Ninja/test3-inc.ninja" "test3-inc.ninja"
    createDirectoryRecursive "subdir"
    copyFile "../../src/Test/Ninja/subdir/1.ninja" "subdir/1.ninja"
    copyFile "../../src/Test/Ninja/subdir/2.ninja" "subdir/2.ninja"
    run "-f../../src/Test/Ninja/test3.ninja"
    assertContentsWords "out3.1" "g4+b1+++i1"
    assertContentsWords "out3.2" "g4++++i1"
    assertContentsWords "out3.3" "g4++++i1"
    assertContentsWords "out3.4" "g4+++s1+s2"

    run "-f../../src/Test/Ninja/test4.ninja out"
    assertExists "out.txt"
    assertExists "out2.txt"

    run "-f../../src/Test/Ninja/test5.ninja"
    assertExists "output file"

    -- #565, check multi-file rules that don't create their contents
    run "-f../../src/Test/Ninja/test7.ninja"

    writeFile "nocreate.log" ""
    writeFile "nocreate.in" ""
    run "-f../../src/Test/Ninja/nocreate.ninja"
    assertContentsWords "nocreate.log" "x"
    run "-f../../src/Test/Ninja/nocreate.ninja"
    run "-f../../src/Test/Ninja/nocreate.ninja"
    assertContentsWords "nocreate.log" "x x x"

    writeFile "input" ""
    runFail "-f../../src/Test/Ninja/lint.ninja bad --lint" "'needed' file required rebuilding"
    run "-f../../src/Test/Ninja/lint.ninja good --lint"
    runFail "-f../../src/Test/Ninja/lint.ninja bad --lint" "not a pre-dependency"

    res <- fmap (drop1 . lines . fst) $ captureOutput $ runEx "-f../../src/Test/Ninja/compdb.ninja -t compdb cxx" "--quiet"
    want <- lines <$> readFile "../../src/Test/Ninja/compdb.output"
    let eq a b | (a1,'*':a2) <- break (== '*') a = unless (a1 `isPrefixOf` b && a2 `isSuffixOf` b) $ a === b
               | otherwise = a === b
    length want === length res
    zipWithM_ eq want res

    -- Test initial variable bindings and variables in include/subninja statements
    let test6 = "test6"

    copyFile "../../src/Test/Ninja/test6-sub.ninja" $ test6 ++ "-sub.ninja"
    copyFile "../../src/Test/Ninja/test6-inc.ninja" $ test6 ++ "-inc.ninja"
    copyFile "../../src/Test/Ninja/test6.ninja" $ test6 ++ ".ninja"

    config <- Config.readConfigFileWithEnv [("v1", test6)] $ test6 ++ ".ninja"
    -- The file included by subninja should have a separate variable scope
    Map.lookup "v2" config === Just "g2"

    run "-f../../src/Test/Ninja/phonyorder.ninja bar.txt"

    -- tests from ninjasmith: https://github.com/ndmitchell/ninjasmith/
    run "-f../../src/Test/Ninja/redefine.ninja"
    assertContentsWords "redefine.txt" "version3 version2"

    run "-f../../src/Test/Ninja/buildseparate.ninja"
    assertContentsWords "buildseparate.txt" "XX"

    run "-f../../src/Test/Ninja/lexical.ninja"
    assertContentsWords "lexical.txt" "XFoo_BarXXFooX.bar"

    run "-f../../src/Test/Ninja/continuations.ninja"
    assertExists "continuations.txt"

    copyFile "../../src/Test/Ninja/restart.ninja" "restart.ninja"
    runEx "-frestart.ninja" "--sleep"
    assertExists "restart.txt"

    createDirectoryRecursive "directory1"
    createDirectoryRecursive "directory2"
    run "-f../../src/Test/Ninja/allow_directory.ninja"

    when False $ do
        -- currently fails because Shake doesn't match Ninja here
        run "-f../../src/Test/Ninja/outputtouch.ninja"
        assertContentsWords "outputtouch.txt" "hello"
        writeFile "outputtouch.txt" "goodbye"
        run "-f../../src/Test/Ninja/outputtouch.ninja"
        assertContentsWords "outputtouch.txt" "goodbye"
        removeFile "outputtouch.txt"
        run "-f../../src/Test/Ninja/outputtouch.ninja"
        assertContentsWords "outputtouch.txt" "hello"
