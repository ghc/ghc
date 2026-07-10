
module Test.Basic(main) where

import Development.Shake
import System.FilePath
import Test.Type
import System.Directory as IO
import Data.List
import Control.Monad
import General.Extra


main = testBuild test $ do
    "AB.txt" %> \out -> do
        need ["A.txt", "B.txt"]
        text1 <- readFile' "A.txt"
        text2 <- readFile' "B.txt"
        writeFile' out $ text1 ++ text2

    "twice.txt" %> \out -> do
        let src = "once.txt"
        need [src, src]
        copyFile' src out

    "once.txt" %> \out -> do
        src <- readFile' "zero.txt"
        writeFile' out src

    phonys $ \x -> if x /= "halfclean" then Nothing else Just $
        removeFilesAfter "dir" ["//*e.txt"]

    phony "cleaner" $
        removeFilesAfter "dir" ["//*"]

    phony "cleandb" $
        removeFilesAfter "." [".shake.database"]

    phony "configure" $
        liftIO $ appendFile "configure" "1"

    phony "install" $ do
        need ["configure","once.txt"]
        liftIO $ appendFile "install" "1"

    phony "duplicate1" $ need ["duplicate2","duplicate3"]
    phony "duplicate2" $ need ["duplicate3"]
    phony "duplicate3" $ liftIO $ appendFile "duplicate" "1"

    phony "dummy" $
        liftIO $ appendFile "dummy" "1"

    phony "threads" $ do
        x <- getShakeOptions
        writeFile' "threads.txt" $ show $ shakeThreads x

    phony ("slash" </> "platform") $ pure ()
    phony "slash/forward" $ pure ()

    phony "options" $ do
        opts <- getShakeOptions
        putInfo $ show opts

    "dummer.txt" %> \out -> do
        need ["dummy","dummy"]
        need ["dummy"]
        liftIO $ appendFile out "1"

    r <- newResource ".log file" 1
    let trace x = withResource r 1 $ liftIO $ appendFile ".log" x
    "*.par" %> \out -> do
        trace "["
        (if "unsafe" `isInfixOf` out then unsafeExtraThread else id) $ liftIO $ sleep 0.1
        trace "]"
        writeFile' out out

    "sep" </> "1.txt" %> \out -> writeFile' out ""
    "sep/2.txt" %> \out -> writeFile' out ""
    ["sep" </> "3.txt", "sep" </> "4.txt", "sep" </> "5.*", "sep/6.txt"] |%> \out -> writeFile' out ""
    ["sep" </> "7.txt"] |%> \out -> writeFile' out ""

    "ids/source" %> \_ -> pure ()
    "ids/out" %> \out -> do need =<< readFileLines "ids/source"; writeFile' out ""
    "ids/*" %> \out -> do alwaysRerun; trace (takeFileName out); writeFile' out $ takeFileName out

    "rerun" %> \out -> do alwaysRerun; liftIO $ appendFile out "."

    phony "foo" $
        liftIO $ createDirectoryRecursive "foo"

    phony "ordering2" $
        liftIO $ appendFile "order.log" "X"
    phony "ordering" $ do
        liftIO $ appendFile "order.log" "Y"
        need ["ordering2"]

test build = do
    build ["clean"]
    writeFile "A.txt" "AAA"
    writeFile "B.txt" "BBB"
    build ["AB.txt","--sleep"]
    assertContents "AB.txt" "AAABBB"
    appendFile "A.txt" "aaa"
    build ["AB.txt"]
    assertContents "AB.txt" "AAAaaaBBB"
    removeFile "AB.txt"
    build ["AB.txt"]
    assertContents "AB.txt" "AAAaaaBBB"

    writeFile "zero.txt" "xxx"
    build ["twice.txt","--sleep"]
    assertContents "twice.txt" "xxx"
    writeFile "zero.txt" "yyy"
    build ["once.txt","--sleep"]
    assertContents "twice.txt" "xxx"
    assertContents "once.txt" "yyy"
    writeFile "zero.txt" "zzz"
    build ["once.txt","twice.txt","--sleep"]
    assertContents "twice.txt" "zzz"
    assertContents "once.txt" "zzz"

    removeFile "twice.txt"
    build ["twice.txt"]
    assertContents "twice.txt" "zzz"

    show shakeOptions === show shakeOptions
    build ["options"]

    createDirectoryRecursive "dir"
    writeFile "dir/ae.txt" ""
    writeFile "dir/ea.txt" ""
    build ["halfclean"]
    assertBoolIO (IO.doesDirectoryExist "dir") "Directory should exist, cleaner should not have removed it"

    build ["cleaner"]
    sleep 1 -- sometimes takes a while for the file system to notice
    assertBoolIO (not <$> IO.doesDirectoryExist "dir") "Directory should not exist, cleaner should have removed it"

    assertBoolIO (IO.doesFileExist ".shake.database") "Precondition not met"
    build ["cleandb"]
    assertBoolIO (not <$> IO.doesFileExist ".shake.database") "Postcondition not met"

    writeFile "zero.txt" ""
    writeFile "configure" ""
    writeFile "install" ""
    build ["configure"]
    build ["install"]
    build ["install"]
    assertContents "configure" "111"
    assertContents "install" "11"

    build ["dummy"]
    assertContents "dummy" "1"
    build ["dummy"]
    assertContents "dummy" "11"
    build ["dummy","dummy"]
    assertContents "dummy" "111"

    writeFile "dummer.txt" ""
    build ["dummer.txt"]
    assertContents "dummer.txt" "1"
    build ["dummer.txt"]
    assertContents "dummer.txt" "11"

    build ["1.par","2.par","-j1"]
    assertContents ".log" "[][]"
    writeFile ".log" ""
    build ["3.par","4.par","-j2"]
    assertContents ".log" "[[]]"
    writeFile ".log" ""
    processors <- getProcessorCount
    putStrLn $ "getProcessorCount returned " ++ show processors
    when (processors > 1) $ do
        build ["5.par","6.par","-j0"]
        assertContents ".log" "[[]]"

    writeFile ".log" ""
    build ["unsafe1.par","unsafe2.par","-j2"]
    assertContents ".log" "[[]]"

    build ["threads","-j3"]
    assertContents "threads.txt" "3"
    build ["threads","-j0"]
    assertContents "threads.txt" (show processors)

    writeFile "duplicate" ""
    build ["duplicate1","duplicate3"]
    assertContents "duplicate" "1"

    build $ concat [["sep/" ++ show i ++ ".txt", "sep" </> show i ++ ".txt"] | i <- [1..7]]

    build ["slash" </> "platform","slash" </> "forward"]
    build ["slash/platform","slash/forward"]

    createDirectoryRecursive "ids"
    writeFile "ids/source" "ids/a"
    build ["ids/out","--sleep"]
    writeFile ".log" ""
    writeFile "ids/source" "ids/b"
    build ["ids/out","-j4"]
    -- if you collapse depends to [Id] then this ends up asking for the stale 'a'
    assertContents ".log" "b"

    writeFile "rerun" ""
    build ["rerun"]
    assertContents "rerun" "."
    build ["rerun","rerun"]
    assertContents "rerun" ".."

    build ["foo"]
    build ["foo"]

    build [] -- should say "no want/action statements, nothing to do" (checked manually)

    -- #523, #524 - phony children should not run first
    writeFile "order.log" ""
    build ["ordering"]
    assertContents "order.log" "YX"
    build ["ordering"]
    assertContents "order.log" "YXYX"
