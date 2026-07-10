
module Test.Parallel(main) where

import Development.Shake
import Test.Type
import Data.Foldable
import Data.Tuple.Extra
import Control.Monad
import Control.Concurrent.Extra
import Data.IORef


main = testBuild test $ do
    "AB.txt" %> \out -> do
        -- need [obj "A.txt", obj "B.txt"]
        (text1,text2) <- readFile' "A.txt" `par` readFile' "B.txt"
        writeFile' out $ text1 ++ text2


    sem <- liftIO $ newQSemN 0
    "papplicative_*" %> \out -> do
        -- wait for both to do the initial start before continuing
        liftIO $ assertWithin 1 $ do
            signalQSemN sem 1
            waitQSemN sem 3
            signalQSemN sem 3
        writeFile' out ""

    phony "papplicative" $ do
        need ["papplicative_1"]
        need ["papplicative_2"]
        let ensureReturn = pure ()
        ensureReturn -- should work even though we have a pure
        need ["papplicative_3"]

    "pseparate_*" %> \out -> do
        liftIO $ appendFile "pseparate.log" "["
        liftIO $ sleep 0.1
        liftIO $ appendFile "pseparate.log" "]"
        writeFile' out ""

    phony "pseparate" $ do
        need ["pseparate_1"]
        liftIO $ pure ()
        need ["pseparate_2"]

    sem <- liftIO $ newQSemN 0
    "ptraverse_*" %> \out -> do
        -- wait for all to do the initial start before continuing
        liftIO $ assertWithin 1 $ do
            signalQSemN sem 1
            waitQSemN sem 8
            signalQSemN sem 8
        writeFile' out ""

    phony "ptraverse" $
        traverse_ (need . pure) ["ptraverse_" ++ show i | i <- [1..8]]

    phony "cancel" $ do
        writeFile' "cancel" ""
        done <- liftIO $ newIORef 0
        lock <- liftIO newLock
        void $ parallel $ replicate 5 $ liftIO $ do
            x <- atomicModifyIORef done $ dupe . succ
            when (x == 3) $ do sleep 0.1; fail "boom"
            withLock lock $ appendFile "cancel" "x"

    phony "parallel" $ do
        active <- liftIO $ newIORef 0
        peak <- liftIO $ newIORef 0
        void $ parallel $ replicate 8 $ liftIO $ do
            now <- atomicModifyIORef active $ dupe . succ
            atomicModifyIORef peak $ dupe . max now
            sleep 0.1
            atomicModifyIORef active $ dupe . pred
        peak <- liftIO $ readIORef peak
        writeFile' "parallel" $ show peak

    "parallels" %> \out -> do
        xs <- parallel $ replicate 5 $ parallel $ map pure [1..5]
        writeFile' out $ show xs

    phony "timings" $
        void $ parallel $ map (liftIO . sleep) [1, 2, 0, 1]


test build = do
    build ["clean"]
    writeFile "A.txt" "AAA"
    writeFile "B.txt" "BBB"
    build ["AB.txt","--sleep"]
    assertContents "AB.txt" "AAABBB"
    appendFile "A.txt" "aaa"
    build ["AB.txt"]
    assertContents "AB.txt" "AAAaaaBBB"

    assertException ["boom"] $ build ["cancel","-j1","--quiet"]
    assertContents "cancel" "xx"
    build ["parallel","-j1"]
    assertContents "parallel" "1"
    build ["parallel","-j5"]
    assertContents "parallel" "5"

    build ["parallels"]
    assertContents "parallels" $ show $ replicate 5 [1..5]

    writeFile "pseparate.log" ""
    build ["pseparate","-j2"]
    assertContents "pseparate.log" "[][]"
    build ["papplicative","-j3"]
    build ["ptraverse","-j8"]

    build ["timings","-j6"]
    assertTimings build [("timings",4)]
    build ["timings","-j1"]
    assertTimings build [("timings",4)]
