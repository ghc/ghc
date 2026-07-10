{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Test.Lint(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import General.Extra
import Test.Type
import Control.Exception
import System.Directory as IO
import System.Info.Extra
import Control.Monad.Extra

newtype Zero = Zero () deriving (Eq, Show, NFData, Typeable, Hashable, Binary)

type instance RuleResult Zero = Zero

main = testBuild test $ do
    addOracle $ \Zero{} -> do
        liftIO $ createDirectoryRecursive "dir"
        liftIO $ setCurrentDirectory "dir"
        pure $ Zero ()

    "changedir" %> \out -> do
        Zero () <- askOracle $ Zero ()
        writeFile' out ""

    "pause.*" %> \out -> do
        liftIO $ sleep 0.2
        need ["cdir" <.> takeExtension out]
        writeFile' out ""

    "cdir.*" %> \out -> do
        pwd <- liftIO getCurrentDirectory
        let dir2 = "dir" ++ takeExtension out
        liftIO $ createDirectoryRecursive dir2
        liftIO $ setCurrentDirectory dir2
        liftIO $ sleep 0.4
        liftIO $ setCurrentDirectory pwd
        writeFile' out ""

    "createonce" %> \out ->
        writeFile' out "X"

    "createtwice" %> \out -> do
        need ["createonce"]
        liftIO sleepFileTime
        writeFile' "createonce" "Y"
        writeFile' out ""

    "recordtwice" %> \out -> do
        alwaysRerun
        trackWrite ["recordtwice_"]
        trackWrite ["recordtwice_"]
        writeFile' "recordtwice_" ""
        writeFile' out ""

    "listing" %> \out -> do
        writeFile' (out <.> "ls1") ""
        getDirectoryFiles "" ["//*.ls*"]
        writeFile' (out <.> "ls2") ""
        writeFile' out ""

    "existance" %> \out -> do
        Development.Shake.doesFileExist "exists"
        writeFile' "exists" ""
        writeFile' out ""

    "gen*" %> \out ->
        writeFile' out out

    "needed1" %> \out -> do
        needed ["gen1"]
        writeFile' out ""

    "needed2" %> \out -> do
        orderOnly ["gen2"]
        needed ["gen2"]
        writeFile' out ""

    "tracker-write1" %> \out -> do
        gen "x" $ out <.> "txt"
        need [out <.> "txt"]
        writeFile' out ""

    "tracker-write2" %> \out -> do
        gen "x" $ out <.> "txt"
        writeFile' out ""

    "tracker-source2" %> \out -> copyFile' "tracker-source1" out
    "tracker-read1" %> \out -> do
        access "tracker-source1"
        writeFile' out ""
    "tracker-read2" %> \out -> do
        access "tracker-source1"
        need ["tracker-source1"]
        writeFile' out ""
    "tracker-read3" %> \out -> do
        access "tracker-source2"
        need ["tracker-source2"]
        writeFile' out ""

    "tracker-compile.o" %> \out -> do
        need ["tracker-source.c", "tracker-source.h"]
        cmd "gcc" ["-c", "tracker-source.c", "-o", out]

    "tracker-compile-auto.o" %> \out -> do
        need ["tracker-source.c"]
        cmd AutoDeps "gcc" ["-c", "tracker-source.c", "-o", out]

    "../lint2/tracker-relative" %> \out ->
        writeFile' out "tracker-relative"
    "tracker-relative1" %> \out -> do
        need ["../lint2/tracker-relative"]
        access "../lint2/tracker-relative"
        writeFile' out "tracker-relative"
    "tracker-relative2" %> \out -> do
        access "../lint2/tracker-relative"
        writeFile' out "tracker-relative"

    where gen t f = cmd Shell "echo" t ">" (toNative f) :: Action ()
          access f = if isWindows
                     then cmd_ Shell "type" (toNative f) "> nul"
                     else cmd_ Shell "cat" f "> /dev/null"


test build = do
    dir <- getCurrentDirectory
    let crash args parts =
            assertException parts (build $ "--quiet" : args)
                `finally` setCurrentDirectory dir

    crash ["changedir"] ["current directory has changed"]
    build ["cdir.1","cdir.2","-j1"]
    build ["--clean","cdir.1","pause.2","-j1"]
    crash ["--clean","cdir.1","pause.2","-j2"] ["output","lint","current directory has changed"]
    crash ["existance"] ["changed since being depended upon"]
    crash ["createtwice"] ["changed since being depended upon"]
    build ["recordtwice"]
    crash ["listing"] ["changed since being depended upon","listing.ls2"]
    crash ["--clean","listing","existance"] ["changed since being depended upon"]
    crash ["needed1"] ["'needed' file required rebuilding"]
    build ["needed2"]
    whenM hasTracker $ do
        writeFile "tracker-source1" ""
        writeFile "tracker-source2" ""
        writeFile "tracker-source.c" "#include <stdio.h>\n#include \"tracker-source.h\"\n"
        writeFile "tracker-source.h" ""
        crash ["tracker-write1"] ["not have its creation tracked","tracker-write1","tracker-write1.txt"]
        build ["tracker-write2"]
        crash ["tracker-read1"] ["used but not depended upon","tracker-source1"]
        build ["tracker-read2"]
        crash ["tracker-read3"] ["depended upon after being used","tracker-source2"]
        build ["tracker-compile.o"]
        build ["tracker-compile-auto.o"]
        build ["tracker-relative1"]
        crash ["tracker-relative2"] ["lint2/tracker-relative"]
