module UnitTests.Distribution.Client.FileMonitor (tests) where

import Control.Monad
import Control.Exception
import Control.Concurrent (threadDelay)
import qualified Data.Set as Set
import System.FilePath
import qualified System.Directory as IO
import Prelude hiding (writeFile)
import qualified Prelude as IO (writeFile)

import Distribution.Text (simpleParse)
import Distribution.Compat.Binary
import Distribution.Simple.Utils (withTempDirectory)
import Distribution.Verbosity (silent)

import Distribution.Client.FileMonitor
import Distribution.Compat.Time

import Test.Tasty
import Test.Tasty.HUnit


tests :: Int -> [TestTree]
tests mtimeChange =
  [ testCase "sanity check mtimes"   $ testFileMTimeSanity mtimeChange
  , testCase "sanity check dirs"     $ testDirChangeSanity mtimeChange
  , testCase "no monitor cache"      testNoMonitorCache
  , testCase "corrupt monitor cache" testCorruptMonitorCache
  , testCase "empty monitor"         testEmptyMonitor
  , testCase "missing file"          testMissingFile
  , testCase "change file"           $ testChangedFile mtimeChange
  , testCase "file mtime vs content" $ testChangedFileMtimeVsContent mtimeChange
  , testCase "update during action"  $ testUpdateDuringAction mtimeChange
  , testCase "remove file"           testRemoveFile
  , testCase "non-existent file"     testNonExistentFile
  , testCase "changed file type"     $ testChangedFileType mtimeChange
  , testCase "several monitor kinds" $ testMultipleMonitorKinds mtimeChange

  , testGroup "glob matches"
    [ testCase "no change"           testGlobNoChange
    , testCase "add match"           $ testGlobAddMatch mtimeChange
    , testCase "remove match"        $ testGlobRemoveMatch mtimeChange
    , testCase "change match"        $ testGlobChangeMatch mtimeChange

    , testCase "add match subdir"    $ testGlobAddMatchSubdir mtimeChange
    , testCase "remove match subdir" $ testGlobRemoveMatchSubdir mtimeChange
    , testCase "change match subdir" $ testGlobChangeMatchSubdir mtimeChange

    , testCase "match toplevel dir"  $ testGlobMatchTopDir mtimeChange
    , testCase "add non-match"       $ testGlobAddNonMatch mtimeChange
    , testCase "remove non-match"    $ testGlobRemoveNonMatch mtimeChange

    , testCase "add non-match"       $ testGlobAddNonMatchSubdir mtimeChange
    , testCase "remove non-match"    $ testGlobRemoveNonMatchSubdir mtimeChange

    , testCase "invariant sorted 1"  $ testInvariantMonitorStateGlobFiles
                                         mtimeChange
    , testCase "invariant sorted 2"  $ testInvariantMonitorStateGlobDirs
                                         mtimeChange

    , testCase "match dirs"          $ testGlobMatchDir mtimeChange
    , testCase "match dirs only"     $ testGlobMatchDirOnly mtimeChange
    , testCase "change file type"    $ testGlobChangeFileType mtimeChange
    , testCase "absolute paths"      $ testGlobAbsolutePath mtimeChange
    ]

  , testCase "value unchanged"       testValueUnchanged
  , testCase "value changed"         testValueChanged
  , testCase "value & file changed"  $ testValueAndFileChanged mtimeChange
  , testCase "value updated"         testValueUpdated
  ]

-- Check the file system behaves the way we expect it to

-- we rely on file mtimes having a reasonable resolution
testFileMTimeSanity :: Int -> Assertion
testFileMTimeSanity mtimeChange =
  withTempDirectory silent "." "file-status-" $ \dir -> do
    replicateM_ 10 $ do
      IO.writeFile (dir </> "a") "content"
      t1 <- getModTime (dir </> "a")
      threadDelay mtimeChange
      IO.writeFile (dir </> "a") "content"
      t2 <- getModTime (dir </> "a")
      assertBool "expected different file mtimes" (t2 > t1)

-- We rely on directories changing mtime when entries are added or removed
testDirChangeSanity :: Int -> Assertion
testDirChangeSanity mtimeChange =
  withTempDirectory silent "." "dir-mtime-" $ \dir -> do

    expectMTimeChange dir "file add" $
      IO.writeFile (dir </> "file") "content"

    expectMTimeSame dir "file content change" $
      IO.writeFile (dir </> "file") "new content"

    expectMTimeChange dir "file del" $
      IO.removeFile (dir </> "file")

    expectMTimeChange dir "subdir add" $
      IO.createDirectory (dir </> "dir")

    expectMTimeSame dir "subdir file add" $
      IO.writeFile (dir </> "dir" </> "file") "content"

    expectMTimeChange dir "subdir file move in" $
      IO.renameFile (dir </> "dir" </> "file") (dir </> "file")

    expectMTimeChange dir "subdir file move out" $
      IO.renameFile (dir </> "file") (dir </> "dir" </> "file")

    expectMTimeSame dir "subdir dir add" $
      IO.createDirectory (dir </> "dir" </> "subdir")

    expectMTimeChange dir "subdir dir move in" $
      IO.renameDirectory (dir </> "dir" </> "subdir") (dir </> "subdir")

    expectMTimeChange dir "subdir dir move out" $
      IO.renameDirectory (dir </> "subdir") (dir </> "dir" </> "subdir")

  where
    expectMTimeChange, expectMTimeSame :: FilePath -> String -> IO ()
                                       -> Assertion

    expectMTimeChange dir descr action = do
      t  <- getModTime dir
      threadDelay mtimeChange
      action
      t' <- getModTime dir
      assertBool ("expected dir mtime change on " ++ descr) (t' > t)

    expectMTimeSame dir descr action = do
      t  <- getModTime dir
      threadDelay mtimeChange
      action
      t' <- getModTime dir
      assertBool ("expected same dir mtime on " ++ descr) (t' == t)


-- Now for the FileMonitor tests proper...

-- first run, where we don't even call updateMonitor
testNoMonitorCache :: Assertion
testNoMonitorCache =
  withFileMonitor $ \root monitor -> do
    reason <- expectMonitorChanged root (monitor :: FileMonitor () ()) ()
    reason @?= MonitorFirstRun

-- write garbage into the binary cache file
testCorruptMonitorCache :: Assertion
testCorruptMonitorCache =
  withFileMonitor $ \root monitor -> do
    IO.writeFile (fileMonitorCacheFile monitor) "broken"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitorCorruptCache

    updateMonitor root monitor [] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= []

    IO.writeFile (fileMonitorCacheFile monitor) "broken"
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitorCorruptCache

-- no files to monitor
testEmptyMonitor :: Assertion
testEmptyMonitor =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"
    updateMonitor root monitor [] () ()
    touchFile root "b"
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= []

-- monitor a file that is expected to exist
testMissingFile :: Assertion
testMissingFile = do
    test monitorFile       touchFile  "a"
    test monitorFileHashed touchFile  "a"
    test monitorFile       touchFile ("dir" </> "a")
    test monitorFileHashed touchFile ("dir" </> "a")
    test monitorDirectory  touchDir   "a"
    test monitorDirectory  touchDir  ("dir" </> "a")
  where
    test :: (FilePath -> MonitorFilePath)
         -> (RootPath -> FilePath -> IO ())
         -> FilePath
         -> IO ()
    test monitorKind touch file =
      withFileMonitor $ \root monitor -> do
        -- a file that doesn't exist at snapshot time is considered to have
        -- changed
        updateMonitor root monitor [monitorKind file] () ()
        reason <- expectMonitorChanged root monitor ()
        reason @?= MonitoredFileChanged file

        -- a file doesn't exist at snapshot time, but gets added afterwards is
        -- also considered to have changed
        updateMonitor root monitor [monitorKind file] () ()
        touch root file
        reason2 <- expectMonitorChanged root monitor ()
        reason2 @?= MonitoredFileChanged file


testChangedFile :: Int -> Assertion
testChangedFile mtimeChange = do
    test monitorFile       touchFile touchFile         "a"
    test monitorFileHashed touchFile touchFileContent  "a"
    test monitorFile       touchFile touchFile        ("dir" </> "a")
    test monitorFileHashed touchFile touchFileContent ("dir" </> "a")
    test monitorDirectory  touchDir  touchDir          "a"
    test monitorDirectory  touchDir  touchDir         ("dir" </> "a")
  where
    test :: (FilePath -> MonitorFilePath)
         -> (RootPath -> FilePath -> IO ())
         -> (RootPath -> FilePath -> IO ())
         -> FilePath
         -> IO ()
    test monitorKind touch touch' file =
      withFileMonitor $ \root monitor -> do
        touch root file
        updateMonitor root monitor [monitorKind file] () ()
        threadDelay mtimeChange
        touch' root file
        reason <- expectMonitorChanged root monitor ()
        reason @?= MonitoredFileChanged file


testChangedFileMtimeVsContent :: Int -> Assertion
testChangedFileMtimeVsContent mtimeChange =
  withFileMonitor $ \root monitor -> do
    -- if we don't touch the file, it's unchanged
    touchFile root "a"
    updateMonitor root monitor [monitorFile "a"] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFile "a"]

    -- if we do touch the file, it's changed if we only consider mtime
    updateMonitor root monitor [monitorFile "a"] () ()
    threadDelay mtimeChange
    touchFile root "a"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged "a"

    -- but if we touch the file, it's unchanged if we consider content hash
    updateMonitor root monitor [monitorFileHashed "a"] () ()
    threadDelay mtimeChange
    touchFile root "a"
    (res2, files2) <- expectMonitorUnchanged root monitor ()
    res2   @?= ()
    files2 @?= [monitorFileHashed "a"]

    -- finally if we change the content it's changed
    updateMonitor root monitor [monitorFileHashed "a"] () ()
    threadDelay mtimeChange
    touchFileContent root "a"
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged "a"


testUpdateDuringAction :: Int -> Assertion
testUpdateDuringAction mtimeChange = do
    test (monitorFile        "a") touchFile "a"
    test (monitorFileHashed  "a") touchFile "a"
    test (monitorDirectory   "a") touchDir  "a"
    test (monitorFileGlobStr "*") touchFile "a"
    test (monitorFileGlobStr "*") { monitorKindDir = DirModTime }
                                  touchDir  "a"
  where
    test :: MonitorFilePath
         -> (RootPath -> FilePath -> IO ())
         -> FilePath
         -> IO ()
    test monitorSpec touch file =
      withFileMonitor $ \root monitor -> do
        touch root file
        updateMonitor root monitor [monitorSpec] () ()

        -- start doing an update action...
        threadDelay mtimeChange -- some time passes
        touch root file         -- a file gets updates during the action
        threadDelay mtimeChange -- some time passes then we finish
        updateMonitor root monitor [monitorSpec] () ()
        -- we don't notice this change since we took the timestamp after the
        -- action finished
        (res, files) <- expectMonitorUnchanged root monitor ()
        res   @?= ()
        files @?= [monitorSpec]

        -- Let's try again, this time taking the timestamp before the action
        timestamp' <- beginUpdateFileMonitor
        threadDelay mtimeChange -- some time passes
        touch root file         -- a file gets updates during the action
        threadDelay mtimeChange -- some time passes then we finish
        updateMonitorWithTimestamp root monitor timestamp' [monitorSpec] () ()
        -- now we do notice the change since we took the snapshot before the
        -- action finished
        reason <- expectMonitorChanged root monitor ()
        reason @?= MonitoredFileChanged file


testRemoveFile :: Assertion
testRemoveFile = do
    test monitorFile       touchFile removeFile  "a"
    test monitorFileHashed touchFile removeFile  "a"
    test monitorFile       touchFile removeFile ("dir" </> "a")
    test monitorFileHashed touchFile removeFile ("dir" </> "a")
    test monitorDirectory  touchDir  removeDir   "a"
    test monitorDirectory  touchDir  removeDir  ("dir" </> "a")
  where
    test :: (FilePath -> MonitorFilePath)
         -> (RootPath -> FilePath -> IO ())
         -> (RootPath -> FilePath -> IO ())
         -> FilePath
         -> IO ()
    test monitorKind touch remove file =
      withFileMonitor $ \root monitor -> do
        touch root file
        updateMonitor root monitor [monitorKind file] () ()
        remove root file
        reason <- expectMonitorChanged root monitor ()
        reason @?= MonitoredFileChanged file


-- monitor a file that we expect not to exist
testNonExistentFile :: Assertion
testNonExistentFile =
  withFileMonitor $ \root monitor -> do
    -- a file that doesn't exist at snapshot time or check time is unchanged
    updateMonitor root monitor [monitorNonExistentFile "a"] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorNonExistentFile "a"]

    -- if the file then exists it has changed
    touchFile root "a"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged "a"

    -- if the file then exists at snapshot and check time it has changed
    updateMonitor root monitor [monitorNonExistentFile "a"] () ()
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged "a"

    -- but if the file existed at snapshot time and doesn't exist at check time
    -- it is consider unchanged. This is unlike files we expect to exist, but
    -- that's because files that exist can have different content and actions
    -- can depend on that content, whereas if the action expected a file not to
    -- exist and it now does not, it'll give the same result, irrespective of
    -- the fact that the file might have existed in the meantime.
    updateMonitor root monitor [monitorNonExistentFile "a"] () ()
    removeFile root "a"
    (res2, files2) <- expectMonitorUnchanged root monitor ()
    res2   @?= ()
    files2 @?= [monitorNonExistentFile "a"]


testChangedFileType :: Int-> Assertion
testChangedFileType mtimeChange = do
    test (monitorFile            "a") touchFile removeFile createDir
    test (monitorFileHashed      "a") touchFile removeFile createDir

    test (monitorDirectory       "a") createDir removeDir touchFile
    test (monitorFileOrDirectory "a") createDir removeDir touchFile

    test (monitorFileGlobStr     "*") { monitorKindDir = DirModTime }
                                      touchFile removeFile createDir
    test (monitorFileGlobStr     "*") { monitorKindDir = DirModTime }
                                      createDir removeDir touchFile
  where
    test :: MonitorFilePath
         -> (RootPath -> String -> IO ())
         -> (RootPath -> String -> IO ())
         -> (RootPath -> String -> IO ())
         -> IO ()
    test monitorKind touch remove touch' =
      withFileMonitor $ \root monitor -> do
        touch  root "a"
        updateMonitor root monitor [monitorKind] () ()
        threadDelay mtimeChange
        remove root "a"
        touch' root "a"
        reason <- expectMonitorChanged root monitor ()
        reason @?= MonitoredFileChanged "a"

-- Monitoring the same file with two different kinds of monitor should work
-- both should be kept, and both checked for changes.
-- We had a bug where only one monitor kind was kept per file.
-- https://github.com/haskell/cabal/pull/3863#issuecomment-248495178
testMultipleMonitorKinds :: Int -> Assertion
testMultipleMonitorKinds mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"
    updateMonitor root monitor [monitorFile "a", monitorFileHashed "a"] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFile "a", monitorFileHashed "a"]
    threadDelay mtimeChange
    touchFile root "a" -- not changing content, just mtime
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged "a"

    createDir root "dir"
    updateMonitor root monitor [monitorDirectory "dir",
                                monitorDirectoryExistence "dir"] () ()
    (res2, files2) <- expectMonitorUnchanged root monitor ()
    res2   @?= ()
    files2 @?= [monitorDirectory "dir", monitorDirectoryExistence "dir"]
    threadDelay mtimeChange
    touchFile root ("dir" </> "a") -- changing dir mtime, not existence
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged "dir"


------------------
-- globs
--

testGlobNoChange :: Assertion
testGlobNoChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    touchFile root ("dir" </> "good-b")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/good-*"]

testGlobAddMatch :: Int -> Assertion
testGlobAddMatch mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/good-*"]
    threadDelay mtimeChange
    touchFile root ("dir" </> "good-b")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "good-b")

testGlobRemoveMatch :: Int -> Assertion
testGlobRemoveMatch mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    touchFile root ("dir" </> "good-b")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    threadDelay mtimeChange
    removeFile root "dir/good-a"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "good-a")

testGlobChangeMatch :: Int -> Assertion
testGlobChangeMatch mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    touchFile root ("dir" </> "good-b")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    threadDelay mtimeChange
    touchFile root ("dir" </> "good-b")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/good-*"]

    touchFileContent root ("dir" </> "good-b")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "good-b")

testGlobAddMatchSubdir :: Int -> Assertion
testGlobAddMatchSubdir mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "good-a")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/good-*"] () ()
    threadDelay mtimeChange
    touchFile root ("dir" </> "b" </> "good-b")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "b" </> "good-b")

testGlobRemoveMatchSubdir :: Int -> Assertion
testGlobRemoveMatchSubdir mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "good-a")
    touchFile root ("dir" </> "b" </> "good-b")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/good-*"] () ()
    threadDelay mtimeChange
    removeDir root ("dir" </> "a")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "a" </> "good-a")

testGlobChangeMatchSubdir :: Int -> Assertion
testGlobChangeMatchSubdir mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "good-a")
    touchFile root ("dir" </> "b" </> "good-b")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/good-*"] () ()
    threadDelay mtimeChange
    touchFile root ("dir" </> "b" </> "good-b")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*/good-*"]

    touchFileContent root "dir/b/good-b"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "b" </> "good-b")

-- check nothing goes squiffy with matching in the top dir
testGlobMatchTopDir :: Int -> Assertion
testGlobMatchTopDir mtimeChange =
  withFileMonitor $ \root monitor -> do
    updateMonitor root monitor [monitorFileGlobStr "*"] () ()
    threadDelay mtimeChange
    touchFile root "a"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged "a"

testGlobAddNonMatch :: Int -> Assertion
testGlobAddNonMatch mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    threadDelay mtimeChange
    touchFile root ("dir" </> "bad")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/good-*"]

testGlobRemoveNonMatch :: Int -> Assertion
testGlobRemoveNonMatch mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "good-a")
    touchFile root ("dir" </> "bad")
    updateMonitor root monitor [monitorFileGlobStr "dir/good-*"] () ()
    threadDelay mtimeChange
    removeFile root "dir/bad"
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/good-*"]

testGlobAddNonMatchSubdir :: Int -> Assertion
testGlobAddNonMatchSubdir mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "good-a")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/good-*"] () ()
    threadDelay mtimeChange
    touchFile root ("dir" </> "b" </> "bad")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*/good-*"]

testGlobRemoveNonMatchSubdir :: Int -> Assertion
testGlobRemoveNonMatchSubdir mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "good-a")
    touchFile root ("dir" </> "b" </> "bad")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/good-*"] () ()
    threadDelay mtimeChange
    removeDir root ("dir" </> "b")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*/good-*"]


-- try and tickle a bug that happens if we don't maintain the invariant that
-- MonitorStateGlobFiles entries are sorted
testInvariantMonitorStateGlobFiles :: Int -> Assertion
testInvariantMonitorStateGlobFiles mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a")
    touchFile root ("dir" </> "b")
    touchFile root ("dir" </> "c")
    touchFile root ("dir" </> "d")
    updateMonitor root monitor [monitorFileGlobStr "dir/*"] () ()
    threadDelay mtimeChange
    -- so there should be no change (since we're doing content checks)
    -- but if we can get the dir entries to appear in the wrong order
    -- then if the sorted invariant is not maintained then we can fool
    -- the 'probeGlobStatus' into thinking there's changes
    removeFile root ("dir" </> "a")
    removeFile root ("dir" </> "b")
    removeFile root ("dir" </> "c")
    removeFile root ("dir" </> "d")
    touchFile root ("dir" </> "d")
    touchFile root ("dir" </> "c")
    touchFile root ("dir" </> "b")
    touchFile root ("dir" </> "a")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*"]

-- same thing for the subdirs case
testInvariantMonitorStateGlobDirs :: Int -> Assertion
testInvariantMonitorStateGlobDirs mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root ("dir" </> "a" </> "file")
    touchFile root ("dir" </> "b" </> "file")
    touchFile root ("dir" </> "c" </> "file")
    touchFile root ("dir" </> "d" </> "file")
    updateMonitor root monitor [monitorFileGlobStr "dir/*/file"] () ()
    threadDelay mtimeChange
    removeDir root ("dir" </> "a")
    removeDir root ("dir" </> "b")
    removeDir root ("dir" </> "c")
    removeDir root ("dir" </> "d")
    touchFile root ("dir" </> "d" </> "file")
    touchFile root ("dir" </> "c" </> "file")
    touchFile root ("dir" </> "b" </> "file")
    touchFile root ("dir" </> "a" </> "file")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*/file"]

-- ensure that a glob can match a directory as well as a file
testGlobMatchDir :: Int -> Assertion
testGlobMatchDir mtimeChange =
  withFileMonitor $ \root monitor -> do
    createDir root ("dir" </> "a")
    updateMonitor root monitor [monitorFileGlobStr "dir/*"] () ()
    threadDelay mtimeChange
    -- nothing changed yet
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*"]
    -- expect dir/b to match and be detected as changed
    createDir root ("dir" </> "b")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "b")
    -- now remove dir/a and expect it to be detected as changed
    updateMonitor root monitor [monitorFileGlobStr "dir/*"] () ()
    threadDelay mtimeChange
    removeDir root ("dir" </> "a")
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged ("dir" </> "a")

testGlobMatchDirOnly :: Int -> Assertion
testGlobMatchDirOnly mtimeChange =
  withFileMonitor $ \root monitor -> do
    updateMonitor root monitor [monitorFileGlobStr "dir/*/"] () ()
    threadDelay mtimeChange
    -- expect file dir/a to not match, so not detected as changed
    touchFile root ("dir" </> "a")
    (res, files) <- expectMonitorUnchanged root monitor ()
    res   @?= ()
    files @?= [monitorFileGlobStr "dir/*/"]
    -- note that checking the file monitor for changes can updates the
    -- cached dir mtimes (when it has to record that there's new matches)
    -- so we need an extra mtime delay
    threadDelay mtimeChange
    -- but expect dir/b to match
    createDir root ("dir" </> "b")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "b")

testGlobChangeFileType :: Int -> Assertion
testGlobChangeFileType mtimeChange =
  withFileMonitor $ \root monitor -> do
    -- change file to dir
    touchFile root ("dir" </> "a")
    updateMonitor root monitor [monitorFileGlobStr "dir/*"] () ()
    threadDelay mtimeChange
    removeFile root ("dir" </> "a")
    createDir  root ("dir" </> "a")
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged ("dir" </> "a")
    -- change dir to file
    updateMonitor root monitor [monitorFileGlobStr "dir/*"] () ()
    threadDelay mtimeChange
    removeDir root ("dir" </> "a")
    touchFile root ("dir" </> "a")
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged ("dir" </> "a")

testGlobAbsolutePath :: Int -> Assertion
testGlobAbsolutePath mtimeChange =
  withFileMonitor $ \root monitor -> do
    root' <- absoluteRoot root
    -- absolute glob, removing a file
    touchFile root ("dir/good-a")
    touchFile root ("dir/good-b")
    updateMonitor root monitor [monitorFileGlobStr (root' </> "dir/good-*")] () ()
    threadDelay mtimeChange
    removeFile root "dir/good-a"
    reason <- expectMonitorChanged root monitor ()
    reason @?= MonitoredFileChanged (root' </> "dir/good-a")
    -- absolute glob, adding a file
    updateMonitor root monitor [monitorFileGlobStr (root' </> "dir/good-*")] () ()
    threadDelay mtimeChange
    touchFile root ("dir/good-a")
    reason2 <- expectMonitorChanged root monitor ()
    reason2 @?= MonitoredFileChanged (root' </> "dir/good-a")
    -- absolute glob, changing a file
    updateMonitor root monitor [monitorFileGlobStr (root' </> "dir/good-*")] () ()
    threadDelay mtimeChange
    touchFileContent root "dir/good-b"
    reason3 <- expectMonitorChanged root monitor ()
    reason3 @?= MonitoredFileChanged (root' </> "dir/good-b")


------------------
-- value changes
--

testValueUnchanged :: Assertion
testValueUnchanged =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"
    updateMonitor root monitor [monitorFile "a"] (42 :: Int) "ok"
    (res, files) <- expectMonitorUnchanged root monitor 42
    res   @?= "ok"
    files @?= [monitorFile "a"]

testValueChanged :: Assertion
testValueChanged =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"
    updateMonitor root monitor [monitorFile "a"] (42 :: Int) "ok"
    reason <- expectMonitorChanged root monitor 43
    reason @?= MonitoredValueChanged 42

testValueAndFileChanged :: Int -> Assertion
testValueAndFileChanged mtimeChange =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"

    -- we change the value and the file, and the value change is reported
    updateMonitor root monitor [monitorFile "a"] (42 :: Int) "ok"
    threadDelay mtimeChange
    touchFile root "a"
    reason <- expectMonitorChanged root monitor 43
    reason @?= MonitoredValueChanged 42

    -- if fileMonitorCheckIfOnlyValueChanged then if only the value changed
    -- then it's reported as MonitoredValueChanged
    let monitor' :: FileMonitor Int String
        monitor' = monitor { fileMonitorCheckIfOnlyValueChanged = True }
    updateMonitor root monitor' [monitorFile "a"] 42 "ok"
    reason2 <- expectMonitorChanged root monitor' 43
    reason2 @?= MonitoredValueChanged 42

    -- but if a file changed too then we don't report MonitoredValueChanged
    updateMonitor root monitor' [monitorFile "a"] 42 "ok"
    threadDelay mtimeChange
    touchFile root "a"
    reason3 <- expectMonitorChanged root monitor' 43
    reason3 @?= MonitoredFileChanged "a"

testValueUpdated :: Assertion
testValueUpdated =
  withFileMonitor $ \root monitor -> do
    touchFile root "a"

    let monitor' :: FileMonitor (Set.Set Int) String
        monitor' = (monitor :: FileMonitor (Set.Set Int) String) {
                     fileMonitorCheckIfOnlyValueChanged = True,
                     fileMonitorKeyValid = Set.isSubsetOf
                   }

    updateMonitor root monitor' [monitorFile "a"] (Set.fromList [42,43]) "ok"
    (res,_files) <- expectMonitorUnchanged root monitor' (Set.fromList [42])
    res @?= "ok"

    reason <- expectMonitorChanged root monitor' (Set.fromList [42,44])
    reason @?= MonitoredValueChanged (Set.fromList [42,43])


-------------
-- Utils

newtype RootPath = RootPath FilePath

touchFile :: RootPath -> FilePath -> IO ()
touchFile (RootPath root) fname = do
  let path = root </> fname
  IO.createDirectoryIfMissing True (takeDirectory path)
  IO.writeFile path "touched"

touchFileContent :: RootPath -> FilePath -> IO ()
touchFileContent (RootPath root) fname = do
  let path = root </> fname
  IO.createDirectoryIfMissing True (takeDirectory path)
  IO.writeFile path "different"

removeFile :: RootPath -> FilePath -> IO ()
removeFile (RootPath root) fname = IO.removeFile (root </> fname)

touchDir :: RootPath -> FilePath -> IO ()
touchDir root@(RootPath rootdir) dname = do
  IO.createDirectoryIfMissing True (rootdir </> dname)
  touchFile  root (dname </> "touch")
  removeFile root (dname </> "touch")

createDir :: RootPath -> FilePath -> IO ()
createDir (RootPath root) dname = do
  let path = root </> dname
  IO.createDirectoryIfMissing True (takeDirectory path)
  IO.createDirectory path

removeDir :: RootPath -> FilePath -> IO ()
removeDir (RootPath root) dname = IO.removeDirectoryRecursive (root </> dname)

absoluteRoot :: RootPath -> IO FilePath
absoluteRoot (RootPath root) = IO.canonicalizePath root

monitorFileGlobStr :: String -> MonitorFilePath
monitorFileGlobStr globstr
  | Just glob <- simpleParse globstr = monitorFileGlob glob
  | otherwise                        = error $ "Failed to parse " ++ globstr


expectMonitorChanged :: (Binary a, Binary b)
                     => RootPath -> FileMonitor a b -> a
                     -> IO (MonitorChangedReason a)
expectMonitorChanged root monitor key = do
  res <- checkChanged root monitor key
  case res of
    MonitorChanged reason -> return reason
    MonitorUnchanged _ _  -> throwIO $ HUnitFailure "expected change"

expectMonitorUnchanged :: (Binary a, Binary b)
                        => RootPath -> FileMonitor a b -> a
                        -> IO (b, [MonitorFilePath])
expectMonitorUnchanged root monitor key = do
  res <- checkChanged root monitor key
  case res of
    MonitorChanged _reason   -> throwIO $ HUnitFailure "expected no change"
    MonitorUnchanged b files -> return (b, files)

checkChanged :: (Binary a, Binary b)
             => RootPath -> FileMonitor a b
             -> a -> IO (MonitorChanged a b)
checkChanged (RootPath root) monitor key =
  checkFileMonitorChanged monitor root key

updateMonitor :: (Binary a, Binary b)
              => RootPath -> FileMonitor a b
              -> [MonitorFilePath] -> a -> b -> IO ()
updateMonitor (RootPath root) monitor files key result =
  updateFileMonitor monitor root Nothing files key result

updateMonitorWithTimestamp :: (Binary a, Binary b)
              => RootPath -> FileMonitor a b -> MonitorTimestamp
              -> [MonitorFilePath] -> a -> b -> IO ()
updateMonitorWithTimestamp (RootPath root) monitor timestamp files key result =
  updateFileMonitor monitor root (Just timestamp) files key result

withFileMonitor :: Eq a => (RootPath -> FileMonitor a b -> IO c) -> IO c
withFileMonitor action = do
  withTempDirectory silent "." "file-status-" $ \root -> do
    let file    = root <.> "monitor"
        monitor = newFileMonitor file
    finally (action (RootPath root) monitor) $ do
      exists <- IO.doesFileExist file
      when exists $ IO.removeFile file
