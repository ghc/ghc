module UnitTests.Distribution.Client.Store (tests) where

--import Control.Monad
--import Control.Concurrent (forkIO, threadDelay)
--import Control.Concurrent.MVar
import qualified Data.Set as Set
import System.FilePath
import System.Directory
--import System.Random

import Distribution.Package (UnitId, mkUnitId)
import Distribution.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Version  (mkVersion)
import Distribution.Verbosity (Verbosity, silent)
import Distribution.Simple.Utils (withTempDirectory)

import Distribution.Client.Store
import Distribution.Client.RebuildMonad

import Test.Tasty
import Test.Tasty.HUnit


tests :: [TestTree]
tests =
  [ testCase "list content empty"  testListEmpty
  , testCase "install serial"      testInstallSerial
--, testCase "install parallel"    testInstallParallel
    --TODO: figure out some way to do a parallel test, see issue below
  ]


testListEmpty :: Assertion
testListEmpty =
  withTempDirectory verbosity "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    assertStoreEntryExists storeDirLayout compid unitid False
    assertStoreContent tmp storeDirLayout compid        Set.empty
  where
    compid = CompilerId GHC (mkVersion [1,0])
    unitid = mkUnitId "foo-1.0-xyz"


testInstallSerial :: Assertion
testInstallSerial =
  withTempDirectory verbosity "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")
        copyFiles file content dir = do
          -- we copy into a prefix inside the tmp dir and return the prefix
          let destprefix = dir </> "prefix"
          createDirectory destprefix
          writeFile (destprefix </> file) content
          return (destprefix,[])

    assertNewStoreEntry tmp storeDirLayout compid unitid1
                        (copyFiles "file1" "content-foo") (return ())
                        UseNewStoreEntry

    assertNewStoreEntry tmp storeDirLayout compid unitid1
                        (copyFiles "file1" "content-foo") (return ())
                        UseExistingStoreEntry

    assertNewStoreEntry tmp storeDirLayout compid unitid2
                        (copyFiles "file2" "content-bar") (return ())
                        UseNewStoreEntry

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout compid
    assertFileEqual (pkgDir unitid1 </> "file1") "content-foo"
    assertFileEqual (pkgDir unitid2 </> "file2") "content-bar"
  where
    compid  = CompilerId GHC (mkVersion [1,0])
    unitid1 = mkUnitId "foo-1.0-xyz"
    unitid2 = mkUnitId "bar-2.0-xyz"


{-
-- unfortunately a parallel test like the one below is thwarted by the normal
-- process-internal file locking. If that locking were not in place then we
-- ought to get the blocking behaviour, but due to the normal Handle locking
-- it just fails instead.

testInstallParallel :: Assertion
testInstallParallel =
  withTempDirectory verbosity "." "store-" $ \tmp -> do
    let storeDirLayout = defaultStoreDirLayout (tmp </> "store")

    sync1 <- newEmptyMVar
    sync2 <- newEmptyMVar
    outv  <- newEmptyMVar
    regv  <- newMVar (0 :: Int)

    sequence_
      [ do forkIO $ do
             let copyFiles dir = do
                   delay <- randomRIO (1,100000)
                   writeFile (dir </> "file") (show n)
                   putMVar  sync1 ()
                   readMVar sync2
                   threadDelay delay
                 register = do
                   modifyMVar_ regv (return . (+1))
                   threadDelay 200000
             o <- newStoreEntry verbosity storeDirLayout
                                compid unitid
                                copyFiles register
             putMVar outv (n, o)
      | n <- [0..9 :: Int] ]

    replicateM_ 10 (takeMVar sync1)
    -- all threads are in the copyFiles action concurrently, release them:
    putMVar  sync2 ()

    outcomes <- replicateM 10 (takeMVar outv)
    regcount <- readMVar regv
    let regcount' = length [ () | (_, UseNewStoreEntry) <- outcomes ]

    assertEqual "num registrations" 1 regcount
    assertEqual "num registrations" 1 regcount'

    assertStoreContent tmp storeDirLayout compid (Set.singleton unitid)

    let pkgDir :: UnitId -> FilePath
        pkgDir = storePackageDirectory storeDirLayout compid
    case [ n | (n, UseNewStoreEntry) <- outcomes ] of
      [n] -> assertFileEqual (pkgDir unitid </> "file") (show n)
      _   -> assertFailure "impossible"

  where
    compid  = CompilerId GHC (mkVersion [1,0])
    unitid = mkUnitId "foo-1.0-xyz"
-}

-------------
-- Utils

assertNewStoreEntry :: FilePath -> StoreDirLayout
                    -> CompilerId -> UnitId
                    -> (FilePath -> IO (FilePath,[FilePath])) -> IO ()
                    -> NewStoreEntryOutcome
                    -> Assertion
assertNewStoreEntry tmp storeDirLayout compid unitid
                    copyFiles register expectedOutcome = do
    entries <- runRebuild tmp $ getStoreEntries storeDirLayout compid
    outcome <- newStoreEntry verbosity storeDirLayout
                             compid unitid
                             copyFiles register
    assertEqual "newStoreEntry outcome" expectedOutcome outcome
    assertStoreEntryExists storeDirLayout compid unitid True
    let expected = Set.insert unitid entries
    assertStoreContent tmp storeDirLayout compid expected


assertStoreEntryExists :: StoreDirLayout
                       -> CompilerId -> UnitId -> Bool
                       -> Assertion
assertStoreEntryExists storeDirLayout compid unitid expected = do
    actual <- doesStoreEntryExist storeDirLayout compid unitid
    assertEqual "store entry exists" expected actual


assertStoreContent :: FilePath -> StoreDirLayout
                   -> CompilerId -> Set.Set UnitId
                   -> Assertion
assertStoreContent tmp storeDirLayout compid expected = do
    actual <- runRebuild tmp $ getStoreEntries storeDirLayout compid
    assertEqual "store content" actual expected


assertFileEqual :: FilePath -> String -> Assertion
assertFileEqual path expected = do
    exists <- doesFileExist path
    assertBool ("file does not exist:\n" ++ path) exists
    actual <- readFile path
    assertEqual ("file content for:\n" ++ path) expected actual


verbosity :: Verbosity
verbosity = silent

