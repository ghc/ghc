
module Test(main) where

import Control.Exception.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra
import System.Directory
import System.Environment
import General.Timing
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.FileName
import qualified Data.ByteString.Char8 as BS
import Test.Type(sleepFileTimeCalibrate)
import Control.Concurrent.Extra

import qualified Test.Basic
import qualified Test.Batch
import qualified Test.Benchmark
import qualified Test.Builtin
import qualified Test.BuiltinOverride
import qualified Test.C
import qualified Test.Cache
import qualified Test.Cleanup
import qualified Test.CloseFileHandles
import qualified Test.Command
import qualified Test.Config
import qualified Test.Database
import qualified Test.Digest
import qualified Test.Directory
import qualified Test.Docs
import qualified Test.Errors
import qualified Test.Existence
import qualified Test.FileLock
import qualified Test.FilePath
import qualified Test.FilePattern
import qualified Test.Files
import qualified Test.Forward
import qualified Test.History
import qualified Test.Journal
import qualified Test.Lint
import qualified Test.Live
import qualified Test.Manual
import qualified Test.Match
import qualified Test.Monad
import qualified Test.Ninja
import qualified Test.Oracle
import qualified Test.OrderOnly
import qualified Test.Parallel
import qualified Test.Pool
import qualified Test.Progress
import qualified Test.Random
import qualified Test.Rebuild
import qualified Test.Reschedule
import qualified Test.Resources
import qualified Test.Self
import qualified Test.SelfMake
import qualified Test.Shuffle
import qualified Test.Tar
import qualified Test.Targets
import qualified Test.Thread
import qualified Test.Tup
import qualified Test.Unicode
import qualified Test.Util
import qualified Test.Verbosity
import qualified Test.Version

import qualified Run


fakes = ["clean" * clean, "test" * test, "make" * makefile, "filetime" * filetime]
    where (*) = (,)

mains =
    ["basic" * Test.Basic.main
    ,"batch" * Test.Batch.main
    ,"benchmark" * Test.Benchmark.main
    ,"builtin" * Test.Builtin.main
    ,"builtinOverride" * Test.BuiltinOverride.main
    ,"c" * Test.C.main
    ,"cache" * Test.Cache.main
    ,"cleanup" * Test.Cleanup.main
    ,"closefilehandles" * Test.CloseFileHandles.main
    ,"command" * Test.Command.main
    ,"config" * Test.Config.main
    ,"database" * Test.Database.main
    ,"digest" * Test.Digest.main
    ,"directory" * Test.Directory.main
    ,"docs" * Test.Docs.main
    ,"errors" * Test.Errors.main
    ,"existence" * Test.Existence.main
    ,"filelock" * Test.FileLock.main
    ,"filepath" * Test.FilePath.main
    ,"filepattern" * Test.FilePattern.main
    ,"files" * Test.Files.main
    ,"forward" * Test.Forward.main
    ,"history" * Test.History.main
    ,"journal" * Test.Journal.main
    ,"lint" * Test.Lint.main
    ,"live" * Test.Live.main
    ,"manual" * Test.Manual.main
    ,"match" * Test.Match.main
    ,"monad" * Test.Monad.main
    ,"ninja" * Test.Ninja.main
    ,"oracle" * Test.Oracle.main
    ,"orderonly" * Test.OrderOnly.main
    ,"parallel" * Test.Parallel.main
    ,"pool" * Test.Pool.main
    ,"progress" * Test.Progress.main
    ,"random" * Test.Random.main
    ,"rebuild" * Test.Rebuild.main
    ,"reschedule" * Test.Reschedule.main
    ,"resources" * Test.Resources.main
    ,"self" * Test.Self.main
    ,"selfmake" * Test.SelfMake.main
    ,"shuffle" * Test.Shuffle.main
    ,"tar" * Test.Tar.main
    ,"targets" * Test.Targets.main
    ,"thread" * Test.Thread.main
    ,"tup" * Test.Tup.main
    ,"unicode" * Test.Unicode.main
    ,"util" * Test.Util.main
    ,"verbosity" * Test.Verbosity.main
    ,"version" * Test.Version.main]
    where (*) = (,)


main :: IO ()
main = do
    resetTimings
    xs <- getArgs
    case flip lookup (fakes ++ mains) =<< listToMaybe xs of
        _ | null xs -> do
            putStrLn "******************************************************************"
            putStrLn "** Running shake test suite, run with '--help' to see arguments **"
            putStrLn "******************************************************************"
            unlessM (doesFileExist "shake.cabal") $ do
                putStrLn ""
                errorIO "\nERROR: Must run the test suite from a directory containing the Shake repo."
            withArgs ["test"] main
            withArgs ["random","test","3m"] main
        Nothing -> putStrLn $ unlines
            ["Welcome to the Shake demo"
            ,""
            ,unwords $ "Modes:" : map fst fakes
            ,unwords $ "Demos:" : map fst mains
            ,""
            ,"As an example, try:"
            ,""
            ,"  shake-test self --jobs=2 --trace"
            ,""
            ,"Which will build Shake, using Shake, on 2 threads."
            ,"You must run the test suite from a directory containing the Shake repo."
            ]
        Just main -> main =<< sleepFileTimeCalibrate "output/calibrate"


makefile :: IO () -> IO ()
makefile _ = do
    args <- getArgs
    withArgs (drop1 args) Run.main


filetime :: IO () -> IO ()
filetime _ = do
    args <- getArgs
    addTiming "Reading files"
    files <- concatForM (drop1 args) $ \file ->
        BS.lines . BS.filter (/= '\r') <$> BS.readFile file
    let n = length files
    evaluate n
    addTiming "Modtime"
    let (a,bcd) = splitAt (n `div` 4) files
    let (b,cd) = splitAt (n `div` 4) bcd
    let (c,d) = splitAt (n `div` 4) cd
    vars <- forM [a,b,c,d] $ \xs ->
        onceFork $ mapM_ (getFileInfo False . fileNameFromByteString) xs
    sequence_ vars


clean :: IO () -> IO ()
clean extra = sequence_ [withArgs [name,"clean"] $ main extra | (name,main) <- mains]


test :: IO () -> IO ()
test yield = do
    args <- getArgs
    flip onException (putStrLn "TESTS FAILED") $
        sequence_ [withArgs (name:"test":drop1 args) $ test yield | (name,test) <- mains, name /= "random"]
