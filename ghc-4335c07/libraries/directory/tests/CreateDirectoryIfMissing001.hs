{-# LANGUAGE CPP #-}
module CreateDirectoryIfMissing001 where
#include "util.inl"
import System.FilePath ((</>), addTrailingPathSeparator)

main :: TestEnv -> IO ()
main _t = do

  createDirectoryIfMissing False testdir
  cleanup

  T(expectIOErrorType) () isDoesNotExistError $
    createDirectoryIfMissing False testdir_a

  createDirectoryIfMissing True  testdir_a
  createDirectoryIfMissing False testdir_a
  createDirectoryIfMissing False (addTrailingPathSeparator testdir_a)
  cleanup

  createDirectoryIfMissing True  (addTrailingPathSeparator testdir_a)

  T(inform) "testing for race conditions ..."
  raceCheck1
  T(inform) "testing for race conditions ..."
  raceCheck2
  T(inform) "done."
  cleanup

  writeFile testdir testdir
  T(expectIOErrorType) () isAlreadyExistsError $
    createDirectoryIfMissing False testdir
  removeFile testdir
  cleanup

  writeFile testdir testdir
  T(expectIOErrorType) () isNotADirectoryError $
    createDirectoryIfMissing True testdir_a
  removeFile testdir
  cleanup

  where

    testname = "CreateDirectoryIfMissing001"

    testdir = testname <> ".d"
    testdir_a = testdir </> "a"

    numRepeats = T.readArg _t testname "num-repeats" 10000
    numThreads = T.readArg _t testname "num-threads" 4

    -- Look for race conditions (bug #2808 on GHC Trac).  This fails with
    -- +RTS -N2 and directory 1.0.0.2.
    raceCheck1 = do
      m <- newEmptyMVar
      _ <- forkIO $ do
        replicateM_ numRepeats create
        putMVar m ()
      _ <- forkIO $ do
        replicateM_ numRepeats cleanup
        putMVar m ()
      replicateM_ 2 (takeMVar m)

    -- This test fails on Windows (see bug #2924 on GHC Trac):
    raceCheck2 = do
      m <- newEmptyMVar
      replicateM_ numThreads $
        forkIO $ do
          replicateM_ numRepeats $ do
            create
            cleanup
          putMVar m ()
      replicateM_ numThreads (takeMVar m)

    -- createDirectoryIfMissing is allowed to fail with isDoesNotExistError if
    -- another process/thread removes one of the directories during the process
    -- of creating the hierarchy.
    --
    -- It is also allowed to fail with permission errors
    -- (see bug #2924 on GHC Trac)
    create =
      createDirectoryIfMissing True testdir_a `catch` \ e ->
      if isDoesNotExistError e || isPermissionError e || isInappropriateTypeError e
      then return ()
      else ioError e

    cleanup = removeDirectoryRecursive testdir `catchAny` \ _ -> return ()

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = catch

#ifdef mingw32_HOST_OS
    isNotADirectoryError = isAlreadyExistsError
#else
    isNotADirectoryError = isInappropriateTypeError
#endif

    isInappropriateTypeError = isInappropriateTypeErrorType . ioeGetErrorType

    isInappropriateTypeErrorType InappropriateType = True
    isInappropriateTypeErrorType _                 = False
