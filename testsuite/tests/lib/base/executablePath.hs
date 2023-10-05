import Control.Monad (unless)
import System.Environment (executablePath, getArgs)
import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath ((</>), dropExtension, equalFilePath)
import System.Exit (exitSuccess, die)

canQuery, canDelete, canQueryAfterDelete :: [String]
canQuery = ["mingw32", "freebsd", "linux", "darwin", "netbsd"]
canDelete = ["freebsd", "linux", "darwin", "netbsd"]
canQueryAfterDelete = ["netbsd"]


main :: IO ()
main = do
  -- If executablePath = Nothing, then this platform
  -- cannot return the executable path.  So just exit
  -- with a success value.
  [os] <- getArgs
  query <- case (os `elem` canQuery, executablePath) of
    (False, Nothing) -> exitSuccess  -- no query, as expected
    (False, Just _) -> die "executablePath unexpectedly defined; this test needs an update!"
    (True, Nothing) -> die "executablePath unexpected not defined"
    (True, Just k) -> pure k

  -- At this point, the query should return the path to the test program.
  before <- query >>= \r -> case r of
    Nothing
      -> die "executablePath query unexpected returned Nothing"
    Just path
      -> pure path

  cwd <- getCurrentDirectory
  let
    -- On some platforms the executable has a file extension
    -- (e.g. ".exe" on Windows). Drop the extension when comparing.
    expected  = cwd </> "executablePath"
    actual    = dropExtension before
  unless (equalFilePath actual expected) $
      die $ "executablePath query returned `" <> actual <> "`; expected `" <> expected <> "`"

  unless (os `elem` canDelete)
    -- This OS cannot delete the executable file while it is
    -- still being executed.  There is nothing left to test.
    exitSuccess

  -- Remove the file
  removeFile before

  -- Now query again, after deletion
  after <- query
  case after of
    Nothing
      | os `elem` canQueryAfterDelete
      -> die "query failed after deletion, but expected success"
      | otherwise
      -> pure ()
    Just _
      | os `elem` canQueryAfterDelete
      -> pure ()
      | otherwise
      -> die $ "query succeeded after deleted (result: " <> show after <> "), but expected failure"
