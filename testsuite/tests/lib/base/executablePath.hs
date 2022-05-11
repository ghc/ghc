import Control.Monad (unless)
import System.Environment (executablePath, getArgs)
import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath ((</>), dropExtension)
import System.Exit (exitSuccess, die)

canQuery, canDelete :: [String]
canQuery = ["mingw32", "freebsd", "linux", "darwin"]
canDelete = ["freebsd", "linux", "darwin"]

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
  unless (actual == expected) $
      die $ "executablePath query returned `" <> actual <> "`; expected `" <> expected <> "`"

  unless (os `elem` canDelete)
    -- This OS cannot delete the executable file while it is
    -- still being executed.  There is nothing left to test.
    exitSuccess

  -- Remove the file
  removeFile before

  -- Now query should return Nothing
  after <- query
  unless (after == Nothing) $ die $
    "executablePath expected to return Nothing, returned " <> show after
