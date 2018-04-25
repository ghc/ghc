{-# LANGUAGE CPP #-}
module LongPaths where
#include "util.inl"
import TestUtils
import System.FilePath ((</>))

main :: TestEnv -> IO ()
main _t = do
  let longName = mconcat (replicate 10 "its_very_long")
  longDir <- makeAbsolute (longName </> longName)

  supportsLongPaths <- do
      -- create 2 dirs because 1 path segment by itself can't exceed MAX_PATH
      -- tests: [createDirectory]
      createDirectory =<< makeAbsolute longName
      createDirectory longDir
      return True
    `catchIOError` \ _ ->
      return False

  -- skip tests on file systems that do not support long paths
  when supportsLongPaths $ do

    -- test relative paths
    let relDir = longName </> mconcat (replicate 8 "yeah_its_long")
    createDirectory relDir
    T(expect) () =<< doesDirectoryExist relDir
    T(expectEq) () [] =<< listDirectory relDir
    setPermissions relDir emptyPermissions
    T(expectEq) () False =<< writable <$> getPermissions relDir

    writeFile "foobar.txt" "^.^" -- writeFile does not support long paths yet

    -- tests: [renamePath], [copyFileWithMetadata]
    renamePath "foobar.txt" (longDir </> "foobar_tmp.txt")
    renamePath (longDir </> "foobar_tmp.txt") (longDir </> "foobar.txt")
    copyFileWithMetadata (longDir </> "foobar.txt")
                         (longDir </> "foobar_copy.txt")

    -- tests: [doesDirectoryExist], [doesFileExist], [doesPathExist]
    T(expect) () =<< doesDirectoryExist longDir
    T(expect) () =<< doesFileExist (longDir </> "foobar.txt")
    T(expect) () =<< doesPathExist longDir
    T(expect) () =<< doesPathExist (longDir </> "foobar.txt")

    -- tests: [getFileSize], [getModificationTime]
    T(expectEq) () 3 =<< getFileSize (longDir </> "foobar.txt")
    _ <- getModificationTime (longDir </> "foobar.txt")

    supportsSymbolicLinks <- supportsSymlinks
    when supportsSymbolicLinks $ do

      -- tests: [createDirectoryLink], [getSymbolicLinkTarget], [listDirectory]
      -- also tests expansion of "." and ".."
      createDirectoryLink "." (longDir </> "link")
      _ <- listDirectory (longDir </> ".." </> longName </> "link")
      T(expectEq) () "." =<< getSymbolicLinkTarget (longDir </> "." </> "link")

      return ()

  -- [removeFile], [removeDirectory] are automatically tested by the cleanup
