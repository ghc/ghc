{-# LANGUAGE CPP #-}
module FileTime where
#include "util.inl"
import Data.Time.Clock (addUTCTime, getCurrentTime)

main :: TestEnv -> IO ()
main _t = do
  now <- getCurrentTime
  let someTimeAgo  = addUTCTime (-3600) now
      someTimeAgo' = addUTCTime (-7200) now

  T(expectIOErrorType) () isDoesNotExistError $
    getAccessTime "nonexistent-file"
  T(expectIOErrorType) () isDoesNotExistError $
    setAccessTime "nonexistent-file" someTimeAgo
  T(expectIOErrorType) () isDoesNotExistError $
    getModificationTime "nonexistent-file"
  T(expectIOErrorType) () isDoesNotExistError $
    setModificationTime "nonexistent-file" someTimeAgo

  writeFile  "foo" ""
  for_ [ "foo", ".", "" ] $ \ file -> do
    let mtime = someTimeAgo
        atime = someTimeAgo'

    atime1 <- getAccessTime file

    setModificationTime file mtime

    atime2 <- getAccessTime file
    mtime2 <- getModificationTime file

    -- modification time should be set with at worst 1 sec resolution
    T(expectNearTime) file mtime  mtime2 1

    -- access time should not change, although it may lose some precision
    -- on POSIX systems without 'utimensat'
    T(expectNearTime) file atime1 atime2 1

    setAccessTime file atime

    atime3 <- getAccessTime file
    mtime3 <- getModificationTime file

    -- access time should be set with at worst 1 sec resolution
    T(expectNearTime) file atime  atime3 1

    -- modification time should not change, although it may lose some precision
    -- on POSIX systems without 'utimensat'
    T(expectNearTime) file mtime2 mtime3 1
