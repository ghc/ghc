
-- GHC trac #2969

import System.Posix.Files
import System.Posix.Directory
import System.Posix.IO
import Control.Exception as E
import Control.Monad

main = do
  cleanup
  fs <- testRegular
  ds <- testDir
  testSymlink fs ds
  cleanup

regular      = "regular"
dir          = "dir"
link_regular = "link-regular"
link_dir     = "link-dir"

testRegular = do
  createFile regular ownerReadMode
  (fs, _) <- getStatus regular
  let expected = (False,False,False,True,False,False,False)
      actual   = snd (statusElements fs)
  when (actual /= expected) $
    fail "unexpected file status bits for regular file"
  return fs

testDir = do
  createDirectory dir ownerReadMode
  (ds, _) <- getStatus dir
  let expected = (False,False,False,False,True,False,False)
      actual   = snd (statusElements ds)
  when (actual /= expected) $
    fail "unexpected file status bits for directory"
  return ds

testSymlink fs ds = do
  createSymbolicLink regular link_regular
  createSymbolicLink dir     link_dir
  (fs', ls)  <- getStatus link_regular
  (ds', lds) <- getStatus link_dir

  let expected = (False,False,False,False,False,True,False)
      actualF  = snd (statusElements ls)
      actualD  = snd (statusElements lds)

  when (actualF /= expected) $
    fail "unexpected file status bits for symlink to regular file"

  when (actualD /= expected) $
    fail "unexpected file status bits for symlink to directory"

  when (statusElements fs /= statusElements fs') $
    fail "status for a file does not match when it's accessed via a symlink"

  when (statusElements ds /= statusElements ds') $
    fail "status for a directory does not match when it's accessed via a symlink"

cleanup = do
  ignoreIOExceptions $ removeDirectory dir
  mapM_ (ignoreIOExceptions . removeLink)
        [regular, link_regular, link_dir]

ignoreIOExceptions io = io `E.catch`
                        ((\_ -> return ()) :: IOException -> IO ())

getStatus f = do
  fs  <- getFileStatus f
  ls  <- getSymbolicLinkStatus f

  fd  <- openFd f ReadOnly Nothing defaultFileFlags
  fs' <- getFdStatus fd

  when (statusElements fs /= statusElements fs') $
    fail "getFileStatus and getFdStatus give inconsistent results"

  when (not (isSymbolicLink ls) && statusElements fs /= statusElements fs') $
    fail $ "getFileStatus and getSymbolicLinkStatus give inconsistent results "
        ++ "on a file that is not a symbolic link"

  return (fs, ls)

-- Yay for 20-element tuples!
statusElements fs = (,)
  (deviceID fs
  ,fileMode fs
  ,linkCount fs
  ,fileOwner fs
  ,fileGroup fs
  ,specialDeviceID fs
  ,fileSize fs
  ,accessTime fs
  ,accessTimeHiRes fs
  ,modificationTime fs
  ,modificationTimeHiRes fs
  ,statusChangeTime fs
  ,statusChangeTimeHiRes fs
  )
  (isBlockDevice fs
  ,isCharacterDevice fs
  ,isNamedPipe fs
  ,isRegularFile fs
  ,isDirectory fs
  ,isSymbolicLink fs
  ,isSocket fs
  )
