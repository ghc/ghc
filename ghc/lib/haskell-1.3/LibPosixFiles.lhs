%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixFiles]{Haskell 1.3 POSIX File and Directory Operations}

\begin{code}
module LibPosixFiles (
    DeviceID(..),
    DirStream(..),
    FileID(..),
    FileMode(..),
    FileStatus(..),
    OpenMode(..),
    PathVar(..),

    accessModes,
    accessTime,
    changeWorkingDirectory,	-- Too much like LibDirectory thing?
    closeDirStream,
    createDirectory,	-- Too much like LibDirectory thing?
    createFile,
    createLink,
    createNamedPipe,
    deviceID,
    fileGroup,
    fileID,
    fileMode,
    fileOwner,
    fileSize,
    getChannelStatus,
    getChannelVar,
    getFileStatus,
    getPathVar,
    getWorkingDirectory,	-- Too much like LibDirectory thing?
    groupExecuteMode,
    groupModes,
    groupReadMode,
    groupWriteMode,
    intersectFileModes,
    isBlockDevice,
    isCharacterDevice,
    isDirectory,
    isNamedPipe,
    isRegularFile,
    linkCount,
    modificationTime,
    nullFileMode,
    openDirStream,
    openChannel,
    otherExecuteMode,
    otherModes,
    otherReadMode,
    otherWriteMode,
    ownerExecuteMode,
    ownerModes,
    ownerReadMode,
    ownerWriteMode,
    queryAccess,
    queryFile,
    readDirStream,
    removeDirectory,	-- Too much like LibDirectory thing
    removeLink,
    rename,
    rewindDirStream,
    setFileCreationMask,
    setFileTimes,
    setGroupIDMode,
    setOwnerAndGroup,
    setFileMode,
    setUserIDMode,
    stdError,
    stdFileMode,
    stdInput,
    stdOutput,
    statusChangeTime,
    touchFile,
    unionFileModes
    ) where

import PreludeGlaST
import PS

import LibPosixErr
import LibPosixUtil

import LibDirectory	( removeDirectory,  -- re-use its code
			  getCurrentDirectory,
			  setCurrentDirectory
			)

type DirStream = _Addr

openDirStream :: FilePath -> IO DirStream
openDirStream name = 
    _packBytesForCST name			    `thenStrictlyST` \ dir ->
    _ccall_ opendir dir				    `thenPrimIO` \ dirp ->
    if dirp /= ``NULL'' then
	return dirp
    else
	syserr "openDirStream"

readDirStream :: DirStream -> IO String
readDirStream dirp =
    setErrorCode noError			    >>
    _ccall_ readdir dirp			    `thenPrimIO` \ dirent ->
    if dirent /= (``NULL''::_Addr) then
	_casm_ ``%r = ((struct dirent *)%0)->d_name;'' dirent
						    `thenPrimIO` \ str ->
	strcpy str				    `thenPrimIO` \ name ->
	return name
    else
	getErrorCode				    >>= \ errno ->
	if errno == noError then
	    failWith EOF
	else
	    syserr "readDirStream"

rewindDirStream :: DirStream -> IO ()
rewindDirStream dirp =
    _ccall_ rewinddir dirp			    `thenPrimIO` \ () ->
    return ()

closeDirStream :: DirStream -> IO ()
closeDirStream dirp = 
    _ccall_ closedir dirp			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "closeDirStream"

getWorkingDirectory :: IO FilePath
getWorkingDirectory = getCurrentDirectory{-LibDirectory-}
{- OLD:
    _ccall_ getCurrentDirectory			    `thenPrimIO` \ str ->
    if str /= ``NULL'' then
        strcpy str				    `thenPrimIO` \ pwd ->
        _ccall_ free str			    `thenPrimIO` \ () ->
        return pwd
    else
	syserr "getWorkingDirectory"
-}

changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory name = setCurrentDirectory{-LibDirectory-} name
{- OLD:
    _packBytesForCST name			    `thenStrictlyST` \ dir ->
    _ccall_ chdir dir				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "changeWorkingDirectory"
-}

type FileMode = _Word

nullFileMode :: FileMode
nullFileMode = ``0''

ownerReadMode :: FileMode
ownerReadMode = ``S_IRUSR''

ownerWriteMode :: FileMode
ownerWriteMode = ``S_IWUSR''

ownerExecuteMode :: FileMode
ownerExecuteMode = ``S_IXUSR''

groupReadMode :: FileMode
groupReadMode = ``S_IRGRP''

groupWriteMode :: FileMode
groupWriteMode = ``S_IWGRP''

groupExecuteMode :: FileMode
groupExecuteMode = ``S_IXGRP''

otherReadMode :: FileMode
otherReadMode = ``S_IROTH''

otherWriteMode :: FileMode
otherWriteMode = ``S_IWOTH''

otherExecuteMode :: FileMode
otherExecuteMode = ``S_IXOTH''

setUserIDMode :: FileMode
setUserIDMode = ``S_ISUID''

setGroupIDMode :: FileMode
setGroupIDMode = ``S_ISGID''

stdFileMode :: FileMode
stdFileMode = ``(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)''

ownerModes :: FileMode
ownerModes = ``S_IRWXU''

groupModes :: FileMode
groupModes = ``S_IRWXG''

otherModes :: FileMode
otherModes = ``S_IRWXO''

accessModes :: FileMode
accessModes = ``(S_IRWXU|S_IRWXG|S_IRWXO)''

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes (W# m1#) (W# m2#) = W# (m1# `or#` m2#)

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes (W# m1#) (W# m2#) = W# (m1# `and#` m2#)

stdInput :: Channel
stdInput = 0

stdOutput :: Channel
stdOutput = 1

stdError :: Channel
stdError = 2

data OpenMode = ReadOnly 
              | WriteOnly 
              | ReadWrite

openChannel :: FilePath
                   -> OpenMode
                   -> Maybe FileMode	-- Just x => O_CREAT, Nothing => must exist
                   -> Bool		-- O_APPEND
                   -> Bool		-- O_EXCL
                   -> Bool		-- O_NOCTTY
                   -> Bool		-- O_NONBLOCK
                   -> Bool		-- O_TRUNC
                   -> IO Channel
openChannel name how maybe_mode append excl noctty nonblock trunc = 
    _packBytesForCST name			    `thenStrictlyST` \ file ->
    _ccall_ open file flags mode		    `thenPrimIO` \ fd ->
    if fd /= -1 then
	return fd
    else
	syserr "openChannel"
  where
    mode, creat :: FileMode
    mode = case maybe_mode of { Nothing -> ``0'' ; Just x -> x }

    creat = case maybe_mode of { Nothing -> ``0'' ; Just _ -> ``O_CREAT'' }
    creat# = case creat of { W# x -> x }

    flags = W# (creat# `or#` append# `or#` excl# `or#` 
                noctty# `or#` nonblock# `or#` trunc#)
    append#   = case (if append   then ``O_APPEND''   else ``0'') of { W# x -> x }
    excl#     = case (if excl     then ``O_EXCL''     else ``0'') of { W# x -> x }
    noctty#   = case (if noctty   then ``O_NOCTTY''   else ``0'') of { W# x -> x }
    nonblock# = case (if nonblock then ``O_NONBLOCK'' else ``0'') of { W# x -> x }
    trunc#    = case (if trunc    then ``O_TRUNC''    else ``0'') of { W# x -> x }

createFile :: FilePath -> FileMode -> IO Channel
createFile name mode = 
    _packBytesForCST name			    `thenStrictlyST` \ file ->
    _ccall_ creat file mode			    `thenPrimIO` \ fd ->
    if fd /= -1 then
	return fd
    else
	syserr "createFile"

setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = 
    _ccall_ umask mask				    `thenPrimIO` \ omask ->
    return omask

createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 =
    _packBytesForCST name1			    `thenStrictlyST` \ path1 ->
    _packBytesForCST name2			    `thenStrictlyST` \ path2 ->
    _ccall_ link path1 path2			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "createLink"
 
createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode = -- NB: diff signature from LibDirectory one!
    _packBytesForCST name			    `thenStrictlyST` \ dir ->
    _ccall_ mkdir dir mode			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "createDirectory"

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode =
    _packBytesForCST name			    `thenStrictlyST` \ pipe ->
    _ccall_ mkfifo pipe mode			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "createNamedPipe"

removeLink :: FilePath -> IO ()
removeLink name = 
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ unlink path				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "removeLink"

{- USE LibDirectory ONE:
removeDirectory :: FilePath -> IO ()
removeDirectory name =
    _packBytesForCST name			    `thenStrictlyST` \ dir ->
    _ccall_ rmdir dir				    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "removeDirectory"
-}

rename :: FilePath -> FilePath -> IO ()
rename name1 name2 =
    _packBytesForCST name1			    `thenStrictlyST` \ path1 ->
    _packBytesForCST name2			    `thenStrictlyST` \ path2 ->
    _ccall_ rename path1 path2			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "rename"

type FileStatus = _ByteArray ()
type FileID = Int
type DeviceID = Int

fileMode :: FileStatus -> FileMode
fileMode stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_mode;'' stat
						    `thenStrictlyST` \ mode ->
    returnPrimIO mode)

fileID :: FileStatus -> FileID
fileID stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_ino;'' stat
						    `thenStrictlyST` \ ino ->
    returnPrimIO ino)

deviceID :: FileStatus -> DeviceID
deviceID stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_dev;'' stat
						    `thenStrictlyST` \ dev ->
    returnPrimIO dev)

linkCount :: FileStatus -> LinkCount
linkCount stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_nlink;'' stat
						    `thenStrictlyST` \ nlink ->
    returnPrimIO nlink)

fileOwner :: FileStatus -> UserID
fileOwner stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_uid;'' stat
						    `thenStrictlyST` \ uid ->
    returnPrimIO uid)

fileGroup :: FileStatus -> GroupID
fileGroup stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_gid;'' stat
						    `thenStrictlyST` \ gid ->
    returnPrimIO gid)

fileSize :: FileStatus -> FileOffset
fileSize stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_size;'' stat
						    `thenStrictlyST` \ size ->
    returnPrimIO size)

accessTime :: FileStatus -> EpochTime
accessTime stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_atime;'' stat
						    `thenStrictlyST` \ atime ->
    returnPrimIO atime)

modificationTime :: FileStatus -> EpochTime
modificationTime stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_mtime;'' stat
						    `thenStrictlyST` \ mtime ->
    returnPrimIO mtime)

statusChangeTime :: FileStatus -> EpochTime
statusChangeTime stat = unsafePerformPrimIO (
    _casm_ ``%r = ((struct stat *)%0)->st_ctime;'' stat
						    `thenStrictlyST` \ ctime ->
    returnPrimIO ctime)

isDirectory :: FileStatus -> Bool
isDirectory stat = unsafePerformPrimIO (
    _casm_ ``%r = S_ISDIR(((struct stat *)%0)->st_mode);'' stat
						    `thenStrictlyST` \ rc ->
    returnPrimIO (rc /= 0))

isCharacterDevice :: FileStatus -> Bool
isCharacterDevice stat = unsafePerformPrimIO (
    _casm_ ``%r = S_ISCHR(((struct stat *)%0)->st_mode);'' stat
						    `thenStrictlyST` \ rc ->
    returnPrimIO (rc /= 0))

isBlockDevice :: FileStatus -> Bool
isBlockDevice stat = unsafePerformPrimIO (
    _casm_ ``%r = S_ISBLK(((struct stat *)%0)->st_mode);'' stat
						    `thenStrictlyST` \ rc ->
    returnPrimIO (rc /= 0))

isRegularFile :: FileStatus -> Bool
isRegularFile stat = unsafePerformPrimIO (
    _casm_ ``%r = S_ISREG(((struct stat *)%0)->st_mode);'' stat
						    `thenStrictlyST` \ rc ->
    returnPrimIO (rc /= 0))

isNamedPipe :: FileStatus -> Bool
isNamedPipe stat = unsafePerformPrimIO (
    _casm_ ``%r = S_ISFIFO(((struct stat *)%0)->st_mode);'' stat
						    `thenStrictlyST` \ rc ->
    returnPrimIO (rc /= 0))

getFileStatus :: FilePath -> IO FileStatus
getFileStatus name =
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    allocChars ``sizeof(struct stat)''		    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = stat(%0,(struct stat *)%1);'' path bytes
						    `thenPrimIO` \ rc ->
    if rc == 0 then
	freeze bytes				    `thenStrictlyST` \ stat ->
	return stat
    else
	syserr "getFileStatus"

getChannelStatus :: Channel -> IO FileStatus
getChannelStatus fd = 
    allocChars ``sizeof(struct stat)''		    `thenStrictlyST` \ bytes ->
    _casm_ ``%r = fstat(%0,(struct stat *)%1);'' fd bytes
						    `thenPrimIO` \ rc ->
    if rc == 0 then
	freeze bytes				    `thenStrictlyST` \ stat ->
	return stat
    else
	syserr "getChannelStatus"

queryAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
queryAccess name read write exec = 
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ access path flags			    `thenPrimIO` \ rc ->
    return (rc == 0)
  where
    flags = I# (word2Int# (read# `or#` write# `or#` exec#))
    read#  = case (if read  then ``R_OK'' else ``0'') of { W# x -> x }
    write# = case (if write then ``W_OK'' else ``0'') of { W# x -> x }
    exec#  = case (if exec  then ``X_OK'' else ``0'') of { W# x -> x }

queryFile :: FilePath -> IO Bool
queryFile name = 
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ access path (``F_OK''::Int)		    `thenPrimIO` \ rc ->
    return (rc == 0)

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name mode = 
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ chmod path mode			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setFileMode"

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = 
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ chown path uid gid			    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setOwnerAndGroup"

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime =
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _casm_ ``do {struct utimbuf ub; ub.actime = (time_t) %0; ub.modtime = (time_t) %1;
	     %r = utime(%2, &ub);} while(0);'' atime mtime path
						    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "setFileTimes"

touchFile :: FilePath -> IO ()
touchFile name =
    _packBytesForCST name			    `thenStrictlyST` \ path ->
    _ccall_ utime path (``NULL''::_Addr)	    `thenPrimIO` \ rc ->
    if rc == 0 then
	return ()
    else
	syserr "touchFile"

data PathVar = LinkLimit
             | InputLineLimit
             | InputQueueLimit
             | FileNameLimit
             | PathNameLimit
             | PipeBufferLimit
             | SetOwnerAndGroupIsRestricted
             | FileNamesAreNotTruncated

getPathVar :: PathVar -> FilePath -> IO Limit
getPathVar v name =
    case v of
      LinkLimit -> pathconf ``_PC_LINK_MAX''
      InputLineLimit -> pathconf ``_PC_MAX_CANON''
      InputQueueLimit -> pathconf ``_PC_MAX_INPUT''
      FileNameLimit -> pathconf ``_PC_NAME_MAX''
      PathNameLimit -> pathconf ``_PC_PATH_MAX''
      PipeBufferLimit -> pathconf ``_PC_PIPE_BUF''
      SetOwnerAndGroupIsRestricted -> pathconf ``_PC_CHOWN_RESTRICTED''
      FileNamesAreNotTruncated -> pathconf ``_PC_NO_TRUNC''
  where
    pathconf :: Int -> IO Limit
    pathconf n =
	_packBytesForCST name			    `thenStrictlyST` \ path ->
	_ccall_ pathconf path n			    `thenPrimIO` \ rc ->
	if rc /= -1 then
	    return rc
	else
	    getErrorCode			    >>= \ errno ->
	    if errno == invalidArgument then
		failWith (NoSuchThing "no such path limit or option")
	else
	    syserr "getPathVar"

getChannelVar :: PathVar -> Channel -> IO Limit
getChannelVar v fd =
    case v of
      LinkLimit -> fpathconf ``_PC_LINK_MAX''
      InputLineLimit -> fpathconf ``_PC_MAX_CANON''
      InputQueueLimit -> fpathconf ``_PC_MAX_INPUT''
      FileNameLimit -> fpathconf ``_PC_NAME_MAX''
      PathNameLimit -> fpathconf ``_PC_PATH_MAX''
      PipeBufferLimit -> fpathconf ``_PC_PIPE_BUF''
      SetOwnerAndGroupIsRestricted -> fpathconf ``_PC_CHOWN_RESTRICTED''
      FileNamesAreNotTruncated -> fpathconf ``_PC_NO_TRUNC''
  where
    fpathconf :: Int -> IO Limit
    fpathconf n =
	_ccall_ fpathconf fd n			    `thenPrimIO` \ rc ->
	if rc /= -1 then
	    return rc
	else
	    getErrorCode			    >>= \ errno ->
	    if errno == invalidArgument then
		failWith (NoSuchThing "no such path limit or option")
	    else
		syserr "getPathVar"

\end{code}
