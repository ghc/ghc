%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixErr]{Haskell 1.3 POSIX Error Codes}

\begin{code}
module LibPosixErr
where

import PreludeGlaST

type ErrorCode = Int

getErrorCode :: IO ErrorCode
getErrorCode =
    _casm_ ``%r = errno;''			    `thenPrimIO` \ errno ->
    return errno

setErrorCode :: ErrorCode -> IO ()
setErrorCode errno =
    _casm_ ``errno = %0;'' errno		    `thenPrimIO` \ () ->
    return ()

noError :: ErrorCode
noError = 0

argumentListTooLong, e2BIG :: ErrorCode
argumentListTooLong = ``E2BIG''
e2BIG = ``E2BIG''

badChannel, eBADF :: ErrorCode
badChannel = ``EBADF''
eBADF = ``EBADF''

brokenPipe, ePIPE :: ErrorCode
brokenPipe = ``EPIPE''
ePIPE = ``EPIPE''

directoryNotEmpty, eNOTEMPTY :: ErrorCode
directoryNotEmpty = ``ENOTEMPTY''
eNOTEMPTY = ``ENOTEMPTY''

execFormatError, eNOEXEC :: ErrorCode
execFormatError = ``ENOEXEC''
eNOEXEC = ``ENOEXEC''

fileAlreadyExists, eEXIST :: ErrorCode
fileAlreadyExists = ``EEXIST''
eEXIST = ``EEXIST''

fileTooLarge, eFBIG :: ErrorCode
fileTooLarge = ``EFBIG''
eFBIG = ``EFBIG''

filenameTooLong, eNAMETOOLONG :: ErrorCode
filenameTooLong = ``ENAMETOOLONG''
eNAMETOOLONG = ``ENAMETOOLONG''

improperLink, eXDEV :: ErrorCode
improperLink = ``EXDEV''
eXDEV = ``EXDEV''

inappropriateIOControlOperation, eNOTTY :: ErrorCode
inappropriateIOControlOperation = ``ENOTTY''
eNOTTY = ``ENOTTY''

inputOutputError, eIO :: ErrorCode
inputOutputError = ``EIO''
eIO = ``EIO''

interruptedOperation, eINTR :: ErrorCode
interruptedOperation = ``EINTR''
eINTR = ``EINTR''

invalidArgument, eINVAL :: ErrorCode
invalidArgument = ``EINVAL''
eINVAL = ``EINVAL''

invalidSeek, eSPIPE :: ErrorCode
invalidSeek = ``ESPIPE''
eSPIPE = ``ESPIPE''

isADirectory, eISDIR :: ErrorCode
isADirectory = ``EISDIR''
eISDIR = ``EISDIR''

noChildProcess, eCHILD :: ErrorCode
noChildProcess = ``ECHILD''
eCHILD = ``ECHILD''

noLocksAvailable, eNOLCK :: ErrorCode
noLocksAvailable = ``ENOLCK''
eNOLCK = ``ENOLCK''

noSpaceLeftOnDevice, eNOSPC :: ErrorCode
noSpaceLeftOnDevice = ``ENOSPC''
eNOSPC = ``ENOSPC''

noSuchOperationOnDevice, eNODEV :: ErrorCode
noSuchOperationOnDevice = ``ENODEV''
eNODEV = ``ENODEV''

noSuchDeviceOrAddress, eNXIO :: ErrorCode
noSuchDeviceOrAddress = ``ENXIO''
eNXIO = ``ENXIO''

noSuchFileOrDirectory, eNOENT :: ErrorCode
noSuchFileOrDirectory = ``ENOENT''
eNOENT = ``ENOENT''

noSuchProcess, eSRCH :: ErrorCode
noSuchProcess = ``ESRCH''
eSRCH = ``ESRCH''

notADirectory, eNOTDIR :: ErrorCode
notADirectory = ``ENOTDIR''
eNOTDIR = ``ENOTDIR''

notEnoughMemory, eNOMEM :: ErrorCode
notEnoughMemory = ``ENOMEM''
eNOMEM = ``ENOMEM''

operationNotImplemented, eNOSYS :: ErrorCode
operationNotImplemented = ``ENOSYS''
eNOSYS = ``ENOSYS''

operationNotPermitted, ePERM :: ErrorCode
operationNotPermitted = ``EPERM''
ePERM = ``EPERM''

permissionDenied, eACCES :: ErrorCode
permissionDenied = ``EACCES''
eACCES = ``EACCES''

readOnlyFileSystem, eROFS :: ErrorCode
readOnlyFileSystem = ``EROFS''
eROFS = ``EROFS''

resourceBusy, eBUSY :: ErrorCode
resourceBusy = ``EBUSY''
eBUSY = ``EBUSY''

resourceDeadlockAvoided, eDEADLK :: ErrorCode
resourceDeadlockAvoided = ``EDEADLK''
eDEADLK = ``EDEADLK''

resourceTemporarilyUnavailable, eAGAIN :: ErrorCode
resourceTemporarilyUnavailable = ``EAGAIN''
eAGAIN = ``EAGAIN''

tooManyLinks, eMLINK :: ErrorCode
tooManyLinks = ``EMLINK''
eMLINK = ``EMLINK''

tooManyOpenFiles, eMFILE :: ErrorCode
tooManyOpenFiles = ``EMFILE''
eMFILE = ``EMFILE''

tooManyOpenFilesInSystem, eNFILE :: ErrorCode
tooManyOpenFilesInSystem = ``ENFILE''
eNFILE = ``ENFILE''

\end{code}

