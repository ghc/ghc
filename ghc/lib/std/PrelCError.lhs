% -----------------------------------------------------------------------------
% $Id: PrelCError.lhs,v 1.5 2001/01/26 17:51:54 rrt Exp $
%
% (c) The FFI task force, 2000
%

C-specific Marshalling support: Handling of C "errno" error codes

\begin{code}
{-# OPTIONS -fno-implicit-prelude -#include "cbits/errno.h" #-}

-- this is were we get the CCONST_XXX definitions from that configure
-- calculated for us
--
#include "config.h"

module PrelCError (

  -- Haskell representation for "errno" values
  --
  Errno(..),		-- instance: Eq
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
                        -- :: Errno
  isValidErrno,		-- :: Errno -> Bool

  -- access to the current thread's "errno" value
  --
  getErrno,             -- :: IO Errno
  resetErrno,           -- :: IO ()

  -- conversion of an "errno" value into IO error
  --
  errnoToIOError,       -- :: String       -- location
                        -- -> Errno        -- errno
                        -- -> Maybe Handle -- handle
                        -- -> Maybe String -- filename
                        -- -> IOError

  -- throw current "errno" value
  --
  throwErrno,           -- ::                String               -> IO a

  -- guards for IO operations that may fail
  --
  throwErrnoIf,         -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIf_,        -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfRetry,    -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIfRetry_,   -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfMinus1,   -- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1_,  -- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfMinus1Retry,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1Retry_,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfNull,	-- ::                String -> IO (Ptr a) -> IO (Ptr a)
  throwErrnoIfNullRetry -- ::                String -> IO (Ptr a) -> IO (Ptr a)
) where


-- system dependent imports
-- ------------------------

-- GHC allows us to get at the guts inside IO errors/exceptions
--
#if __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 409
import PrelIOBase (IOError(..), IOErrorType(..))
#else
import PrelIOBase (Exception(..), IOException(..), IOErrorType(..))
#endif
#endif /* __GLASGOW_HASKELL__ */


-- regular imports
-- ---------------

import Monad        (liftM)

#if __GLASGOW_HASKELL__
import PrelStorable
import PrelMarshalError
import PrelCTypes
import PrelIOBase
import PrelPtr
import PrelNum
import PrelShow
import PrelMaybe
import PrelBase
#else
import Ptr          (Ptr, nullPtr)
import CTypes       (CInt)
import MarshalError (void)

import IO           (IOError, Handle, ioError)
#endif

-- system dependent re-definitions
-- -------------------------------

-- we bring GHC's `IOErrorType' in scope in other compilers to simplify the
-- routine `errnoToIOError' below
--
#if !__GLASGOW_HASKELL__
data IOErrorType
  = AlreadyExists        | HardwareFault
  | IllegalOperation     | InappropriateType
  | Interrupted          | InvalidArgument
  | NoSuchThing          | OtherError
  | PermissionDenied     | ProtocolError
  | ResourceBusy         | ResourceExhausted
  | ResourceVanished     | SystemError
  | TimeExpired          | UnsatisfiedConstraints
  | UnsupportedOperation
  | EOF
#endif


-- "errno" type
-- ------------

-- import of C function that gives address of errno
--
foreign import "ghcErrno" unsafe _errno :: Ptr CInt

-- Haskell representation for "errno" values
--
newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == errno2@(Errno no2) 
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise					 = False

-- common "errno" symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV		       :: Errno
--
-- the CCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure 
--
eOK             = Errno 0
e2BIG           = Errno (CCONST_E2BIG)
eACCES		= Errno (CCONST_EACCES)
eADDRINUSE	= Errno (CCONST_EADDRINUSE)
eADDRNOTAVAIL	= Errno (CCONST_EADDRNOTAVAIL)
eADV		= Errno (CCONST_EADV)
eAFNOSUPPORT	= Errno (CCONST_EAFNOSUPPORT)
eAGAIN		= Errno (CCONST_EAGAIN)
eALREADY	= Errno (CCONST_EALREADY)
eBADF		= Errno (CCONST_EBADF)
eBADMSG		= Errno (CCONST_EBADMSG)
eBADRPC		= Errno (CCONST_EBADRPC)
eBUSY		= Errno (CCONST_EBUSY)
eCHILD		= Errno (CCONST_ECHILD)
eCOMM		= Errno (CCONST_ECOMM)
eCONNABORTED	= Errno (CCONST_ECONNABORTED)
eCONNREFUSED	= Errno (CCONST_ECONNREFUSED)
eCONNRESET	= Errno (CCONST_ECONNRESET)
eDEADLK		= Errno (CCONST_EDEADLK)
eDESTADDRREQ	= Errno (CCONST_EDESTADDRREQ)
eDIRTY		= Errno (CCONST_EDIRTY)
eDOM		= Errno (CCONST_EDOM)
eDQUOT		= Errno (CCONST_EDQUOT)
eEXIST		= Errno (CCONST_EEXIST)
eFAULT		= Errno (CCONST_EFAULT)
eFBIG		= Errno (CCONST_EFBIG)
eFTYPE		= Errno (CCONST_EFTYPE)
eHOSTDOWN	= Errno (CCONST_EHOSTDOWN)
eHOSTUNREACH	= Errno (CCONST_EHOSTUNREACH)
eIDRM		= Errno (CCONST_EIDRM)
eILSEQ		= Errno (CCONST_EILSEQ)
eINPROGRESS	= Errno (CCONST_EINPROGRESS)
eINTR		= Errno (CCONST_EINTR)
eINVAL		= Errno (CCONST_EINVAL)
eIO		= Errno (CCONST_EIO)
eISCONN		= Errno (CCONST_EISCONN)
eISDIR		= Errno (CCONST_EISDIR)
eLOOP		= Errno (CCONST_ELOOP)
eMFILE		= Errno (CCONST_EMFILE)
eMLINK		= Errno (CCONST_EMLINK)
eMSGSIZE	= Errno (CCONST_EMSGSIZE)
eMULTIHOP	= Errno (CCONST_EMULTIHOP)
eNAMETOOLONG	= Errno (CCONST_ENAMETOOLONG)
eNETDOWN	= Errno (CCONST_ENETDOWN)
eNETRESET	= Errno (CCONST_ENETRESET)
eNETUNREACH	= Errno (CCONST_ENETUNREACH)
eNFILE		= Errno (CCONST_ENFILE)
eNOBUFS		= Errno (CCONST_ENOBUFS)
eNODATA		= Errno (CCONST_ENODATA)
eNODEV		= Errno (CCONST_ENODEV)
eNOENT		= Errno (CCONST_ENOENT)
eNOEXEC		= Errno (CCONST_ENOEXEC)
eNOLCK		= Errno (CCONST_ENOLCK)
eNOLINK		= Errno (CCONST_ENOLINK)
eNOMEM		= Errno (CCONST_ENOMEM)
eNOMSG		= Errno (CCONST_ENOMSG)
eNONET		= Errno (CCONST_ENONET)
eNOPROTOOPT	= Errno (CCONST_ENOPROTOOPT)
eNOSPC		= Errno (CCONST_ENOSPC)
eNOSR		= Errno (CCONST_ENOSR)
eNOSTR		= Errno (CCONST_ENOSTR)
eNOSYS		= Errno (CCONST_ENOSYS)
eNOTBLK		= Errno (CCONST_ENOTBLK)
eNOTCONN	= Errno (CCONST_ENOTCONN)
eNOTDIR		= Errno (CCONST_ENOTDIR)
eNOTEMPTY	= Errno (CCONST_ENOTEMPTY)
eNOTSOCK	= Errno (CCONST_ENOTSOCK)
eNOTTY		= Errno (CCONST_ENOTTY)
eNXIO		= Errno (CCONST_ENXIO)
eOPNOTSUPP	= Errno (CCONST_EOPNOTSUPP)
ePERM		= Errno (CCONST_EPERM)
ePFNOSUPPORT	= Errno (CCONST_EPFNOSUPPORT)
ePIPE		= Errno (CCONST_EPIPE)
ePROCLIM	= Errno (CCONST_EPROCLIM)
ePROCUNAVAIL	= Errno (CCONST_EPROCUNAVAIL)
ePROGMISMATCH	= Errno (CCONST_EPROGMISMATCH)
ePROGUNAVAIL	= Errno (CCONST_EPROGUNAVAIL)
ePROTO		= Errno (CCONST_EPROTO)
ePROTONOSUPPORT = Errno (CCONST_EPROTONOSUPPORT)
ePROTOTYPE	= Errno (CCONST_EPROTOTYPE)
eRANGE		= Errno (CCONST_ERANGE)
eREMCHG		= Errno (CCONST_EREMCHG)
eREMOTE		= Errno (CCONST_EREMOTE)
eROFS		= Errno (CCONST_EROFS)
eRPCMISMATCH	= Errno (CCONST_ERPCMISMATCH)
eRREMOTE	= Errno (CCONST_ERREMOTE)
eSHUTDOWN	= Errno (CCONST_ESHUTDOWN)
eSOCKTNOSUPPORT = Errno (CCONST_ESOCKTNOSUPPORT)
eSPIPE		= Errno (CCONST_ESPIPE)
eSRCH		= Errno (CCONST_ESRCH)
eSRMNT		= Errno (CCONST_ESRMNT)
eSTALE		= Errno (CCONST_ESTALE)
eTIME		= Errno (CCONST_ETIME)
eTIMEDOUT	= Errno (CCONST_ETIMEDOUT)
eTOOMANYREFS	= Errno (CCONST_ETOOMANYREFS)
eTXTBSY		= Errno (CCONST_ETXTBSY)
eUSERS		= Errno (CCONST_EUSERS)
eWOULDBLOCK	= Errno (CCONST_EWOULDBLOCK)
eXDEV		= Errno (CCONST_EXDEV)

-- checks whether the given errno value is supported on the current
-- architecture
--
isValidErrno               :: Errno -> Bool
--
-- the configure script sets all invalid "errno"s to -1
--
isValidErrno (Errno errno)  = errno /= -1


-- access to the current thread's "errno" value
-- --------------------------------------------

-- yield the current thread's "errno" value
--
getErrno :: IO Errno
getErrno  = liftM Errno (peek _errno)


-- set the current thread's "errno" value to 0
--
resetErrno :: IO ()
resetErrno  = poke _errno 0


-- throw current "errno" value
-- ---------------------------

-- the common case: throw an IO error based on a textual description
-- of the error location and the current thread's "errno" value
--
throwErrno     :: String -> IO a
throwErrno loc  =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing Nothing)


-- guards for IO operations that may fail
-- --------------------------------------

-- guard an IO operation and throw an "errno" based exception of the result
-- value of the IO operation meets the given predicate
--
throwErrnoIf            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf pred loc f  = 
  do
    res <- f
    if pred res then throwErrno loc else return res

-- as `throwErrnoIf', but discards the result
--
throwErrnoIf_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

-- as `throwErrnoIf', but retries interrupted IO operations (ie, those whose
-- flag `EINTR')
--
throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  = 
  do
    res <- f
    if pred res
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfRetry pred loc f
	  else throwErrno loc
      else return res

-- as `throwErrnoIfRetry', but discards the result
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- throws "errno" if a result of "-1" is returned
--
throwErrnoIfMinus1 :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== -1)

-- throws "errno" if a result of "-1" is returned, but retries in case of an
-- interrupted operation
--
throwErrnoIfMinus1Retry :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1Retry_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== -1)

-- throws "errno" if a result of a NULL pointer is returned
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- throws "errno" if a result of a NULL pointer is returned, but retries in
-- case of an interrupted operation
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)


-- conversion of an "errno" value into IO error
-- --------------------------------------------

-- convert a location string, an "errno" value, an optional handle,
-- and an optional filename into a matching IO error
--
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError loc errno@(Errno no) maybeHdl maybeName =
#if __GLASGOW_HASKELL__
  IOException (IOError maybeHdl errType loc str maybeName)
#else
  userError (loc ++ ": " ++ str ++ maybe "" (": "++) maybeName)
#endif
  where
    (errType, str)
      | errno == eOK              = (OtherError,
				     "no error")
      | errno == e2BIG            = (ResourceExhausted,
				     "argument list too long")
      | errno == eACCES		  = (PermissionDenied,
				     "inadequate access permission")
      | errno == eADDRINUSE	  = (ResourceBusy,
				     "address already in use")
      | errno == eADDRNOTAVAIL	  = (UnsupportedOperation,
				     "address not available")
      | errno == eADV		  = (OtherError,
				     "RFS advertise error")
      | errno == eAFNOSUPPORT	  = (UnsupportedOperation,
				     "address family not supported by " ++
				     "protocol family")
				     -- no multiline strings with cpp
      | errno == eAGAIN		  = (ResourceExhausted,
				     "insufficient resources")
      | errno == eALREADY	  = (AlreadyExists,
				     "operation already in progress")
      | errno == eBADF		  = (OtherError,
				     "internal error (EBADF)")
      | errno == eBADMSG	  = (InappropriateType,
				     "next message has wrong type")
      | errno == eBADRPC	  = (OtherError,
				     "invalid RPC request or response")
      | errno == eBUSY		  = (ResourceBusy,
				     "device busy")
      | errno == eCHILD		  = (NoSuchThing,
				     "no child processes")
      | errno == eCOMM		  = (ResourceVanished,
				     "no virtual circuit could be found")
      | errno == eCONNABORTED	  = (OtherError,
				     "aborted connection")
      | errno == eCONNREFUSED	  = (NoSuchThing,
				     "no listener on remote host")
      | errno == eCONNRESET	  = (ResourceVanished,
				     "connection reset by peer")
      | errno == eDEADLK	  = (ResourceBusy,
				     "resource deadlock avoided")
      | errno == eDESTADDRREQ	  = (InvalidArgument,
				     "destination address required")
      | errno == eDIRTY		  = (UnsatisfiedConstraints,
				     "file system dirty")
      | errno == eDOM		  = (InvalidArgument,
				     "argument too large")
      | errno == eDQUOT		  = (PermissionDenied,
				     "quota exceeded")
      | errno == eEXIST		  = (AlreadyExists,
				     "file already exists")
      | errno == eFAULT		  = (OtherError,
				     "internal error (EFAULT)")
      | errno == eFBIG		  = (PermissionDenied,
				     "file too large")
      | errno == eFTYPE		  = (InappropriateType,
				     "inappropriate NFS file type or format")
      | errno == eHOSTDOWN	  = (NoSuchThing,
				     "destination host down")
      | errno == eHOSTUNREACH	  = (NoSuchThing,
				     "remote host is unreachable")
      | errno == eIDRM		  = (ResourceVanished,
				     "IPC identifier removed")
      | errno == eILSEQ		  = (InvalidArgument,
				     "invalid wide character")
      | errno == eINPROGRESS	  = (AlreadyExists,
				     "operation now in progress")
      | errno == eINTR		  = (Interrupted,
				     "interrupted system call")
      | errno == eINVAL		  = (InvalidArgument,
				     "invalid argument")
      | errno == eIO		  = (HardwareFault,
				     "unknown I/O fault")
      | errno == eISCONN	  = (AlreadyExists,
				     "socket is already connected")
      | errno == eISDIR		  = (InappropriateType,
				     "file is a directory")
      | errno == eLOOP		  = (InvalidArgument,
				     "too many symbolic links")
      | errno == eMFILE		  = (ResourceExhausted,
				     "process file table full")
      | errno == eMLINK		  = (ResourceExhausted,
				     "too many links")
      | errno == eMSGSIZE	  = (ResourceExhausted,
				     "message too long")
      | errno == eMULTIHOP	  = (UnsupportedOperation,
				     "multi-hop RFS request")
      | errno == eNAMETOOLONG	  = (InvalidArgument,
				     "filename too long")
      | errno == eNETDOWN	  = (ResourceVanished,
				     "network is down")
      | errno == eNETRESET	  = (ResourceVanished,
				     "remote host rebooted; connection lost")
      | errno == eNETUNREACH	  = (NoSuchThing,
				     "remote network is unreachable")
      | errno == eNFILE		  = (ResourceExhausted,
				     "system file table full")
      | errno == eNOBUFS	  = (ResourceExhausted,
				     "no buffer space available")
      | errno == eNODATA	  = (NoSuchThing,
				     "no message on the stream head read " ++
				     "queue")
				     -- no multiline strings with cpp
      | errno == eNODEV		  = (NoSuchThing,
				     "no such device")
      | errno == eNOENT		  = (NoSuchThing,
				     "no such file or directory")
      | errno == eNOEXEC	  = (InvalidArgument,
				     "not an executable file")
      | errno == eNOLCK		  = (ResourceExhausted,
				     "no file locks available")
      | errno == eNOLINK	  = (ResourceVanished,
				     "RFS link has been severed")
      | errno == eNOMEM		  = (ResourceExhausted,
				     "not enough virtual memory")
      | errno == eNOMSG		  = (NoSuchThing,
				     "no message of desired type")
      | errno == eNONET		  = (NoSuchThing,
				     "host is not on a network")
      | errno == eNOPROTOOPT	  = (UnsupportedOperation,
				     "operation not supported by protocol")
      | errno == eNOSPC		  = (ResourceExhausted,
				     "no space left on device")
      | errno == eNOSR		  = (ResourceExhausted,
				     "out of stream resources")
      | errno == eNOSTR		  = (InvalidArgument,
				     "not a stream device")
      | errno == eNOSYS		  = (UnsupportedOperation,
				     "function not implemented")
      | errno == eNOTBLK	  = (InvalidArgument,
				     "not a block device")
      | errno == eNOTCONN	  = (InvalidArgument,
				     "socket is not connected")
      | errno == eNOTDIR	  = (InappropriateType,
				     "not a directory")
      | errno == eNOTEMPTY	  = (UnsatisfiedConstraints,
				     "directory not empty")
      | errno == eNOTSOCK	  = (InvalidArgument,
				     "not a socket")
      | errno == eNOTTY		  = (IllegalOperation,
				     "inappropriate ioctl for device")
      | errno == eNXIO		  = (NoSuchThing,
				     "no such device or address")
      | errno == eOPNOTSUPP	  = (UnsupportedOperation,
				     "operation not supported on socket")
      | errno == ePERM		  = (PermissionDenied,
				     "privileged operation")
      | errno == ePFNOSUPPORT	  = (UnsupportedOperation,
				     "protocol family not supported")
      | errno == ePIPE		  = (ResourceVanished,
				     "broken pipe")
      | errno == ePROCLIM	  = (PermissionDenied,
				     "too many processes")
      | errno == ePROCUNAVAIL	  = (UnsupportedOperation,
				     "unimplemented RPC procedure")
      | errno == ePROGMISMATCH	  = (ProtocolError,
				     "unsupported RPC program version")
      | errno == ePROGUNAVAIL	  = (UnsupportedOperation,
				     "RPC program unavailable")
      | errno == ePROTO		  = (ProtocolError,
				     "error in streams protocol")
      | errno == ePROTONOSUPPORT  = (ProtocolError,
				     "protocol not supported")
      | errno == ePROTOTYPE	  = (ProtocolError,
				     "wrong protocol for socket")
      | errno == eRANGE		  = (UnsupportedOperation,
				     "result too large")
      | errno == eREMCHG	  = (ResourceVanished,
				     "remote address changed")
      | errno == eREMOTE	  = (IllegalOperation,
				     "too many levels of remote in path")
      | errno == eROFS		  = (PermissionDenied,
				     "read-only file system")
      | errno == eRPCMISMATCH	  = (ProtocolError,
				     "RPC version is wrong")
      | errno == eRREMOTE	  = (IllegalOperation,
				     "object is remote")
      | errno == eSHUTDOWN        = (IllegalOperation,
				     "can't send after socket shutdown")
      | errno == eSOCKTNOSUPPORT  = (UnsupportedOperation,
				     "socket type not supported")
      | errno == eSPIPE		  = (UnsupportedOperation,
				     "can't seek on a pipe")
      | errno == eSRCH		  = (NoSuchThing,
				     "no such process")
      | errno == eSRMNT		  = (UnsatisfiedConstraints,
				     "RFS resources still mounted by " ++
				     "remote host(s)")
				     -- no multiline strings with cpp
      | errno == eSTALE		  = (ResourceVanished,
				     "stale NFS file handle")
      | errno == eTIME		  = (TimeExpired,
				     "timer expired")
      | errno == eTIMEDOUT	  = (TimeExpired,
				     "connection timed out")
      | errno == eTOOMANYREFS	  = (ResourceExhausted,
				     "too many references; can't splice")
      | errno == eTXTBSY	  = (ResourceBusy,
				     "text file in-use")
      | errno == eUSERS		  = (ResourceExhausted,
				     "quota table full")
      | errno == eWOULDBLOCK	  = (OtherError,
				     "operation would block")
      | errno == eXDEV		  = (UnsupportedOperation,
				     "can't make a cross-device link")
      | otherwise                 = (OtherError, 
				     "unexpected error (error code: " 
				     ++ show no ++")")
\end{code}
