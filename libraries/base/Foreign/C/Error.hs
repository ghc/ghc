{-# OPTIONS -fno-implicit-prelude -#include "HsCore.h" #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Error.hs,v 1.2 2001/07/03 11:37:50 simonmar Exp $
--
-- C-specific Marshalling support: Handling of C "errno" error codes
--
-----------------------------------------------------------------------------

module Foreign.C.Error (

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
  throwErrnoIfNullRetry,-- ::                String -> IO (Ptr a) -> IO (Ptr a)

  throwErrnoIfRetryMayBlock, 
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,  
  throwErrnoIfNullRetryMayBlock
) where


-- this is were we get the CCONST_XXX definitions from that configure
-- calculated for us
--
#include "config.h"

-- system dependent imports
-- ------------------------

-- GHC allows us to get at the guts inside IO errors/exceptions
--
#if __GLASGOW_HASKELL__
import GHC.IOBase (Exception(..), IOException(..), IOErrorType(..))
#endif /* __GLASGOW_HASKELL__ */


-- regular imports
-- ---------------

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Error 	( void )
import Data.Maybe

#if __GLASGOW_HASKELL__
import GHC.Storable
import GHC.IOBase
import GHC.Num
import GHC.Base
#else
import System.IO		( IOError, Handle, ioError )
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
getErrno  = do e <- peek _errno; return (Errno e)

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

-- as `throwErrnoIfRetry', but checks for operations that would block and
-- executes an alternative action in that case.

throwErrnoIfRetryMayBlock  :: (a -> Bool) -> String -> IO a -> IO b -> IO a
throwErrnoIfRetryMayBlock pred loc f on_block  = 
  do
    res <- f
    if pred res
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
	         then do on_block; throwErrnoIfRetryMayBlock pred loc f on_block
                 else throwErrno loc
      else return res

-- as `throwErrnoIfRetry', but discards the result
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- as `throwErrnoIfRetryMayBlock', but discards the result
--
throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ pred loc f on_block 
  = void $ throwErrnoIfRetryMayBlock pred loc f on_block

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

-- as throwErrnoIfMinus1Retry, but checks for operations that would block
--
throwErrnoIfMinus1RetryMayBlock :: Num a => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== -1)

-- as `throwErrnoIfMinus1RetryMayBlock', but discards the result
--
throwErrnoIfMinus1RetryMayBlock_ :: Num a => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_  = throwErrnoIfRetryMayBlock_ (== -1)

-- throws "errno" if a result of a NULL pointer is returned
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- throws "errno" if a result of a NULL pointer is returned, but retries in
-- case of an interrupted operation
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)

-- as throwErrnoIfNullRetry, but checks for operations that would block
--
throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock  = throwErrnoIfRetryMayBlock (== nullPtr)

-- conversion of an "errno" value into IO error
-- --------------------------------------------

-- convert a location string, an "errno" value, an optional handle,
-- and an optional filename into a matching IO error
--
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError loc errno maybeHdl maybeName = unsafePerformIO $ do
    str <- strerror errno >>= peekCString
#if __GLASGOW_HASKELL__
    return (IOException (IOError maybeHdl errType loc str maybeName))
    where
    errType
        | errno == eOK             = OtherError
        | errno == e2BIG           = ResourceExhausted
        | errno == eACCES          = PermissionDenied
        | errno == eADDRINUSE      = ResourceBusy
        | errno == eADDRNOTAVAIL   = UnsupportedOperation
        | errno == eADV            = OtherError
        | errno == eAFNOSUPPORT    = UnsupportedOperation
        | errno == eAGAIN          = ResourceExhausted
        | errno == eALREADY        = AlreadyExists
        | errno == eBADF           = OtherError
        | errno == eBADMSG         = InappropriateType
        | errno == eBADRPC         = OtherError
        | errno == eBUSY           = ResourceBusy
        | errno == eCHILD          = NoSuchThing
        | errno == eCOMM           = ResourceVanished
        | errno == eCONNABORTED    = OtherError
        | errno == eCONNREFUSED    = NoSuchThing
        | errno == eCONNRESET      = ResourceVanished
        | errno == eDEADLK         = ResourceBusy
        | errno == eDESTADDRREQ    = InvalidArgument
        | errno == eDIRTY          = UnsatisfiedConstraints
        | errno == eDOM            = InvalidArgument
        | errno == eDQUOT          = PermissionDenied
        | errno == eEXIST          = AlreadyExists
        | errno == eFAULT          = OtherError
        | errno == eFBIG           = PermissionDenied
        | errno == eFTYPE          = InappropriateType
        | errno == eHOSTDOWN       = NoSuchThing
        | errno == eHOSTUNREACH    = NoSuchThing
        | errno == eIDRM           = ResourceVanished
        | errno == eILSEQ          = InvalidArgument
        | errno == eINPROGRESS     = AlreadyExists
        | errno == eINTR           = Interrupted
        | errno == eINVAL          = InvalidArgument
        | errno == eIO             = HardwareFault
        | errno == eISCONN         = AlreadyExists
        | errno == eISDIR          = InappropriateType
        | errno == eLOOP           = InvalidArgument
        | errno == eMFILE          = ResourceExhausted
        | errno == eMLINK          = ResourceExhausted
        | errno == eMSGSIZE        = ResourceExhausted
        | errno == eMULTIHOP       = UnsupportedOperation
        | errno == eNAMETOOLONG    = InvalidArgument
        | errno == eNETDOWN        = ResourceVanished
        | errno == eNETRESET       = ResourceVanished
        | errno == eNETUNREACH     = NoSuchThing
        | errno == eNFILE          = ResourceExhausted
        | errno == eNOBUFS         = ResourceExhausted
        | errno == eNODATA         = NoSuchThing
        | errno == eNODEV          = NoSuchThing
        | errno == eNOENT          = NoSuchThing
        | errno == eNOEXEC         = InvalidArgument
        | errno == eNOLCK          = ResourceExhausted
        | errno == eNOLINK         = ResourceVanished
        | errno == eNOMEM          = ResourceExhausted
        | errno == eNOMSG          = NoSuchThing
        | errno == eNONET          = NoSuchThing
        | errno == eNOPROTOOPT     = UnsupportedOperation
        | errno == eNOSPC          = ResourceExhausted
        | errno == eNOSR           = ResourceExhausted
        | errno == eNOSTR          = InvalidArgument
        | errno == eNOSYS          = UnsupportedOperation
        | errno == eNOTBLK         = InvalidArgument
        | errno == eNOTCONN        = InvalidArgument
        | errno == eNOTDIR         = InappropriateType
        | errno == eNOTEMPTY       = UnsatisfiedConstraints
        | errno == eNOTSOCK        = InvalidArgument
        | errno == eNOTTY          = IllegalOperation
        | errno == eNXIO           = NoSuchThing
        | errno == eOPNOTSUPP      = UnsupportedOperation
        | errno == ePERM           = PermissionDenied
        | errno == ePFNOSUPPORT    = UnsupportedOperation
        | errno == ePIPE           = ResourceVanished
        | errno == ePROCLIM        = PermissionDenied
        | errno == ePROCUNAVAIL    = UnsupportedOperation
        | errno == ePROGMISMATCH   = ProtocolError
        | errno == ePROGUNAVAIL    = UnsupportedOperation
        | errno == ePROTO          = ProtocolError
        | errno == ePROTONOSUPPORT = ProtocolError
        | errno == ePROTOTYPE      = ProtocolError
        | errno == eRANGE          = UnsupportedOperation
        | errno == eREMCHG         = ResourceVanished
        | errno == eREMOTE         = IllegalOperation
        | errno == eROFS           = PermissionDenied
        | errno == eRPCMISMATCH    = ProtocolError
        | errno == eRREMOTE        = IllegalOperation
        | errno == eSHUTDOWN       = IllegalOperation
        | errno == eSOCKTNOSUPPORT = UnsupportedOperation
        | errno == eSPIPE          = UnsupportedOperation
        | errno == eSRCH           = NoSuchThing
        | errno == eSRMNT          = UnsatisfiedConstraints
        | errno == eSTALE          = ResourceVanished
        | errno == eTIME           = TimeExpired
        | errno == eTIMEDOUT       = TimeExpired
        | errno == eTOOMANYREFS    = ResourceExhausted
        | errno == eTXTBSY         = ResourceBusy
        | errno == eUSERS          = ResourceExhausted
        | errno == eWOULDBLOCK     = OtherError
        | errno == eXDEV           = UnsupportedOperation
        | otherwise                = OtherError
#else
    return (userError (loc ++ ": " ++ str ++ maybe "" (": "++) maybeName))
#endif

foreign import unsafe strerror :: Errno -> IO (Ptr CChar)
