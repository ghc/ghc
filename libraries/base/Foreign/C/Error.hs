{-# OPTIONS -fno-implicit-prelude -#include "HsBase.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
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

import System.IO.Unsafe( unsafePerformIO )
import Foreign.Storable
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
-- This function exists because errno is a variable on some systems, but on
-- Windows it is a macro for a function...
-- [yes, global variables and thread safety don't really go hand-in-hand. -- sof]
foreign import ccall unsafe "HsBase.h ghcErrno" _errno :: Ptr CInt

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
-- the cCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure 
--
eOK             = Errno 0
e2BIG           = Errno (cCONST_E2BIG)
eACCES		= Errno (cCONST_EACCES)
eADDRINUSE	= Errno (cCONST_EADDRINUSE)
eADDRNOTAVAIL	= Errno (cCONST_EADDRNOTAVAIL)
eADV		= Errno (cCONST_EADV)
eAFNOSUPPORT	= Errno (cCONST_EAFNOSUPPORT)
eAGAIN		= Errno (cCONST_EAGAIN)
eALREADY	= Errno (cCONST_EALREADY)
eBADF		= Errno (cCONST_EBADF)
eBADMSG		= Errno (cCONST_EBADMSG)
eBADRPC		= Errno (cCONST_EBADRPC)
eBUSY		= Errno (cCONST_EBUSY)
eCHILD		= Errno (cCONST_ECHILD)
eCOMM		= Errno (cCONST_ECOMM)
eCONNABORTED	= Errno (cCONST_ECONNABORTED)
eCONNREFUSED	= Errno (cCONST_ECONNREFUSED)
eCONNRESET	= Errno (cCONST_ECONNRESET)
eDEADLK		= Errno (cCONST_EDEADLK)
eDESTADDRREQ	= Errno (cCONST_EDESTADDRREQ)
eDIRTY		= Errno (cCONST_EDIRTY)
eDOM		= Errno (cCONST_EDOM)
eDQUOT		= Errno (cCONST_EDQUOT)
eEXIST		= Errno (cCONST_EEXIST)
eFAULT		= Errno (cCONST_EFAULT)
eFBIG		= Errno (cCONST_EFBIG)
eFTYPE		= Errno (cCONST_EFTYPE)
eHOSTDOWN	= Errno (cCONST_EHOSTDOWN)
eHOSTUNREACH	= Errno (cCONST_EHOSTUNREACH)
eIDRM		= Errno (cCONST_EIDRM)
eILSEQ		= Errno (cCONST_EILSEQ)
eINPROGRESS	= Errno (cCONST_EINPROGRESS)
eINTR		= Errno (cCONST_EINTR)
eINVAL		= Errno (cCONST_EINVAL)
eIO		= Errno (cCONST_EIO)
eISCONN		= Errno (cCONST_EISCONN)
eISDIR		= Errno (cCONST_EISDIR)
eLOOP		= Errno (cCONST_ELOOP)
eMFILE		= Errno (cCONST_EMFILE)
eMLINK		= Errno (cCONST_EMLINK)
eMSGSIZE	= Errno (cCONST_EMSGSIZE)
eMULTIHOP	= Errno (cCONST_EMULTIHOP)
eNAMETOOLONG	= Errno (cCONST_ENAMETOOLONG)
eNETDOWN	= Errno (cCONST_ENETDOWN)
eNETRESET	= Errno (cCONST_ENETRESET)
eNETUNREACH	= Errno (cCONST_ENETUNREACH)
eNFILE		= Errno (cCONST_ENFILE)
eNOBUFS		= Errno (cCONST_ENOBUFS)
eNODATA		= Errno (cCONST_ENODATA)
eNODEV		= Errno (cCONST_ENODEV)
eNOENT		= Errno (cCONST_ENOENT)
eNOEXEC		= Errno (cCONST_ENOEXEC)
eNOLCK		= Errno (cCONST_ENOLCK)
eNOLINK		= Errno (cCONST_ENOLINK)
eNOMEM		= Errno (cCONST_ENOMEM)
eNOMSG		= Errno (cCONST_ENOMSG)
eNONET		= Errno (cCONST_ENONET)
eNOPROTOOPT	= Errno (cCONST_ENOPROTOOPT)
eNOSPC		= Errno (cCONST_ENOSPC)
eNOSR		= Errno (cCONST_ENOSR)
eNOSTR		= Errno (cCONST_ENOSTR)
eNOSYS		= Errno (cCONST_ENOSYS)
eNOTBLK		= Errno (cCONST_ENOTBLK)
eNOTCONN	= Errno (cCONST_ENOTCONN)
eNOTDIR		= Errno (cCONST_ENOTDIR)
eNOTEMPTY	= Errno (cCONST_ENOTEMPTY)
eNOTSOCK	= Errno (cCONST_ENOTSOCK)
eNOTTY		= Errno (cCONST_ENOTTY)
eNXIO		= Errno (cCONST_ENXIO)
eOPNOTSUPP	= Errno (cCONST_EOPNOTSUPP)
ePERM		= Errno (cCONST_EPERM)
ePFNOSUPPORT	= Errno (cCONST_EPFNOSUPPORT)
ePIPE		= Errno (cCONST_EPIPE)
ePROCLIM	= Errno (cCONST_EPROCLIM)
ePROCUNAVAIL	= Errno (cCONST_EPROCUNAVAIL)
ePROGMISMATCH	= Errno (cCONST_EPROGMISMATCH)
ePROGUNAVAIL	= Errno (cCONST_EPROGUNAVAIL)
ePROTO		= Errno (cCONST_EPROTO)
ePROTONOSUPPORT = Errno (cCONST_EPROTONOSUPPORT)
ePROTOTYPE	= Errno (cCONST_EPROTOTYPE)
eRANGE		= Errno (cCONST_ERANGE)
eREMCHG		= Errno (cCONST_EREMCHG)
eREMOTE		= Errno (cCONST_EREMOTE)
eROFS		= Errno (cCONST_EROFS)
eRPCMISMATCH	= Errno (cCONST_ERPCMISMATCH)
eRREMOTE	= Errno (cCONST_ERREMOTE)
eSHUTDOWN	= Errno (cCONST_ESHUTDOWN)
eSOCKTNOSUPPORT = Errno (cCONST_ESOCKTNOSUPPORT)
eSPIPE		= Errno (cCONST_ESPIPE)
eSRCH		= Errno (cCONST_ESRCH)
eSRMNT		= Errno (cCONST_ESRMNT)
eSTALE		= Errno (cCONST_ESTALE)
eTIME		= Errno (cCONST_ETIME)
eTIMEDOUT	= Errno (cCONST_ETIMEDOUT)
eTOOMANYREFS	= Errno (cCONST_ETOOMANYREFS)
eTXTBSY		= Errno (cCONST_ETXTBSY)
eUSERS		= Errno (cCONST_EUSERS)
eWOULDBLOCK	= Errno (cCONST_EWOULDBLOCK)
eXDEV		= Errno (cCONST_EXDEV)

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
        | errno == eNODEV          = UnsupportedOperation
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

foreign import ccall unsafe "string.h" strerror :: Errno -> IO (Ptr CChar)


-- Dreadfully tedious callouts to wrappers which define  the
-- actual values for the error codes.
foreign import ccall unsafe "prel_error_E2BIG" cCONST_E2BIG :: CInt
foreign import ccall unsafe "prel_error_EACCES" cCONST_EACCES :: CInt
foreign import ccall unsafe "prel_error_EADDRINUSE" cCONST_EADDRINUSE :: CInt
foreign import ccall unsafe "prel_error_EADDRNOTAVAIL" cCONST_EADDRNOTAVAIL :: CInt
foreign import ccall unsafe "prel_error_EADV" cCONST_EADV :: CInt
foreign import ccall unsafe "prel_error_EAFNOSUPPORT" cCONST_EAFNOSUPPORT :: CInt
foreign import ccall unsafe "prel_error_EAGAIN" cCONST_EAGAIN :: CInt
foreign import ccall unsafe "prel_error_EALREADY" cCONST_EALREADY :: CInt
foreign import ccall unsafe "prel_error_EBADF" cCONST_EBADF :: CInt
foreign import ccall unsafe "prel_error_EBADMSG" cCONST_EBADMSG :: CInt
foreign import ccall unsafe "prel_error_EBADRPC" cCONST_EBADRPC :: CInt
foreign import ccall unsafe "prel_error_EBUSY" cCONST_EBUSY :: CInt
foreign import ccall unsafe "prel_error_ECHILD" cCONST_ECHILD :: CInt
foreign import ccall unsafe "prel_error_ECOMM" cCONST_ECOMM :: CInt
foreign import ccall unsafe "prel_error_ECONNABORTED" cCONST_ECONNABORTED :: CInt
foreign import ccall unsafe "prel_error_ECONNREFUSED" cCONST_ECONNREFUSED :: CInt
foreign import ccall unsafe "prel_error_ECONNRESET" cCONST_ECONNRESET :: CInt
foreign import ccall unsafe "prel_error_EDEADLK" cCONST_EDEADLK :: CInt
foreign import ccall unsafe "prel_error_EDESTADDRREQ" cCONST_EDESTADDRREQ :: CInt
foreign import ccall unsafe "prel_error_EDIRTY" cCONST_EDIRTY :: CInt
foreign import ccall unsafe "prel_error_EDOM" cCONST_EDOM :: CInt
foreign import ccall unsafe "prel_error_EDQUOT" cCONST_EDQUOT :: CInt
foreign import ccall unsafe "prel_error_EEXIST" cCONST_EEXIST :: CInt
foreign import ccall unsafe "prel_error_EFAULT" cCONST_EFAULT :: CInt
foreign import ccall unsafe "prel_error_EFBIG" cCONST_EFBIG :: CInt
foreign import ccall unsafe "prel_error_EFTYPE" cCONST_EFTYPE :: CInt
foreign import ccall unsafe "prel_error_EHOSTDOWN" cCONST_EHOSTDOWN :: CInt
foreign import ccall unsafe "prel_error_EHOSTUNREACH" cCONST_EHOSTUNREACH :: CInt
foreign import ccall unsafe "prel_error_EIDRM" cCONST_EIDRM :: CInt
foreign import ccall unsafe "prel_error_EILSEQ" cCONST_EILSEQ :: CInt
foreign import ccall unsafe "prel_error_EINPROGRESS" cCONST_EINPROGRESS :: CInt
foreign import ccall unsafe "prel_error_EINTR" cCONST_EINTR :: CInt
foreign import ccall unsafe "prel_error_EINVAL" cCONST_EINVAL :: CInt
foreign import ccall unsafe "prel_error_EIO" cCONST_EIO :: CInt
foreign import ccall unsafe "prel_error_EISCONN" cCONST_EISCONN :: CInt
foreign import ccall unsafe "prel_error_EISDIR" cCONST_EISDIR :: CInt
foreign import ccall unsafe "prel_error_ELOOP" cCONST_ELOOP :: CInt
foreign import ccall unsafe "prel_error_EMFILE" cCONST_EMFILE :: CInt
foreign import ccall unsafe "prel_error_EMLINK" cCONST_EMLINK :: CInt
foreign import ccall unsafe "prel_error_EMSGSIZE" cCONST_EMSGSIZE :: CInt
foreign import ccall unsafe "prel_error_EMULTIHOP" cCONST_EMULTIHOP :: CInt
foreign import ccall unsafe "prel_error_ENAMETOOLONG" cCONST_ENAMETOOLONG :: CInt
foreign import ccall unsafe "prel_error_ENETDOWN" cCONST_ENETDOWN :: CInt
foreign import ccall unsafe "prel_error_ENETRESET" cCONST_ENETRESET :: CInt
foreign import ccall unsafe "prel_error_ENETUNREACH" cCONST_ENETUNREACH :: CInt
foreign import ccall unsafe "prel_error_ENFILE" cCONST_ENFILE :: CInt
foreign import ccall unsafe "prel_error_ENOBUFS" cCONST_ENOBUFS :: CInt
foreign import ccall unsafe "prel_error_ENODATA" cCONST_ENODATA :: CInt
foreign import ccall unsafe "prel_error_ENODEV" cCONST_ENODEV :: CInt
foreign import ccall unsafe "prel_error_ENOENT" cCONST_ENOENT :: CInt
foreign import ccall unsafe "prel_error_ENOEXEC" cCONST_ENOEXEC :: CInt
foreign import ccall unsafe "prel_error_ENOLCK" cCONST_ENOLCK :: CInt
foreign import ccall unsafe "prel_error_ENOLINK" cCONST_ENOLINK :: CInt
foreign import ccall unsafe "prel_error_ENOMEM" cCONST_ENOMEM :: CInt
foreign import ccall unsafe "prel_error_ENOMSG" cCONST_ENOMSG :: CInt
foreign import ccall unsafe "prel_error_ENONET" cCONST_ENONET :: CInt
foreign import ccall unsafe "prel_error_ENOPROTOOPT" cCONST_ENOPROTOOPT :: CInt
foreign import ccall unsafe "prel_error_ENOSPC" cCONST_ENOSPC :: CInt
foreign import ccall unsafe "prel_error_ENOSR" cCONST_ENOSR :: CInt
foreign import ccall unsafe "prel_error_ENOSTR" cCONST_ENOSTR :: CInt
foreign import ccall unsafe "prel_error_ENOSYS" cCONST_ENOSYS :: CInt
foreign import ccall unsafe "prel_error_ENOTBLK" cCONST_ENOTBLK :: CInt
foreign import ccall unsafe "prel_error_ENOTCONN" cCONST_ENOTCONN :: CInt
foreign import ccall unsafe "prel_error_ENOTDIR" cCONST_ENOTDIR :: CInt
foreign import ccall unsafe "prel_error_ENOTEMPTY" cCONST_ENOTEMPTY :: CInt
foreign import ccall unsafe "prel_error_ENOTSOCK" cCONST_ENOTSOCK :: CInt
foreign import ccall unsafe "prel_error_ENOTTY" cCONST_ENOTTY :: CInt
foreign import ccall unsafe "prel_error_ENXIO" cCONST_ENXIO :: CInt
foreign import ccall unsafe "prel_error_EOPNOTSUPP" cCONST_EOPNOTSUPP :: CInt
foreign import ccall unsafe "prel_error_EPERM" cCONST_EPERM :: CInt
foreign import ccall unsafe "prel_error_EPFNOSUPPORT" cCONST_EPFNOSUPPORT :: CInt
foreign import ccall unsafe "prel_error_EPIPE" cCONST_EPIPE :: CInt
foreign import ccall unsafe "prel_error_EPROCLIM" cCONST_EPROCLIM :: CInt
foreign import ccall unsafe "prel_error_EPROCUNAVAIL" cCONST_EPROCUNAVAIL :: CInt
foreign import ccall unsafe "prel_error_EPROGMISMATCH" cCONST_EPROGMISMATCH :: CInt
foreign import ccall unsafe "prel_error_EPROGUNAVAIL" cCONST_EPROGUNAVAIL :: CInt
foreign import ccall unsafe "prel_error_EPROTO" cCONST_EPROTO :: CInt
foreign import ccall unsafe "prel_error_EPROTONOSUPPORT" cCONST_EPROTONOSUPPORT :: CInt
foreign import ccall unsafe "prel_error_EPROTOTYPE" cCONST_EPROTOTYPE :: CInt
foreign import ccall unsafe "prel_error_ERANGE" cCONST_ERANGE :: CInt
foreign import ccall unsafe "prel_error_EREMCHG" cCONST_EREMCHG :: CInt
foreign import ccall unsafe "prel_error_EREMOTE" cCONST_EREMOTE :: CInt
foreign import ccall unsafe "prel_error_EROFS" cCONST_EROFS :: CInt
foreign import ccall unsafe "prel_error_ERPCMISMATCH" cCONST_ERPCMISMATCH :: CInt
foreign import ccall unsafe "prel_error_ERREMOTE" cCONST_ERREMOTE :: CInt
foreign import ccall unsafe "prel_error_ESHUTDOWN" cCONST_ESHUTDOWN :: CInt
foreign import ccall unsafe "prel_error_ESOCKTNOSUPPORT" cCONST_ESOCKTNOSUPPORT :: CInt
foreign import ccall unsafe "prel_error_ESPIPE" cCONST_ESPIPE :: CInt
foreign import ccall unsafe "prel_error_ESRCH" cCONST_ESRCH :: CInt
foreign import ccall unsafe "prel_error_ESRMNT" cCONST_ESRMNT :: CInt
foreign import ccall unsafe "prel_error_ESTALE" cCONST_ESTALE :: CInt
foreign import ccall unsafe "prel_error_ETIME" cCONST_ETIME :: CInt
foreign import ccall unsafe "prel_error_ETIMEDOUT" cCONST_ETIMEDOUT :: CInt
foreign import ccall unsafe "prel_error_ETOOMANYREFS" cCONST_ETOOMANYREFS :: CInt
foreign import ccall unsafe "prel_error_ETXTBSY" cCONST_ETXTBSY :: CInt
foreign import ccall unsafe "prel_error_EUSERS" cCONST_EUSERS :: CInt
foreign import ccall unsafe "prel_error_EWOULDBLOCK" cCONST_EWOULDBLOCK :: CInt
foreign import ccall unsafe "prel_error_EXDEV" cCONST_EXDEV :: CInt

