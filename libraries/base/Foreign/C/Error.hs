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
-- C-specific Marshalling support: Handling of C \"errno\" error codes
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


-- this is were we get the CONST_XXX definitions from that configure
-- calculated for us
--
#ifndef __NHC__
#include "config.h"
#endif

-- system dependent imports
-- ------------------------

-- GHC allows us to get at the guts inside IO errors/exceptions
--
#if __GLASGOW_HASKELL__
import GHC.IOBase (IOException(..), IOErrorType(..))
#endif /* __GLASGOW_HASKELL__ */


-- regular imports
-- ---------------

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Error 	( void )
import Data.Maybe

#if __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Num
import GHC.Base
#else
import System.IO		( IOError, Handle, ioError )
import System.IO.Unsafe		( unsafePerformIO )
#endif

#ifdef __HUGS__
{-# CBITS errno.c #-}
#endif


-- "errno" type
-- ------------

-- import of C function that gives address of errno
-- This function exists because errno is a variable on some systems, but on
-- Windows it is a macro for a function...
-- [yes, global variables and thread safety don't really go hand-in-hand. -- sof]
#ifdef __NHC__
foreign import ccall unsafe "errno.h &errno" _errno :: Ptr CInt
#else
foreign import ccall unsafe "HsBase.h ghcErrno" _errno :: Ptr CInt
#endif

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
#ifdef __NHC__
#include "Errno.hs"
#else
e2BIG           = Errno (CONST_E2BIG)
eACCES		= Errno (CONST_EACCES)
eADDRINUSE	= Errno (CONST_EADDRINUSE)
eADDRNOTAVAIL	= Errno (CONST_EADDRNOTAVAIL)
eADV		= Errno (CONST_EADV)
eAFNOSUPPORT	= Errno (CONST_EAFNOSUPPORT)
eAGAIN		= Errno (CONST_EAGAIN)
eALREADY	= Errno (CONST_EALREADY)
eBADF		= Errno (CONST_EBADF)
eBADMSG		= Errno (CONST_EBADMSG)
eBADRPC		= Errno (CONST_EBADRPC)
eBUSY		= Errno (CONST_EBUSY)
eCHILD		= Errno (CONST_ECHILD)
eCOMM		= Errno (CONST_ECOMM)
eCONNABORTED	= Errno (CONST_ECONNABORTED)
eCONNREFUSED	= Errno (CONST_ECONNREFUSED)
eCONNRESET	= Errno (CONST_ECONNRESET)
eDEADLK		= Errno (CONST_EDEADLK)
eDESTADDRREQ	= Errno (CONST_EDESTADDRREQ)
eDIRTY		= Errno (CONST_EDIRTY)
eDOM		= Errno (CONST_EDOM)
eDQUOT		= Errno (CONST_EDQUOT)
eEXIST		= Errno (CONST_EEXIST)
eFAULT		= Errno (CONST_EFAULT)
eFBIG		= Errno (CONST_EFBIG)
eFTYPE		= Errno (CONST_EFTYPE)
eHOSTDOWN	= Errno (CONST_EHOSTDOWN)
eHOSTUNREACH	= Errno (CONST_EHOSTUNREACH)
eIDRM		= Errno (CONST_EIDRM)
eILSEQ		= Errno (CONST_EILSEQ)
eINPROGRESS	= Errno (CONST_EINPROGRESS)
eINTR		= Errno (CONST_EINTR)
eINVAL		= Errno (CONST_EINVAL)
eIO		= Errno (CONST_EIO)
eISCONN		= Errno (CONST_EISCONN)
eISDIR		= Errno (CONST_EISDIR)
eLOOP		= Errno (CONST_ELOOP)
eMFILE		= Errno (CONST_EMFILE)
eMLINK		= Errno (CONST_EMLINK)
eMSGSIZE	= Errno (CONST_EMSGSIZE)
eMULTIHOP	= Errno (CONST_EMULTIHOP)
eNAMETOOLONG	= Errno (CONST_ENAMETOOLONG)
eNETDOWN	= Errno (CONST_ENETDOWN)
eNETRESET	= Errno (CONST_ENETRESET)
eNETUNREACH	= Errno (CONST_ENETUNREACH)
eNFILE		= Errno (CONST_ENFILE)
eNOBUFS		= Errno (CONST_ENOBUFS)
eNODATA		= Errno (CONST_ENODATA)
eNODEV		= Errno (CONST_ENODEV)
eNOENT		= Errno (CONST_ENOENT)
eNOEXEC		= Errno (CONST_ENOEXEC)
eNOLCK		= Errno (CONST_ENOLCK)
eNOLINK		= Errno (CONST_ENOLINK)
eNOMEM		= Errno (CONST_ENOMEM)
eNOMSG		= Errno (CONST_ENOMSG)
eNONET		= Errno (CONST_ENONET)
eNOPROTOOPT	= Errno (CONST_ENOPROTOOPT)
eNOSPC		= Errno (CONST_ENOSPC)
eNOSR		= Errno (CONST_ENOSR)
eNOSTR		= Errno (CONST_ENOSTR)
eNOSYS		= Errno (CONST_ENOSYS)
eNOTBLK		= Errno (CONST_ENOTBLK)
eNOTCONN	= Errno (CONST_ENOTCONN)
eNOTDIR		= Errno (CONST_ENOTDIR)
eNOTEMPTY	= Errno (CONST_ENOTEMPTY)
eNOTSOCK	= Errno (CONST_ENOTSOCK)
eNOTTY		= Errno (CONST_ENOTTY)
eNXIO		= Errno (CONST_ENXIO)
eOPNOTSUPP	= Errno (CONST_EOPNOTSUPP)
ePERM		= Errno (CONST_EPERM)
ePFNOSUPPORT	= Errno (CONST_EPFNOSUPPORT)
ePIPE		= Errno (CONST_EPIPE)
ePROCLIM	= Errno (CONST_EPROCLIM)
ePROCUNAVAIL	= Errno (CONST_EPROCUNAVAIL)
ePROGMISMATCH	= Errno (CONST_EPROGMISMATCH)
ePROGUNAVAIL	= Errno (CONST_EPROGUNAVAIL)
ePROTO		= Errno (CONST_EPROTO)
ePROTONOSUPPORT = Errno (CONST_EPROTONOSUPPORT)
ePROTOTYPE	= Errno (CONST_EPROTOTYPE)
eRANGE		= Errno (CONST_ERANGE)
eREMCHG		= Errno (CONST_EREMCHG)
eREMOTE		= Errno (CONST_EREMOTE)
eROFS		= Errno (CONST_EROFS)
eRPCMISMATCH	= Errno (CONST_ERPCMISMATCH)
eRREMOTE	= Errno (CONST_ERREMOTE)
eSHUTDOWN	= Errno (CONST_ESHUTDOWN)
eSOCKTNOSUPPORT = Errno (CONST_ESOCKTNOSUPPORT)
eSPIPE		= Errno (CONST_ESPIPE)
eSRCH		= Errno (CONST_ESRCH)
eSRMNT		= Errno (CONST_ESRMNT)
eSTALE		= Errno (CONST_ESTALE)
eTIME		= Errno (CONST_ETIME)
eTIMEDOUT	= Errno (CONST_ETIMEDOUT)
eTOOMANYREFS	= Errno (CONST_ETOOMANYREFS)
eTXTBSY		= Errno (CONST_ETXTBSY)
eUSERS		= Errno (CONST_EUSERS)
eWOULDBLOCK	= Errno (CONST_EWOULDBLOCK)
eXDEV		= Errno (CONST_EXDEV)
#endif

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
    return (IOError maybeHdl errType loc str maybeName)
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
