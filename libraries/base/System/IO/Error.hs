{-# OPTIONS -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- 
-- Module      :  System.IO.Error
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Error.hs,v 1.1 2002/02/05 17:32:27 simonmar Exp $
--
-- Standard IO Errors.
--
-----------------------------------------------------------------------------

module System.IO.Error (
    IOError,			-- abstract
    IOErrorType,		-- abstract

    ioError,		       	-- :: IOError -> IO a
    userError,		       	-- :: String  -> IOError

    mkIOError,			-- :: IOErrorType -> String -> Maybe Handle
				--    -> Maybe FilePath -> IOError

    alreadyExistsErrorType,	-- :: IOErrorType
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType, 
    permissionErrorType,
    userErrorType,

    isAlreadyExistsErrorType,	-- :: IOErrorType -> Bool
    isDoesNotExistErrorType,
    isAlreadyInUseErrorType,
    isFullErrorType, 
    isEOFErrorType,
    isIllegalOperationErrorType, 
    isPermissionErrorType,
    isUserErrorType, 

    isAlreadyExistsError,	-- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError, 
    isEOFError,
    isIllegalOperation, 
    isPermissionError,
    isUserError,

    ioeGetErrorType,		-- :: IOError -> IOErrorType
    ioeGetErrorString,		-- :: IOError -> String
    ioeGetHandle,		-- :: IOError -> Maybe Handle
    ioeGetFileName,		-- :: IOError -> Maybe FilePath

  ) where


#ifdef __GLASGOW_HASKELL__
import GHC.Base
import Data.Maybe
import GHC.IOBase
import Text.Show
#endif

-- -----------------------------------------------------------------------------
-- Constructing an IOError

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError t location maybe_hdl maybe_filename =
   IOException IOError{ ioe_type = t, 
			ioe_location = location,
	   		ioe_descr = "",
			ioe_handle = maybe_hdl, 
			ioe_filename = maybe_filename
 			}

-- -----------------------------------------------------------------------------
-- IOErrorType

isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError,
 isFullError, isEOFError, isIllegalOperation, isPermissionError,
 isUserError :: IOError -> Bool

isAlreadyExistsError = isAlreadyExistsErrorType    . ioeGetErrorType
isDoesNotExistError  = isDoesNotExistErrorType     . ioeGetErrorType
isAlreadyInUseError  = isAlreadyInUseErrorType     . ioeGetErrorType
isFullError          = isFullErrorType             . ioeGetErrorType
isEOFError           = isEOFErrorType              . ioeGetErrorType
isIllegalOperation   = isIllegalOperationErrorType . ioeGetErrorType
isPermissionError    = isPermissionErrorType       . ioeGetErrorType
isUserError          = isUserErrorType             . ioeGetErrorType

-- -----------------------------------------------------------------------------
-- IOErrorTypes

#ifdef __GLASGOW_HASKELL__
alreadyExistsErrorType, doesNotExistErrorType, alreadyInUseErrorType,
 fullErrorType, eofErrorType, illegalOperationErrorType,
 permissionErrorType, userErrorType :: IOErrorType

alreadyExistsErrorType    = AlreadyExists
doesNotExistErrorType     = NoSuchThing
alreadyInUseErrorType     = ResourceBusy
fullErrorType             = ResourceExhausted
eofErrorType              = EOF
illegalOperationErrorType = IllegalOperation
permissionErrorType       = PermissionDenied
userErrorType		  = UserError
#endif

-- -----------------------------------------------------------------------------
-- IOErrorType predicates

#ifdef __GLASGOW_HASKELL__
isAlreadyExistsErrorType AlreadyExists = True
isAlreadyExistsErrorType _ = False

isDoesNotExistErrorType NoSuchThing = True
isDoesNotExistErrorType _ = False

isAlreadyInUseErrorType ResourceBusy = True
isAlreadyInUseErrorType _ = False

isFullErrorType ResourceExhausted = True
isFullErrorType _ = False

isEOFErrorType EOF = True
isEOFErrorType _ = False

isIllegalOperationErrorType IllegalOperation = True
isIllegalOperationErrorType _ = False

isPermissionErrorType PermissionDenied = True
isPermissionErrorType _ = False

isUserErrorType UserError = True
isUserErrorType _ = False
#endif

-- -----------------------------------------------------------------------------
-- Miscellaneous

#ifdef __GLASGOW_HASKELL__
ioeGetErrorType	      :: IOError -> IOErrorType
ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetErrorString     :: IOError -> String
ioeGetHandle          :: IOError -> Maybe Handle

ioeGetErrorType (IOException ioe) = ioe_type ioe
ioeGetHandle _ = error "System.IO.Error.ioeGetHandle: not an IO error"

ioeGetHandle (IOException ioe) = ioe_handle ioe
ioeGetHandle _ = error "System.IO.Error.ioeGetHandle: not an IO error"

ioeGetErrorString (IOException ioe) 
   | isUserErrorType (ioe_type ioe) = show (ioe_descr ioe)
   | otherwise                      = show (ioe_type ioe)
ioeGetErrorString _ = error "System.IO.Error.ioeGetErrorString: not an IO error"

ioeGetFileName (IOException ioe) = ioe_filename ioe
ioeGetFileName _ = error "System.IO.Error.ioeGetFileName: not an IO error"
#endif

-- -----------------------------------------------------------------------------
-- annotating an IOError

#ifdef __GLASGOW_HASKELL__
annotateIOError :: IOError 
              -> String 
              -> Maybe FilePath 
              -> Maybe Handle 
              -> IOError 
annotateIOError (IOException (IOError hdl errTy _ str path)) loc opath ohdl = 
  IOException (IOError (hdl `mplus` ohdl) errTy loc str (path `mplus` opath))
  where
    Nothing `mplus` ys = ys
    xs      `mplus` _  = xs
annotateIOError exc _ _ _ = 
  exc
#endif
