{-# OPTIONS -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Error
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Standard IO Errors.
--
-----------------------------------------------------------------------------

module System.IO.Error (
    IOError,			-- abstract
#ifdef __GLASGOW_HASKELL__
    IOErrorType,		-- abstract
#endif

    ioError,		       	-- :: IOError -> IO a
    userError,		       	-- :: String  -> IOError

#ifdef __GLASGOW_HASKELL__
    mkIOError,			-- :: IOErrorType -> String -> Maybe Handle
				--    -> Maybe FilePath -> IOError

    annotateIOError,		-- :: IOError -> String -> Maybe FilePath 
				--    -> Maybe Handle -> IOError 

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
#endif  /* __GLASGOW_HASKELL__ */

    isAlreadyExistsError,	-- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError, 
    isEOFError,
    isIllegalOperation, 
    isPermissionError,
    isUserError,

#ifdef __GLASGOW_HASKELL__
    ioeGetErrorType,		-- :: IOError -> IOErrorType
#endif
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

#ifdef __HUGS__
import Hugs.IO
#endif

#ifdef __NHC__
import IO
  ( IOError ()
  , ioError
  , userError
  , isAlreadyExistsError	-- :: IOError -> Bool
  , isDoesNotExistError
  , isAlreadyInUseError
  , isFullError
  , isEOFError
  , isIllegalOperation
  , isPermissionError
  , isUserError
  , ioeGetErrorString           -- :: IOError -> String
  , ioeGetHandle                -- :: IOError -> Maybe Handle
  , ioeGetFileName              -- :: IOError -> Maybe FilePath
  )
--import Data.Maybe (fromJust)
--import Control.Monad (MonadPlus(mplus))
#endif

#ifdef __GLASGOW_HASKELL__
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
#ifdef __NHC__
mkIOError EOF       location maybe_hdl maybe_filename =
    EOFError location (fromJust maybe_hdl)
mkIOError UserError location maybe_hdl maybe_filename =
    UserError location ""
mkIOError t         location maybe_hdl maybe_filename =
    NHC.FFI.mkIOError location maybe_filename maybe_handle (ioeTypeToInt t)
  where
    ioeTypeToInt AlreadyExists     = fromEnum EEXIST
    ioeTypeToInt NoSuchThing       = fromEnum ENOENT
    ioeTypeToInt ResourceBusy      = fromEnum EBUSY
    ioeTypeToInt ResourceExhausted = fromEnum ENOSPC
    ioeTypeToInt IllegalOperation  = fromEnum EPERM
    ioeTypeToInt PermissionDenied  = fromEnum EACCES
#endif

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
#endif

-- -----------------------------------------------------------------------------
-- IOErrorTypes

#ifdef __NHC__
data IOErrorType = AlreadyExists | NoSuchThing | ResourceBusy
		 | ResourceExhausted | EOF | IllegalOperation
		 | PermissionDenied | UserError
#endif

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
isAlreadyExistsErrorType, isDoesNotExistErrorType, isAlreadyInUseErrorType,
  isFullErrorType, isEOFErrorType, isIllegalOperationErrorType, 
  isPermissionErrorType, isUserErrorType :: IOErrorType -> Bool
#endif

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
ioeGetHandle          :: IOError -> Maybe Handle
ioeGetErrorString     :: IOError -> String
ioeGetFileName        :: IOError -> Maybe FilePath

ioeGetErrorType (IOException ioe) = ioe_type ioe
ioeGetErrorType _ = error "System.IO.Error.ioeGetErrorType: not an IO error"

ioeGetHandle (IOException ioe) = ioe_handle ioe
ioeGetHandle _ = error "System.IO.Error.ioeGetHandle: not an IO error"

ioeGetErrorString (IOException ioe) 
   | isUserErrorType (ioe_type ioe) = ioe_descr ioe
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

#ifdef 0 /*__NHC__*/
annotateIOError (IOError msg file hdl code) msg' file' hdl' =
    IOError (msg++'\n':msg') (file`mplus`file') (hdl`mplus`hdl') code
annotateIOError (EOFError msg hdl) msg' file' hdl' =
    EOFError (msg++'\n':msg') (hdl`mplus`hdl')
annotateIOError (UserError loc msg) msg' file' hdl' =
    UserError loc (msg++'\n':msg')
annotateIOError (PatternError loc) msg' file' hdl' =
    PatternError (loc++'\n':msg')
#endif
