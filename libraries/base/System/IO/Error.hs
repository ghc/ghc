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
    IOErrorType,		-- abstract

    catch,			-- :: IO a -> (IOError -> IO a) -> IO a
    try,			-- :: IO a -> IO (Either IOError a)

    ioError,		       	-- :: IOError -> IO a
    userError,		       	-- :: String  -> IOError

#ifndef __NHC__
    mkIOError,			-- :: IOErrorType -> String -> Maybe Handle
				--    -> Maybe FilePath -> IOError

    annotateIOError,		-- :: IOError -> String -> Maybe FilePath 
				--    -> Maybe Handle -> IOError 
#endif

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

#ifndef __NHC__
    ioeGetErrorType,		-- :: IOError -> IOErrorType
#endif
    ioeGetErrorString,		-- :: IOError -> String
    ioeGetHandle,		-- :: IOError -> Maybe Handle
    ioeGetFileName,		-- :: IOError -> Maybe FilePath

  ) where

import Data.Either
import Data.Maybe

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Exception
import Text.Show
#endif

#ifdef __HUGS__
import Hugs.Prelude(Handle, IOException(..), IOErrorType(..))
#endif

#ifdef __NHC__
import IO
  ( IOError ()
  , try
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

-- | The construct @try comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
-- Other exceptions are not caught by this variant;
-- to catch all exceptions, use @try@ from "Control.Exception".

#ifndef __NHC__
try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)
#endif

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
-- -----------------------------------------------------------------------------
-- Constructing an IOError

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError t location maybe_hdl maybe_filename =
               IOError{ ioe_type = t, 
			ioe_location = location,
	   		ioe_description = "",
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
#endif /* __GLASGOW_HASKELL__ || __HUGS__ */

#ifndef __NHC__
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
#endif /* __NHC__ */

-- -----------------------------------------------------------------------------
-- IOErrorTypes

#ifdef __NHC__
data IOErrorType = AlreadyExists | NoSuchThing | ResourceBusy
		 | ResourceExhausted | EOF | IllegalOperation
		 | PermissionDenied | UserError
#endif

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

-- -----------------------------------------------------------------------------
-- IOErrorType predicates

isAlreadyExistsErrorType, isDoesNotExistErrorType, isAlreadyInUseErrorType,
  isFullErrorType, isEOFErrorType, isIllegalOperationErrorType, 
  isPermissionErrorType, isUserErrorType :: IOErrorType -> Bool

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

-- -----------------------------------------------------------------------------
-- Miscellaneous

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
ioeGetErrorType	      :: IOError -> IOErrorType
ioeGetHandle          :: IOError -> Maybe Handle
ioeGetErrorString     :: IOError -> String
ioeGetFileName        :: IOError -> Maybe FilePath

ioeGetErrorType ioe = ioe_type ioe

ioeGetHandle ioe = ioe_handle ioe

ioeGetErrorString ioe
   | isUserErrorType (ioe_type ioe) = ioe_description ioe
   | otherwise                      = show (ioe_type ioe)

ioeGetFileName ioe = ioe_filename ioe

-- -----------------------------------------------------------------------------
-- annotating an IOError

annotateIOError :: IOError 
              -> String 
              -> Maybe FilePath 
              -> Maybe Handle 
              -> IOError 
annotateIOError (IOError hdl errTy _ str path) loc opath ohdl = 
  IOError (hdl `mplus` ohdl) errTy loc str (path `mplus` opath)
  where
    Nothing `mplus` ys = ys
    xs      `mplus` _  = xs
#endif /* __GLASGOW_HASKELL__ || __HUGS__ */

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
