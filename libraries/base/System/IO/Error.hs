{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

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

    -- * I\/O errors
    IOError,                    -- = IOException

    userError,                  -- :: String  -> IOError

    mkIOError,                  -- :: IOErrorType -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    annotateIOError,            -- :: IOError -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    -- ** Classifying I\/O errors
    isAlreadyExistsError,       -- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError, 
    isEOFError,
    isIllegalOperation, 
    isPermissionError,
    isUserError,

    -- ** Attributes of I\/O errors
    ioeGetErrorType,            -- :: IOError -> IOErrorType
    ioeGetLocation,             -- :: IOError -> String
    ioeGetErrorString,          -- :: IOError -> String
    ioeGetHandle,               -- :: IOError -> Maybe Handle
    ioeGetFileName,             -- :: IOError -> Maybe FilePath

    ioeSetErrorType,            -- :: IOError -> IOErrorType -> IOError
    ioeSetErrorString,          -- :: IOError -> String -> IOError
    ioeSetLocation,             -- :: IOError -> String -> IOError
    ioeSetHandle,               -- :: IOError -> Handle -> IOError
    ioeSetFileName,             -- :: IOError -> FilePath -> IOError

    -- * Types of I\/O error
    IOErrorType,                -- abstract

    alreadyExistsErrorType,     -- :: IOErrorType
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType, 
    permissionErrorType,
    userErrorType,

    -- ** 'IOErrorType' predicates
    isAlreadyExistsErrorType,   -- :: IOErrorType -> Bool
    isDoesNotExistErrorType,
    isAlreadyInUseErrorType,
    isFullErrorType, 
    isEOFErrorType,
    isIllegalOperationErrorType, 
    isPermissionErrorType,
    isUserErrorType, 

    -- * Throwing and catching I\/O errors

    ioError,                    -- :: IOError -> IO a

    catchIOError,               -- :: IO a -> (IOError -> IO a) -> IO a
    catch,                      -- :: IO a -> (IOError -> IO a) -> IO a
    tryIOError,                 -- :: IO a -> IO (Either IOError a)
    try,                        -- :: IO a -> IO (Either IOError a)

    modifyIOError,              -- :: (IOError -> IOError) -> IO a -> IO a
  ) where

#ifndef __HUGS__
import qualified Control.Exception.Base as New (catch)
#endif

#ifndef __HUGS__
import Data.Either
#endif
import Data.Maybe

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IO
import GHC.IO.Exception
import GHC.IO.Handle.Types
import Text.Show
#endif

#ifdef __HUGS__
import Hugs.Prelude(Handle, IOException(..), IOErrorType(..), IO)
#endif

#ifdef __NHC__
import IO
  ( IOError ()
  , Handle ()
  , try
  , ioError
  , userError
  , isAlreadyExistsError        -- :: IOError -> Bool
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
import qualified NHC.Internal as NHC (IOError(..))
import qualified NHC.DErrNo as NHC (ErrNo(..))
import Data.Maybe (fromJust)
import Control.Monad (MonadPlus(mplus))
#endif

-- | The construct 'tryIOError' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.try' from "Control.Exception".
tryIOError     :: IO a -> IO (Either IOError a)
tryIOError f   =  catch (do r <- f
                            return (Right r))
                        (return . Left)

#ifndef __NHC__
{-# DEPRECATED try "Please use the new exceptions variant, Control.Exception.try" #-}
-- | The 'try' function is deprecated. Please use the new exceptions
-- variant, 'Control.Exception.try' from "Control.Exception", instead.
try            :: IO a -> IO (Either IOError a)
try f          =  catch (do r <- f
                            return (Right r))
                        (return . Left)
#endif

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
-- -----------------------------------------------------------------------------
-- Constructing an IOError

-- | Construct an 'IOError' of the given type where the second argument
-- describes the error location and the third and fourth argument
-- contain the file handle and file path of the file involved in the
-- error if applicable.
mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError t location maybe_hdl maybe_filename =
               IOError{ ioe_type = t, 
                        ioe_location = location,
                        ioe_description = "",
#if defined(__GLASGOW_HASKELL__)
                        ioe_errno = Nothing,
#endif
                        ioe_handle = maybe_hdl, 
                        ioe_filename = maybe_filename
                        }
#endif /* __GLASGOW_HASKELL__ || __HUGS__ */
#ifdef __NHC__
mkIOError EOF       location maybe_hdl maybe_filename =
    NHC.EOFError location (fromJust maybe_hdl)
mkIOError UserError location maybe_hdl maybe_filename =
    NHC.UserError location ""
mkIOError t         location maybe_hdl maybe_filename =
    NHC.IOError location maybe_filename maybe_hdl (ioeTypeToErrNo t)
  where
    ioeTypeToErrNo AlreadyExists     = NHC.EEXIST
    ioeTypeToErrNo NoSuchThing       = NHC.ENOENT
    ioeTypeToErrNo ResourceBusy      = NHC.EBUSY
    ioeTypeToErrNo ResourceExhausted = NHC.ENOSPC
    ioeTypeToErrNo IllegalOperation  = NHC.EPERM
    ioeTypeToErrNo PermissionDenied  = NHC.EACCES
#endif /* __NHC__ */

#ifndef __NHC__
-- -----------------------------------------------------------------------------
-- IOErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments already exists.
isAlreadyExistsError :: IOError -> Bool
isAlreadyExistsError = isAlreadyExistsErrorType    . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments does not exist.
isDoesNotExistError :: IOError -> Bool
isDoesNotExistError  = isDoesNotExistErrorType     . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- one of its arguments is a single-use resource, which is already
-- being used (for example, opening the same file twice for writing
-- might give this error).
isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError  = isAlreadyInUseErrorType     . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the device is full.
isFullError         :: IOError -> Bool
isFullError          = isFullErrorType             . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the end of file has been reached.
isEOFError          :: IOError -> Bool
isEOFError           = isEOFErrorType              . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the operation was not possible.
-- Any computation which returns an 'IO' result may fail with
-- 'isIllegalOperation'.  In some cases, an implementation will not be
-- able to distinguish between the possible error causes.  In this case
-- it should fail with 'isIllegalOperation'.
isIllegalOperation  :: IOError -> Bool
isIllegalOperation   = isIllegalOperationErrorType . ioeGetErrorType

-- | An error indicating that an 'IO' operation failed because
-- the user does not have sufficient operating system privilege
-- to perform that operation.
isPermissionError   :: IOError -> Bool
isPermissionError    = isPermissionErrorType       . ioeGetErrorType

-- | A programmer-defined error value constructed using 'userError'.
isUserError         :: IOError -> Bool
isUserError          = isUserErrorType             . ioeGetErrorType
#endif /* __NHC__ */

-- -----------------------------------------------------------------------------
-- IOErrorTypes

#ifdef __NHC__
data IOErrorType = AlreadyExists | NoSuchThing | ResourceBusy
                 | ResourceExhausted | EOF | IllegalOperation
                 | PermissionDenied | UserError
#endif

-- | I\/O error where the operation failed because one of its arguments
-- already exists.
alreadyExistsErrorType   :: IOErrorType
alreadyExistsErrorType    = AlreadyExists

-- | I\/O error where the operation failed because one of its arguments
-- does not exist.
doesNotExistErrorType    :: IOErrorType
doesNotExistErrorType     = NoSuchThing

-- | I\/O error where the operation failed because one of its arguments
-- is a single-use resource, which is already being used.
alreadyInUseErrorType    :: IOErrorType
alreadyInUseErrorType     = ResourceBusy

-- | I\/O error where the operation failed because the device is full.
fullErrorType            :: IOErrorType
fullErrorType             = ResourceExhausted

-- | I\/O error where the operation failed because the end of file has
-- been reached.
eofErrorType             :: IOErrorType
eofErrorType              = EOF

-- | I\/O error where the operation is not possible.
illegalOperationErrorType :: IOErrorType
illegalOperationErrorType = IllegalOperation

-- | I\/O error where the operation failed because the user does not
-- have sufficient operating system privilege to perform that operation.
permissionErrorType      :: IOErrorType
permissionErrorType       = PermissionDenied

-- | I\/O error that is programmer-defined.
userErrorType            :: IOErrorType
userErrorType             = UserError

-- -----------------------------------------------------------------------------
-- IOErrorType predicates

-- | I\/O error where the operation failed because one of its arguments
-- already exists.
isAlreadyExistsErrorType :: IOErrorType -> Bool
isAlreadyExistsErrorType AlreadyExists = True
isAlreadyExistsErrorType _ = False

-- | I\/O error where the operation failed because one of its arguments
-- does not exist.
isDoesNotExistErrorType :: IOErrorType -> Bool
isDoesNotExistErrorType NoSuchThing = True
isDoesNotExistErrorType _ = False

-- | I\/O error where the operation failed because one of its arguments
-- is a single-use resource, which is already being used.
isAlreadyInUseErrorType :: IOErrorType -> Bool
isAlreadyInUseErrorType ResourceBusy = True
isAlreadyInUseErrorType _ = False

-- | I\/O error where the operation failed because the device is full.
isFullErrorType :: IOErrorType -> Bool
isFullErrorType ResourceExhausted = True
isFullErrorType _ = False

-- | I\/O error where the operation failed because the end of file has
-- been reached.
isEOFErrorType :: IOErrorType -> Bool
isEOFErrorType EOF = True
isEOFErrorType _ = False

-- | I\/O error where the operation is not possible.
isIllegalOperationErrorType :: IOErrorType -> Bool
isIllegalOperationErrorType IllegalOperation = True
isIllegalOperationErrorType _ = False

-- | I\/O error where the operation failed because the user does not
-- have sufficient operating system privilege to perform that operation.
isPermissionErrorType :: IOErrorType -> Bool
isPermissionErrorType PermissionDenied = True
isPermissionErrorType _ = False

-- | I\/O error that is programmer-defined.
isUserErrorType :: IOErrorType -> Bool
isUserErrorType UserError = True
isUserErrorType _ = False

-- -----------------------------------------------------------------------------
-- Miscellaneous

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
ioeGetErrorType       :: IOError -> IOErrorType
ioeGetErrorString     :: IOError -> String
ioeGetLocation        :: IOError -> String
ioeGetHandle          :: IOError -> Maybe Handle
ioeGetFileName        :: IOError -> Maybe FilePath

ioeGetErrorType ioe = ioe_type ioe

ioeGetErrorString ioe
   | isUserErrorType (ioe_type ioe) = ioe_description ioe
   | otherwise                      = show (ioe_type ioe)

ioeGetLocation ioe = ioe_location ioe

ioeGetHandle ioe = ioe_handle ioe

ioeGetFileName ioe = ioe_filename ioe

ioeSetErrorType   :: IOError -> IOErrorType -> IOError
ioeSetErrorString :: IOError -> String      -> IOError
ioeSetLocation    :: IOError -> String      -> IOError
ioeSetHandle      :: IOError -> Handle      -> IOError
ioeSetFileName    :: IOError -> FilePath    -> IOError

ioeSetErrorType   ioe errtype  = ioe{ ioe_type = errtype }
ioeSetErrorString ioe str      = ioe{ ioe_description = str }
ioeSetLocation    ioe str      = ioe{ ioe_location = str }
ioeSetHandle      ioe hdl      = ioe{ ioe_handle = Just hdl }
ioeSetFileName    ioe filename = ioe{ ioe_filename = Just filename }

#elif defined(__NHC__)
ioeGetErrorType       :: IOError -> IOErrorType
ioeGetLocation        :: IOError -> String

ioeGetErrorType e | isAlreadyExistsError e = AlreadyExists
                  | isDoesNotExistError e  = NoSuchThing
                  | isAlreadyInUseError e  = ResourceBusy
                  | isFullError e          = ResourceExhausted
                  | isEOFError e           = EOF
                  | isIllegalOperation e   = IllegalOperation
                  | isPermissionError e    = PermissionDenied
                  | isUserError e          = UserError

ioeGetLocation (NHC.IOError _ _ _ _)  = "unknown location"
ioeGetLocation (NHC.EOFError _ _ )    = "unknown location"
ioeGetLocation (NHC.PatternError loc) = loc
ioeGetLocation (NHC.UserError loc _)  = loc

ioeSetErrorType   :: IOError -> IOErrorType -> IOError
ioeSetErrorString :: IOError -> String      -> IOError
ioeSetLocation    :: IOError -> String      -> IOError
ioeSetHandle      :: IOError -> Handle      -> IOError
ioeSetFileName    :: IOError -> FilePath    -> IOError

ioeSetErrorType e _ = e
ioeSetErrorString   (NHC.IOError _ f h e) s = NHC.IOError s f h e
ioeSetErrorString   (NHC.EOFError _ f)    s = NHC.EOFError s f
ioeSetErrorString e@(NHC.PatternError _)  _ = e
ioeSetErrorString   (NHC.UserError l _)   s = NHC.UserError l s
ioeSetLocation e@(NHC.IOError _ _ _ _) _ = e
ioeSetLocation e@(NHC.EOFError _ _)    _ = e
ioeSetLocation   (NHC.PatternError _)  l = NHC.PatternError l
ioeSetLocation   (NHC.UserError _ m)   l = NHC.UserError l m
ioeSetHandle   (NHC.IOError o f _ e) h = NHC.IOError o f (Just h) e
ioeSetHandle   (NHC.EOFError o _)    h = NHC.EOFError o h
ioeSetHandle e@(NHC.PatternError _)  _ = e
ioeSetHandle e@(NHC.UserError _ _)   _ = e
ioeSetFileName (NHC.IOError o _ h e) f = NHC.IOError o (Just f) h e
ioeSetFileName e _ = e
#endif

-- | Catch any 'IOError' that occurs in the computation and throw a
-- modified version.
modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError f io = catch io (\e -> ioError (f e))

-- -----------------------------------------------------------------------------
-- annotating an IOError

-- | Adds a location description and maybe a file path and file handle
-- to an 'IOError'.  If any of the file handle or file path is not given
-- the corresponding value in the 'IOError' remains unaltered.
annotateIOError :: IOError 
              -> String 
              -> Maybe Handle 
              -> Maybe FilePath 
              -> IOError 

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
annotateIOError ioe loc hdl path = 
  ioe{ ioe_handle = hdl `mplus` ioe_handle ioe,
       ioe_location = loc, ioe_filename = path `mplus` ioe_filename ioe }
  where
    mplus :: Maybe a -> Maybe a -> Maybe a
    Nothing `mplus` ys = ys
    xs      `mplus` _  = xs
#endif /* __GLASGOW_HASKELL__ || __HUGS__ */

#if defined(__NHC__)
annotateIOError (NHC.IOError msg file hdl code) msg' hdl' file' =
    NHC.IOError (msg++'\n':msg') (file`mplus`file') (hdl`mplus`hdl') code
annotateIOError (NHC.EOFError msg hdl) msg' _ _ =
    NHC.EOFError (msg++'\n':msg') hdl
annotateIOError (NHC.UserError loc msg) msg' _ _ =
    NHC.UserError loc (msg++'\n':msg')
annotateIOError (NHC.PatternError loc) msg' _ _ =
    NHC.PatternError (loc++'\n':msg')
#endif

#ifndef __HUGS__
-- | The 'catchIOError' function establishes a handler that receives any
-- 'IOError' raised in the action protected by 'catchIOError'.
-- An 'IOError' is caught by
-- the most recent handler established by one of the exception handling
-- functions.  These handlers are
-- not selective: all 'IOError's are caught.  Exception propagation
-- must be explicitly provided in a handler by re-raising any unwanted
-- exceptions.  For example, in
--
-- > f = catchIOError g (\e -> if IO.isEOFError e then return [] else ioError e)
--
-- the function @f@ returns @[]@ when an end-of-file exception
-- (cf. 'System.IO.Error.isEOFError') occurs in @g@; otherwise, the
-- exception is propagated to the next outer handler.
--
-- When an exception propagates outside the main program, the Haskell
-- system prints the associated 'IOError' value and exits the program.
--
-- Non-I\/O exceptions are not caught by this variant; to catch all
-- exceptions, use 'Control.Exception.catch' from "Control.Exception".
catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = New.catch

{-# DEPRECATED catch "Please use the new exceptions variant, Control.Exception.catch" #-}
-- | The 'catch' function is deprecated. Please use the new exceptions
-- variant, 'Control.Exception.catch' from "Control.Exception", instead.
catch :: IO a -> (IOError -> IO a) -> IO a
catch = New.catch
#endif /* !__HUGS__ */

