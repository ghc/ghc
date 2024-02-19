{-# LANGUAGE Safe #-}

-- |
--
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

module System.IO.Error
    (-- *  I\/O errors
     IOError,
     userError,
     mkIOError,
     annotateIOError,
     -- **  Classifying I\/O errors
     isAlreadyExistsError,
     isDoesNotExistError,
     isAlreadyInUseError,
     isFullError,
     isEOFError,
     isIllegalOperation,
     isPermissionError,
     isUserError,
     isResourceVanishedError,
     -- **  Attributes of I\/O errors
     ioeGetErrorType,
     ioeGetLocation,
     ioeGetErrorString,
     ioeGetHandle,
     ioeGetFileName,
     ioeSetErrorType,
     ioeSetErrorString,
     ioeSetLocation,
     ioeSetHandle,
     ioeSetFileName,
     -- *  Types of I\/O error
     IOErrorType,
     alreadyExistsErrorType,
     doesNotExistErrorType,
     alreadyInUseErrorType,
     fullErrorType,
     eofErrorType,
     illegalOperationErrorType,
     permissionErrorType,
     userErrorType,
     resourceVanishedErrorType,
     -- **  'IOErrorType' predicates
     isAlreadyExistsErrorType,
     isDoesNotExistErrorType,
     isAlreadyInUseErrorType,
     isFullErrorType,
     isEOFErrorType,
     isIllegalOperationErrorType,
     isPermissionErrorType,
     isUserErrorType,
     isResourceVanishedErrorType,
     -- *  Throwing and catching I\/O errors
     ioError,
     catchIOError,
     tryIOError,
     modifyIOError
     ) where

import GHC.Internal.System.IO.Error