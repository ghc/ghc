{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module MarshalError (
        module Foreign.Marshal.Error,
        IOErrorType,
        mkIOError,
        alreadyExistsErrorType,
        doesNotExistErrorType,
        alreadyInUseErrorType,
        fullErrorType,
        eofErrorType,
        illegalOperationErrorType,
        permissionErrorType,
        userErrorType,
        annotateIOError
  ) where

import System.IO.Error
import Foreign.Marshal.Error
