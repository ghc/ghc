{-# LANGUAGE CPP, NoImplicitPrelude #-}
module System.IO.Error.Compat (
  module Base
, isResourceVanishedError
, resourceVanishedErrorType
, isResourceVanishedErrorType
) where

import System.IO.Error as Base

#if !(MIN_VERSION_base(4,14,0))
import GHC.IO.Exception (IOErrorType(..))
import Prelude.Compat

-- | An error indicating that the operation failed because the
-- resource vanished. See 'resourceVanishedErrorType'.
--
-- /Since 4.14.0.0/
isResourceVanishedError :: IOError -> Bool
isResourceVanishedError = isResourceVanishedErrorType . ioeGetErrorType

-- | I\/O error where the operation failed because the resource vanished.
-- This happens when, for example, attempting to write to a closed
-- socket or attempting to write to a named pipe that was deleted.
--
-- /Since 4.14.0.0/
resourceVanishedErrorType :: IOErrorType
resourceVanishedErrorType = ResourceVanished

-- | I\/O error where the operation failed because the resource vanished.
-- See 'resourceVanishedErrorType'.
--
-- /Since 4.14.0.0/
isResourceVanishedErrorType :: IOErrorType -> Bool
isResourceVanishedErrorType ResourceVanished = True
isResourceVanishedErrorType _ = False
#endif
