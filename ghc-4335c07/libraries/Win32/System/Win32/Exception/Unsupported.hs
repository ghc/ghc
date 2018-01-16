{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      :  System.Win32.Exception.Unsupported
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Exception handling if using unsupported Win32 API.
-}

module System.Win32.Exception.Unsupported
  ( module System.Win32.Exception.Unsupported
  ) where

import Control.Exception      ( Exception(..), throwIO )
import Data.Typeable          ( Typeable )
import Foreign.Ptr            ( Ptr, nullPtr )
import Foreign.Marshal.Unsafe ( unsafeLocalState )

----------------------------------------------------------------
-- Exception type of Unsupported
----------------------------------------------------------------
data Unsupported = MissingLibrary  FilePath String
                 | MissingFunction String   String
                 | MissingValue    String   String
                 deriving Typeable

instance Show Unsupported where
  show (MissingLibrary  name reason)
    = "Can't load library \"" ++ name ++ "\". "  ++ reason
  show (MissingFunction name reason)
    = "Can't find \"" ++ name ++ "\" function. " ++ reason
  show (MissingValue    name reason)
    = "Can't use \""  ++ name ++ "\" value. "    ++ reason

instance Exception Unsupported

missingLibrary                          :: FilePath -> Unsupported
missingFunction,      missingValue      :: String -> Unsupported
missingLibrary  name = MissingLibrary  name ""
missingFunction name = MissingFunction name ""
missingValue    name = MissingValue    name ""

missingWin32Function, missingWin32Value :: String -> String -> Unsupported
missingWin32Function name reason = MissingFunction name $ doesn'tSupport ++ '\n':reason
missingWin32Value    name reason = MissingValue    name $ doesn'tSupport ++ '\n':reason

doesn'tSupport, upgradeVista, removed :: String
doesn'tSupport = "Because it's not supported on this OS."
upgradeVista   = upgradeWindowsOS "Windows Vista"
removed = "It's removed. "

upgradeWindowsOS :: String -> String
upgradeWindowsOS ver
  =  "If you want to use it, please upgrade your OS to "
  ++ ver ++ " or higher."

unsupportedIfNull :: Unsupported -> IO (Ptr a) -> IO (Ptr a)
unsupportedIfNull wh act = do
  v <- act
  if v /= nullPtr then return v else throwIO wh

unsupportedVal :: String -> IO Bool -> String -> a -> a
unsupportedVal name checkVer reason val = unsafeLocalState $ do
  cv <- checkVer
  if cv then return val else throwIO $ MissingValue name reason

