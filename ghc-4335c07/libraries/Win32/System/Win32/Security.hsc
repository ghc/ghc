{-# OPTIONS_GHC -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
-- for details

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Security
-- Copyright   :  (c) Simon Marlow 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Simon Marlow
-- Stability   :  provisional
-- Portability :  portable
--
-- FFI-bindings to interact with Win32 Security
--
-----------------------------------------------------------------------------

module System.Win32.Security ( 
        -- * Types
        SID, PSID,
        ACL, PACL,
        SECURITY_DESCRIPTOR,

        SECURITY_DESCRIPTOR_CONTROL,
        se_OWNER_DEFAULTED,
        se_GROUP_DEFAULTED,
        se_DACL_PRESENT,
        se_DACL_DEFAULTED,
        se_SACL_PRESENT,
        se_SACL_DEFAULTED,
        se_DACL_AUTO_INHERIT_REQ,
        se_SACL_AUTO_INHERIT_REQ,
        se_DACL_AUTO_INHERITED,
        se_SACL_AUTO_INHERITED,
        se_DACL_PROTECTED,
        se_SACL_PROTECTED,
        se_SELF_RELATIVE,

        SECURITY_INFORMATION,
        oWNER_SECURITY_INFORMATION,
        gROUP_SECURITY_INFORMATION,
        dACL_SECURITY_INFORMATION,
        sACL_SECURITY_INFORMATION,

        -- * Functions
        getFileSecurity,
  ) where

import Foreign
-- import Foreign.C
import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>

-- --------------------------------------------------------------------------
-- Security Descriptors

newtype SECURITY_DESCRIPTOR = SECURITY_DESCRIPTOR SECURITY_DESCRIPTOR
type PSECURITY_DESCRIPTOR = Ptr SECURITY_DESCRIPTOR
newtype SecurityDescriptor = SecurityDescriptor (ForeignPtr SECURITY_DESCRIPTOR)

type SECURITY_DESCRIPTOR_CONTROL = WORD
#{enum SECURITY_DESCRIPTOR_CONTROL,
 , se_OWNER_DEFAULTED       = SE_OWNER_DEFAULTED
 , se_GROUP_DEFAULTED       = SE_GROUP_DEFAULTED
 , se_DACL_PRESENT          = SE_DACL_PRESENT
 , se_DACL_DEFAULTED        = SE_DACL_DEFAULTED
 , se_SACL_PRESENT          = SE_SACL_PRESENT
 , se_SACL_DEFAULTED        = SE_SACL_DEFAULTED
 , se_DACL_AUTO_INHERIT_REQ = SE_DACL_AUTO_INHERIT_REQ
 , se_SACL_AUTO_INHERIT_REQ = SE_SACL_AUTO_INHERIT_REQ
 , se_DACL_AUTO_INHERITED   = SE_DACL_AUTO_INHERITED
 , se_SACL_AUTO_INHERITED   = SE_SACL_AUTO_INHERITED
 , se_DACL_PROTECTED        = SE_DACL_PROTECTED
 , se_SACL_PROTECTED        = SE_SACL_PROTECTED
 , se_SELF_RELATIVE         = SE_SELF_RELATIVE
 }

newtype ACL = ACL ACL   -- abstract
type PACL = Ptr ACL

newtype SID = SID SID
type PSID = Ptr SID

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorControl"
  c_getSecurityDescriptorControl
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> Ptr SECURITY_DESCRIPTOR_CONTROL -- pControl
    -> LPDWORD -- lpdwRevision
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorDacl"
  c_getSecurityDescriptorDacl 
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> LPBOOL -- lpbDaclPresent
    -> Ptr PACL -- pDacl
    -> LPBOOL -- lpbDaclDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorGroup"
  c_getSecurityDescriptorGroup
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> Ptr PSID -- pGroup
    -> LPBOOL -- lpbGroupDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorLength"
  c_getSecurityDescriptorLength
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> IO DWORD 

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorOwner"
  c_getSecurityDescriptorOwner
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> Ptr PSID -- pOwner
    -> LPBOOL -- lpbOwnerDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h GetSecurityDescriptorSacl"
  c_getSecurityDescriptorSacl
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> LPBOOL -- lpbSaclPresent
    -> Ptr PACL -- pSacl
    -> LPBOOL -- lpbSaclDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h InitializeSecurityDescriptor"
  c_initializeSecurityDescriptor
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> DWORD -- dwRevision
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h IsValidSecurityDescriptor"
  c_isValidSecurityDescriptor
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h SetSecurityDescriptorDacl"
  c_setSecurityDescriptorDacl
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> BOOL -- bDaclPresent
    -> PACL -- pDacl
    -> BOOL -- bDaclDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h SetSecurityDescriptorGroup"
  c_setSecurityDescriptorGroup
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> PSID -- pGroup
    -> BOOL -- bGroupDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h SetSecurityDescriptorOwner"
  c_setSecurityDescriptorOwner
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> PSID -- pOwner
    -> BOOL -- bOwnerDefaulted
    -> IO BOOL 

foreign import WINDOWS_CCONV unsafe "windows.h SetSecurityDescriptorSacl"
  c_setSecurityDescriptorSacl
    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> BOOL -- bSaclPresent
    -> PACL -- pSacl
    -> BOOL -- bSaclDefaulted
    -> IO BOOL 

-- ---------------------------------------------------------------------------

-- PGENERIC_MAPPING
-- PPRIVILEGE_SET

type SECURITY_INFORMATION = DWORD

#{enum SECURITY_INFORMATION,
 , oWNER_SECURITY_INFORMATION = OWNER_SECURITY_INFORMATION
 , gROUP_SECURITY_INFORMATION = GROUP_SECURITY_INFORMATION
 , dACL_SECURITY_INFORMATION = DACL_SECURITY_INFORMATION
 , sACL_SECURITY_INFORMATION = SACL_SECURITY_INFORMATION
 }
-- , pROTECTED_DACL_SECURITY_INFORMATION = PROTECTED_DACL_SECURITY_INFORMATION
-- , pROTECTED_SACL_SECURITY_INFORMATION = PROTECTED_SACL_SECURITY_INFORMATION
-- , uNPROTECTED_DACL_SECURITY_INFORMATION = UNPROTECTED_DACL_SECURITY_INFORMATION
-- , uNPROTECTED_SACL_SECURITY_INFORMATION = UNPROTECTED_SACL_SECURITY_INFORMATION 

getFileSecurity
    :: String
    -> SECURITY_INFORMATION
    -> IO SecurityDescriptor
getFileSecurity filename si =
  withTString filename $ \lpFileName ->
  with 0 $ \lpnLengthNeeded -> do
  c_GetFileSecurity lpFileName si nullPtr 0 lpnLengthNeeded
  needed <- peek lpnLengthNeeded
  fpSd <- mallocForeignPtrBytes (fromIntegral needed)
  withForeignPtr fpSd $ \pSd -> do
  failIfFalse_ "getFileSecurity" $ 
    c_GetFileSecurity lpFileName si pSd needed lpnLengthNeeded
  return (SecurityDescriptor fpSd)

foreign import WINDOWS_CCONV unsafe "windows.h GetFileSecurityW"
  c_GetFileSecurity
    :: LPCWSTR -- lpFileName
    -> SECURITY_INFORMATION -- RequestedInformation
    -> PSECURITY_DESCRIPTOR -- pSecurityDescriptor
    -> DWORD -- nLength
    -> LPDWORD -- lpnLengthNeeded
    -> IO BOOL 

--foreign import WINDOWS_CCONV unsafe "windows.h AccessCheck"
--  c_AccessCheck
--    :: PSECURITY_DESCRIPTOR -- pSecurityDescriptor
--    -> HANDLE -- ClientToken
--    -> DWORD -- DesiredAccess
--    -> PGENERIC_MAPPING -- GenericMapping
--    -> PPRIVILEGE_SET -- PrivilegeSet
--    -> LPDWORD -- PrivilegeSetLength
--    -> LPDWORD -- GrantedAccess
--    -> LPBOOL -- AccessStatus
--    -> IO BOOL

-- foreign import WINDOWS_CCONV unsafe "windows.h OpenThreadToken"
--    OpenThreadToken
--      :: HANDLE -- ThreadHandle
--      -> DWORD -- DesiredAccess
--      -> BOOL -- OpenAsSelf
--      -> PHANDLE -- TokenHandle
--      -> IO BOOL
