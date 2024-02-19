{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.Windows
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Windows functionality used by several modules.
--
-- ToDo: this just duplicates part of System.Win32.Types, which isn't
-- available yet.  We should move some Win32 functionality down here,
-- maybe as part of the grand reorganisation of the base package...
--

module GHC.Windows
#if defined(javascript_HOST_ARCH)
    ( ) where

#else
    (
        -- * Types
        BOOL,
        LPBOOL,
        BYTE,
        DWORD,
        DDWORD,
        UINT,
        ULONG,
        ErrCode,
        HANDLE,
        LPWSTR,
        LPTSTR,
        LPCTSTR,
        LPVOID,
        LPDWORD,
        LPSTR,
        LPCSTR,
        LPCWSTR,
        WORD,
        UCHAR,
        NTSTATUS,

        -- * Constants
        iNFINITE,
        iNVALID_HANDLE_VALUE,

        -- * System errors
        throwGetLastError,
        failWith,
        getLastError,
        getErrorMessage,
        errCodeToIOError,

        -- ** Guards for system calls that might fail
        failIf,
        failIf_,
        failIfNull,
        failIfZero,
        failIfFalse_,
        failUnlessSuccess,
        failUnlessSuccessOr,

        -- ** Mapping system errors to errno
        -- $errno
        c_maperrno,
        c_maperrno_func,

        -- * Misc
        ddwordToDwords,
        dwordsToDdword,
        nullHANDLE,
    ) where

#endif

import GHC.Internal.Windows

