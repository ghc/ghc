{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------

module GHC.Windows (
        HANDLE, DWORD, LPTSTR, iNFINITE,
        throwGetLastError, c_maperrno
    ) where

import GHC.Base
import GHC.Ptr

import Data.Word

import Foreign.C.Error (throwErrno)
import Foreign.C.Types


type HANDLE       = Ptr ()
type DWORD        = Word32

type LPTSTR = Ptr CWchar

iNFINITE :: DWORD
iNFINITE = 0xFFFFFFFF -- urgh

throwGetLastError :: String -> IO a
throwGetLastError where_from = c_maperrno >> throwErrno where_from

foreign import ccall unsafe "maperrno"             -- in Win32Utils.c
   c_maperrno :: IO ()

