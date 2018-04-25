{-# LANGUAGE Trustworthy #-}
#if __GLASGOW_HASKELL__ >= 709
{-# OPTIONS_GHC -fno-warn-trustworthy-safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Prim
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- @dlopen(3)@ and friends
--  Derived from @GModule.chs@ by M.Weber & M.Chakravarty which is part of c2hs.
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Prim (
  -- * low level API
  c_dlopen,
  c_dlsym,
  c_dlerror,
  c_dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packRTLDFlags,
  RTLDFlags(..),
  packDL,
  DL(..),
 )

where

#include "HsUnix.h"

import Data.Bits        ( (.|.) )
import Foreign.Ptr      ( Ptr, FunPtr, nullPtr )
import Foreign.C.Types
import Foreign.C.String ( CString )


-- |On some hosts (e.g. SuSe and Ubuntu Linux) @RTLD_NEXT@ (and
-- @RTLD_DEFAULT@) are not visible without setting the macro
-- @_GNU_SOURCE@. Since we don\'t want to define this macro, you can use
-- the function 'haveRtldNext' to check wether the flag `Next` is
-- available. Ideally, this will be optimized by the compiler so that it
-- should be as efficient as an @#ifdef@.
--
-- If you fail to test the flag and use it although it is undefined,
-- 'packDL' will throw an error.

haveRtldNext :: Bool

#ifdef HAVE_RTLDNEXT
haveRtldNext = True
foreign import ccall unsafe "__hsunix_rtldNext" rtldNext :: Ptr a
#else  /* HAVE_RTLDNEXT */
haveRtldNext = False
#endif /* HAVE_RTLDNEXT */

#ifdef HAVE_RTLDDEFAULT
foreign import ccall unsafe "__hsunix_rtldDefault" rtldDefault :: Ptr a
#endif /* HAVE_RTLDDEFAULT */

haveRtldLocal :: Bool
haveRtldLocal = True
{-# DEPRECATED haveRtldLocal "defaults to True" #-}


-- |Flags for 'System.Posix.DynamicLinker.dlopen'.

data RTLDFlags
  = RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_LOCAL
    deriving (Show, Read)

foreign import ccall unsafe "dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dlsym"  c_dlsym  :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "dlerror" c_dlerror :: IO CString
foreign import ccall unsafe "dlclose" c_dlclose :: (Ptr ()) -> IO CInt

packRTLDFlags :: [RTLDFlags] -> CInt
packRTLDFlags flags = foldl (\ s f -> (packRTLDFlag f) .|. s) 0 flags

packRTLDFlag :: RTLDFlags -> CInt
packRTLDFlag RTLD_LAZY = #const RTLD_LAZY
packRTLDFlag RTLD_NOW = #const RTLD_NOW
packRTLDFlag RTLD_GLOBAL = #const RTLD_GLOBAL
packRTLDFlag RTLD_LOCAL = #const RTLD_LOCAL


-- |Flags for 'System.Posix.DynamicLinker.dlsym'. Notice that 'Next'
-- might not be available on your particular platform! Use
-- 'haveRtldNext'.
--
-- If 'RTLD_DEFAULT' is not defined on your platform, 'packDL' 'Default'
-- reduces to 'nullPtr'.

data DL = Null | Next | Default | DLHandle (Ptr ()) deriving (Show)

packDL :: DL -> Ptr ()
packDL Null = nullPtr

#ifdef HAVE_RTLDNEXT
packDL Next = rtldNext
#else
packDL Next = error "RTLD_NEXT not available"
#endif

#ifdef HAVE_RTLDDEFAULT
packDL Default = rtldDefault
#else
packDL Default = nullPtr
#endif

packDL (DLHandle h) = h
