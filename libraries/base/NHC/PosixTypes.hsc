{-# OPTIONS_NHC98 -I/usr/include #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NHC.PosixTypes
-- Copyright   :  (c) Malcolm Wallace 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX data types: Haskell equivalents of the types defined by the
-- @\<sys\/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------

module NHC.PosixTypes (

  -- * POSIX data types
  CDev,
  CIno,
  CMode,
  COff,
  CPid,
  CSsize,

  CGid,
  CNlink,
  CUid,
  CCc,
  CSpeed,
  CTcflag,
  CRLim,

  Fd(..),

  LinkCount,
  UserID,
  GroupID,
 ) where

import Foreign
import Foreign.C
import Data.Typeable
import Data.Bits
import Unsafe.Coerce

import Control.Monad


-- Curious hack to ensure that the CTypes macros are expanded *after* hsc2hs.
##include "CTypes.h"
-- C header files that contain all the types we are looking for here.
#if __APPLE__
#include <libc.h>
#endif
#include <stdlib.h>
#include <unistd.h>
#include <sys/resource.h>
#include <termios.h>

ARITHMETIC_TYPE(CDev,tyConCDev,"CDev",#{type dev_t})
INTEGRAL_TYPE(CIno,tyConCIno,"CIno",#{type ino_t})
INTEGRAL_TYPE(CMode,tyConCMode,"CMode",#{type mode_t})
INTEGRAL_TYPE(COff,tyConCOff,"COff",#{type off_t})
INTEGRAL_TYPE(CPid,tyConCPid,"CPid",#{type pid_t})

INTEGRAL_TYPE(CSsize,tyConCSsize,"CSsize",#{type ssize_t})

INTEGRAL_TYPE(CGid,tyConCGid,"CGid",#{type gid_t})
INTEGRAL_TYPE(CNlink,tyConCNlink,"CNlink",#{type nlink_t})

INTEGRAL_TYPE(CUid,tyConCUid,"CUid",#{type uid_t})
ARITHMETIC_TYPE(CCc,tyConCCc,"CCc",#{type cc_t})
ARITHMETIC_TYPE(CSpeed,tyConCSpeed,"CSpeed",#{type speed_t})
INTEGRAL_TYPE(CTcflag,tyConCTcflag,"CTcflag",#{type tcflag_t})
INTEGRAL_TYPE(CRLim,tyConCRlim,"CRLim",#{type rlim_t})

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere
INTEGRAL_TYPE(Fd,tyConFd,"Fd",CInt)

-- nicer names, and backwards compatibility with POSIX library:
type LinkCount      = CNlink
type UserID         = CUid
type GroupID        = CGid
