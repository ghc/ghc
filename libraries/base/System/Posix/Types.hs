{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Types
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX data types: Haskell equivalents of the types defined by the
-- @\<sys/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------

#include "config.h"

module System.Posix.Types (

  -- * POSIX data types
  CDev(..), CIno(..), CMode(..), COff(..), CPid(..), CSsize(..),

#ifndef mingw32_TARGET_OS
  CGid(..), CNlink(..), CSsize(..), CUid(..), CCc(..), CSpeed(..),
  CTcflag(..),
#endif

  Fd(..),

#ifndef mingw32_TARGET_OS
  LinkCount,
  UserID,
  GroupID,
#endif

  ByteCount,
  ClockTick,
  EpochTime,
  FileOffset,
  ProcessID,
  ProcessGroupID,
  DeviceID,
  FileID,
  FileMode
 ) where

import Foreign
import Foreign.C
import Data.Dynamic
import Data.Bits

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Prim
import GHC.Read
import GHC.Show
#else
import Control.Monad
#endif

#include "Dynamic.h"
#include "CTypes.h"

NUMERIC_TYPE(CDev,tyConCDev,"CDev",HTYPE_DEV_T)
INTEGRAL_TYPE(CIno,tyConCIno,"CIno",HTYPE_INO_T)
INTEGRAL_TYPE(CMode,tyConCMode,"CMode",HTYPE_MODE_T)
INTEGRAL_TYPE(COff,tyConCOff,"COff",HTYPE_OFF_T)
INTEGRAL_TYPE(CPid,tyConCPid,"CPid",HTYPE_PID_T)

#ifdef mingw32_TARGET_OS
INTEGRAL_TYPE(CSsize,tyConCSsize,"CSsize",HTYPE_SIZE_T)
#else
INTEGRAL_TYPE(CSsize,tyConCSsize,"CSsize",HTYPE_SSIZE_T)
#endif

#ifndef mingw32_TARGET_OS
INTEGRAL_TYPE(CGid,tyConCGid,"CGid",HTYPE_GID_T)
INTEGRAL_TYPE(CNlink,tyConCNlink,"CNlink",HTYPE_NLINK_T)
INTEGRAL_TYPE(CUid,tyConCUid,"CUid",HTYPE_UID_T)
NUMERIC_TYPE(CCc,tyConCCc,"CCc",HTYPE_CC_T)
NUMERIC_TYPE(CSpeed,tyConCSpeed,"CSpeed",HTYPE_SPEED_T)
INTEGRAL_TYPE(CTcflag,tyConCTcflag,"CTcflag",HTYPE_TCFLAG_T)
#endif

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere
INTEGRAL_TYPE(Fd,tyConFd,"Fd",CInt)

-- nicer names, and backwards compatibility with POSIX library:
#ifndef mingw32_TARGET_OS
type LinkCount      = CNlink
type UserID         = CUid
type GroupID        = CGid
#endif

type ByteCount      = CSize
type ClockTick      = CClock
type EpochTime      = CTime
type DeviceID       = CDev
type FileID         = CIno
type FileMode       = CMode
type ProcessID      = CPid
type FileOffset     = COff
type ProcessGroupID = CPid
