{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}

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
-- @\<sys\/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------
#include "HsBaseConfig.h"

module System.Posix.Types (

  -- * POSIX data types

  -- ** Platform differences
  -- | This module contains platform specific information about types.
  --   __/As such the types presented on this page reflect the platform
  --   on which the documentation was generated and may not coincide with
  --   the types on your platform./__
#if defined(HTYPE_DEV_T)
  CDev(..),
#endif
#if defined(HTYPE_INO_T)
  CIno(..),
#endif
#if defined(HTYPE_MODE_T)
  CMode(..),
#endif
#if defined(HTYPE_OFF_T)
  COff(..),
#endif
#if defined(HTYPE_PID_T)
  CPid(..),
#endif
#if defined(HTYPE_SSIZE_T)
  CSsize(..),
#endif

#if defined(HTYPE_GID_T)
  CGid(..),
#endif
#if defined(HTYPE_NLINK_T)
  CNlink(..),
#endif
#if defined(HTYPE_UID_T)
  CUid(..),
#endif
#if defined(HTYPE_CC_T)
  CCc(..),
#endif
#if defined(HTYPE_SPEED_T)
  CSpeed(..),
#endif
#if defined(HTYPE_TCFLAG_T)
  CTcflag(..),
#endif
#if defined(HTYPE_RLIM_T)
  CRLim(..),
#endif
#if defined(HTYPE_BLKSIZE_T)
  CBlkSize(..),
#endif
#if defined(HTYPE_BLKCNT_T)
  CBlkCnt(..),
#endif
#if defined(HTYPE_CLOCKID_T)
  CClockId(..),
#endif
#if defined(HTYPE_FSBLKCNT_T)
  CFsBlkCnt(..),
#endif
#if defined(HTYPE_FSFILCNT_T)
  CFsFilCnt(..),
#endif
#if defined(HTYPE_ID_T)
  CId(..),
#endif
#if defined(HTYPE_KEY_T)
  CKey(..),
#endif
#if defined(HTYPE_TIMER_T)
  CTimer(..),
#endif
#if defined(HTYPE_SOCKLEN_T)
  CSocklen(..),
#endif
#if defined(HTYPE_NFDS_T)
  CNfds(..),
#endif

  Fd(..),

  -- See Note [Exporting constructors of marshallable foreign types]
  -- in Foreign.Ptr for why the constructors for these newtypes are
  -- exported.

#if defined(HTYPE_NLINK_T)
  LinkCount,
#endif
#if defined(HTYPE_UID_T)
  UserID,
#endif
#if defined(HTYPE_GID_T)
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
  FileMode,
  Limit
 ) where

import Foreign
import Foreign.C
-- import Data.Bits

import GHC.Base
import GHC.Enum
import GHC.Ix
import GHC.Num
import GHC.Read
import GHC.Real
-- import GHC.Prim
import GHC.Show

#include "CTypes.h"

#if defined(HTYPE_DEV_T)
INTEGRAL_TYPE(CDev,"dev_t",HTYPE_DEV_T)
#endif
#if defined(HTYPE_INO_T)
INTEGRAL_TYPE(CIno,"ino_t",HTYPE_INO_T)
#endif
#if defined(HTYPE_MODE_T)
INTEGRAL_TYPE(CMode,"mode_t",HTYPE_MODE_T)
#endif
#if defined(HTYPE_OFF_T)
INTEGRAL_TYPE(COff,"off_t",HTYPE_OFF_T)
#endif
#if defined(HTYPE_PID_T)
INTEGRAL_TYPE(CPid,"pid_t",HTYPE_PID_T)
#endif

#if defined(HTYPE_SSIZE_T)
INTEGRAL_TYPE(CSsize,"ssize_t",HTYPE_SSIZE_T)
#endif

#if defined(HTYPE_GID_T)
INTEGRAL_TYPE(CGid,"gid_t",HTYPE_GID_T)
#endif
#if defined(HTYPE_NLINK_T)
INTEGRAL_TYPE(CNlink,"nlink_t",HTYPE_NLINK_T)
#endif

#if defined(HTYPE_UID_T)
INTEGRAL_TYPE(CUid,"uid_t",HTYPE_UID_T)
#endif
#if defined(HTYPE_CC_T)
ARITHMETIC_TYPE(CCc,"cc_t",HTYPE_CC_T)
#endif
#if defined(HTYPE_SPEED_T)
ARITHMETIC_TYPE(CSpeed,"speed_t",HTYPE_SPEED_T)
#endif
#if defined(HTYPE_TCFLAG_T)
INTEGRAL_TYPE(CTcflag,"tcflag_t",HTYPE_TCFLAG_T)
#endif
#if defined(HTYPE_RLIM_T)
INTEGRAL_TYPE(CRLim,"rlim_t",HTYPE_RLIM_T)
#endif

#if defined(HTYPE_BLKSIZE_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CBlkSize,"blksize_t",HTYPE_BLKSIZE_T)
#endif
#if defined(HTYPE_BLKCNT_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CBlkCnt,"blkcnt_t",HTYPE_BLKCNT_T)
#endif
#if defined(HTYPE_CLOCKID_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CClockId,"clockid_t",HTYPE_CLOCKID_T)
#endif
#if defined(HTYPE_FSBLKCNT_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CFsBlkCnt,"fsblkcnt_t",HTYPE_FSBLKCNT_T)
#endif
#if defined(HTYPE_FSFILCNT_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CFsFilCnt,"fsfilcnt_t",HTYPE_FSFILCNT_T)
#endif
#if defined(HTYPE_ID_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CId,"id_t",HTYPE_ID_T)
#endif
#if defined(HTYPE_KEY_T)
-- | @since 4.10.0.0
INTEGRAL_TYPE(CKey,"key_t",HTYPE_KEY_T)
#endif
#if defined(HTYPE_TIMER_T)
-- | @since 4.10.0.0
OPAQUE_TYPE(CTimer,"timer_t",HTYPE_TIMER_T)
#endif

#if defined(HTYPE_SOCKLEN_T)
-- | @since 4.14.0.0
INTEGRAL_TYPE(CSocklen,"socklen_t",HTYPE_SOCKLEN_T)
#endif
#if defined(HTYPE_NFDS_T)
-- | @since 4.14.0.0
INTEGRAL_TYPE(CNfds,"nfds_t",HTYPE_NFDS_T)
#endif

-- Make an Fd type rather than using CInt everywhere
INTEGRAL_TYPE(Fd,"int",CInt)

-- nicer names, and backwards compatibility with POSIX library:
#if defined(HTYPE_NLINK_T)
type LinkCount      = CNlink
#endif
#if defined(HTYPE_UID_T)
type UserID         = CUid
#endif
#if defined(HTYPE_GID_T)
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
type Limit          = CLong
