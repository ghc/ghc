{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# LANGUAGE StandaloneDeriving #-}

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

  Fd(..),

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
import GHC.Num
import GHC.Real
-- import GHC.Prim
import GHC.Read
import GHC.Show

#include "CTypes.h"

#if defined(HTYPE_DEV_T)
INTEGRAL_TYPE(CDev,HTYPE_DEV_T)
#endif
#if defined(HTYPE_INO_T)
INTEGRAL_TYPE(CIno,HTYPE_INO_T)
#endif
#if defined(HTYPE_MODE_T)
INTEGRAL_TYPE_WITH_CTYPE(CMode,mode_t,HTYPE_MODE_T)
#endif
#if defined(HTYPE_OFF_T)
INTEGRAL_TYPE(COff,HTYPE_OFF_T)
#endif
#if defined(HTYPE_PID_T)
INTEGRAL_TYPE(CPid,HTYPE_PID_T)
#endif

#if defined(HTYPE_SSIZE_T)
INTEGRAL_TYPE(CSsize,HTYPE_SSIZE_T)
#endif

#if defined(HTYPE_GID_T)
INTEGRAL_TYPE(CGid,HTYPE_GID_T)
#endif
#if defined(HTYPE_NLINK_T)
INTEGRAL_TYPE(CNlink,HTYPE_NLINK_T)
#endif

#if defined(HTYPE_UID_T)
INTEGRAL_TYPE(CUid,HTYPE_UID_T)
#endif
#if defined(HTYPE_CC_T)
ARITHMETIC_TYPE(CCc,HTYPE_CC_T)
#endif
#if defined(HTYPE_SPEED_T)
ARITHMETIC_TYPE(CSpeed,HTYPE_SPEED_T)
#endif
#if defined(HTYPE_TCFLAG_T)
INTEGRAL_TYPE(CTcflag,HTYPE_TCFLAG_T)
#endif
#if defined(HTYPE_RLIM_T)
INTEGRAL_TYPE(CRLim,HTYPE_RLIM_T)
#endif

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere
INTEGRAL_TYPE(Fd,CInt)

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

