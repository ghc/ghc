-----------------------------------------------------------------------------
-- $Id: PrelPosixTypes.hsc,v 1.3 2001/02/27 10:03:22 rrt Exp $
-- 
-- (c) 2000
-- 
-- Module PrelPosixTypes

module PrelPosixTypes where

import PrelWord
import PrelInt

#include <sys/types.h>
#ifndef mingw32_TARGET_OS
#include <termios.h>
#endif

data CDir    = CDir

type CDev    = #type dev_t
type CIno    = #type ino_t
type CMode   = #type mode_t
type COff    = #type off_t
type CPid    = #type pid_t
#ifndef mingw32_TARGET_OS
type CGid    = #type gid_t
type CNlink  = #type nlink_t
type CSsize  = #type ssize_t
type CUid    = #type uid_t
type CCc     = #type cc_t
type CSpeed  = #type speed_t
type CTcflag = #type tcflag_t
#endif