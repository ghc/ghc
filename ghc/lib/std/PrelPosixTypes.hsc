-----------------------------------------------------------------------------
-- $Id: PrelPosixTypes.hsc,v 1.2 2001/01/12 15:48:36 simonmar Exp $
-- 
-- (c) 2000
-- 
-- Module PrelPosixTypes

module PrelPosixTypes where

import PrelWord
import PrelInt

#include <sys/types.h>
#include <termios.h>

data CDir    = CDir

type CDev    = #type dev_t
type CGid    = #type gid_t
type CIno    = #type ino_t
type CMode   = #type mode_t
type CNlink  = #type nlink_t
type COff    = #type off_t
type CPid    = #type pid_t
type CSsize  = #type ssize_t
type CUid    = #type uid_t
type CCc     = #type cc_t
type CSpeed  = #type speed_t
type CTcflag = #type tcflag_t
