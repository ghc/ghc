/*
 * (c) The University of Glasgow 2001
 *
 * $Id: lockFile.h,v 1.2 2005/01/02 00:00:00 krasimir Exp $
 *
 * lockFile header
 */

#ifndef mingw32_TARGET_OS

int lockFile(int fd, int for_writing, int exclusive);
int unlockFile(int fd);

#endif
