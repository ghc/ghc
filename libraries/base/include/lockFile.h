/*
 * (c) The University of Glasgow 2001
 *
 * $Id: lockFile.h,v 1.3 2005/01/28 13:36:34 simonmar Exp $
 *
 * lockFile header
 */

#ifndef mingw32_HOST_OS

int lockFile(int fd, int for_writing, int exclusive);
int unlockFile(int fd);

#endif
