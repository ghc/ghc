/* 
 * (c) The University of Glasgow 2001
 *
 * $Id: lockFile.h,v 1.1 2001/06/28 14:15:04 simonmar Exp $
 *
 * lockFile header
 */

int lockFile(int fd, int for_writing, int exclusive);
int unlockFile(int fd);
