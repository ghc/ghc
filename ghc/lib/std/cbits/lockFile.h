/* 
 * (c) The University of Glasgow 2001
 *
 * $Id: lockFile.h,v 1.1 2001/05/18 16:54:06 simonmar Exp $
 *
 * lockFile header
 */

int lockFile(int fd, int for_writing, int exclusive);
int unlockFile(int fd);
