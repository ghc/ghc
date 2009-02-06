/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__
#include <sys/types.h>
#include <dirent.h>

extern int __hscore_readdir(DIR *dirPtr, struct dirent **pDirEnt);

#endif /* __DIRUTILS_H__ */
