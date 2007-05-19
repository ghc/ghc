/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

extern int __hscore_readdir(DIR *dirPtr, struct dirent **pDirEnt);
extern int __hscore_renameFile(char *src, char *dest);

#endif /* __DIRUTILS_H__ */
