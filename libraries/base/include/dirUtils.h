/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

extern HsInt __hscore_readdir(HsAddr dirPtr, HsAddr pDirEnt);
extern HsInt __hscore_renameFile(HsAddr src, HsAddr dest);

#endif /* __DIRUTILS_H__ */
