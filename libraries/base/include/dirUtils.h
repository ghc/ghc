/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

extern HsInt __hscore_readdir(HsAddr dirPtr, HsAddr pDirEnt);
extern HsInt __hscore_renameFile(HsAddr src, HsAddr dest);

#if defined(mingw32_HOST_OS)
extern int __hscore_getFolderPath(HWND hwndOwner,
				  int nFolder,
				  HANDLE hToken,
				  DWORD dwFlags,
				  char*  pszPath);
#endif

#endif /* __DIRUTILS_H__ */
