/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

extern HsInt __hscore_readdir(HsAddr dirPtr, HsAddr pDirEnt);
extern HsInt __hscore_renameFile(HsAddr src, HsAddr dest);

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
extern int __hscore_getFolderPath(HWND hwndOwner,
				  int nFolder,
				  HANDLE hToken,
				  DWORD dwFlags,
				  char*  pszPath);
#endif

#endif /* __DIRUTILS_H__ */
