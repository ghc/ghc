/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

extern int __hscore_readdir(DIR *dirPtr, struct dirent **pDirEnt);
extern int __hscore_renameFile(char *src, char *dest);

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
extern int __hscore_getFolderPath(HWND hwndOwner,
				  int nFolder,
				  HANDLE hToken,
				  DWORD dwFlags,
				  char*  pszPath);
#endif

#endif /* __DIRUTILS_H__ */
