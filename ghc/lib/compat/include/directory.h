#ifndef __DIRECTORY_H__
#define __DIRECTORY_H__ 

#if defined(mingw32_HOST_OS)
extern int __compat_long_path_size();
extern int __hscore_CSIDL_APPDATA();
extern int __hscore_getFolderPath(HWND hwndOwner,
				  int nFolder,
				  HANDLE hToken,
				  DWORD dwFlags,
				  char*  pszPath);
#endif
#endif
