/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-
 *
 * Directory Runtime Support
 */
#include "dirUtils.h"

#if defined(mingw32_TARGET_OS)
#include <windows.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

HsInt
prel_mkdir(HsAddr pathName, HsInt mode)
{
#if defined(mingw32_TARGET_OS)
  return mkdir(pathName);
#else
  return mkdir(pathName,mode);
#endif
}

HsInt
prel_lstat(HsAddr fname, HsAddr st)
{
#ifdef HAVE_LSTAT
  return lstat((const char*)fname, (struct stat*)st);
#else
  return stat((const char*)fname, (struct stat*)st);
#endif
}

HsInt prel_s_ISDIR(mode_t m) {return S_ISDIR(m);}
HsInt prel_s_ISREG(mode_t m) {return S_ISREG(m);}

HsInt prel_sz_stat()  { return sizeof(struct stat); }
HsInt prel_path_max() { return PATH_MAX; }
mode_t prel_R_OK() { return R_OK; }
mode_t prel_W_OK() { return W_OK; }
mode_t prel_X_OK() { return X_OK; }

mode_t prel_S_IRUSR() { return S_IRUSR; }
mode_t prel_S_IWUSR() { return S_IWUSR; }
mode_t prel_S_IXUSR() { return S_IXUSR; }

time_t prel_st_mtime(struct stat* st) { return st->st_mtime; }
mode_t prel_st_mode(struct stat* st) { return st->st_mode; }

HsAddr prel_d_name(struct dirent* d)
{ 
#ifndef mingw32_TARGET_OS
  return (HsAddr)(&d->d_name);
#else
  return (HsAddr)(d->d_name);
#endif
}

HsInt prel_end_of_dir()
{
#ifndef mingw32_TARGET_OS
  return 0;
#else
  return ENOENT;
#endif  
}

