/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-
 *
 * IO / Handle support.
 */
#include "HsStd.h"
#include "PrelIOUtils.h"
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#ifndef offsetof
#define offsetof(t, f) ((size_t) &((t *)0)->f)
#endif

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

HsBool prel_supportsTextMode()
{
#if defined(mingw32_TARGET_OS)
  return HS_BOOL_FALSE;
#else
  return HS_BOOL_TRUE;
#endif
}

HsInt prel_bufsiz()
{
  return BUFSIZ;
}

HsInt prel_seek_cur()
{
  return SEEK_CUR;
}

HsInt prel_o_binary()
{
#ifdef HAVE_O_BINARY
  return O_BINARY;
#else
  return 0;
#endif
}

HsInt prel_seek_set()
{
  return SEEK_SET;
}

HsInt prel_seek_end()
{
  return SEEK_END;
}

HsInt prel_setmode(HsInt fd, HsBool toBin)
{
#ifdef _WIN32
  return setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif  
}

HsInt prel_PrelHandle_write(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz)
{
#ifdef _WIN32
  if (isSock) {
    return send(fd,ptr + off, sz, 0);
  }
#endif
  return write(fd,ptr + off, sz);
}

HsInt prel_PrelHandle_read(HsInt fd, HsBool isSock, HsAddr ptr, HsInt off, int sz)
{
#ifdef _WIN32
  if (isSock) {
    return recv(fd,ptr + off, sz, 0);
  }
#endif
  return read(fd,ptr + off, sz);

}

void *prel_PrelIO_memcpy(char *dst, HsInt dst_off, const char *src, size_t sz)
{ 
  return memcpy(dst+dst_off, src, sz);
}


int s_isreg_PrelPosix_wrap(int m) { return S_ISREG(m); }
int s_isdir_PrelPosix_wrap(int m) { return S_ISDIR(m); }
int s_isfifo_PrelPosix_wrap(int m) { return S_ISFIFO(m); }
int s_isblk_PrelPosix_wrap(int m) { return S_ISBLK(m); }
int s_ischr_PrelPosix_wrap(int m) { return S_ISCHR(m); }
#ifndef mingw32_TARGET_OS
int s_issock_PrelPosix_wrap(int m) { return S_ISSOCK(m); }
void sigemptyset_PrelPosix_wrap(sigset_t *set) { sigemptyset(set); }
#endif
