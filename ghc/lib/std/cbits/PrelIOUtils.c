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

int prel_o_binary()
{
#ifdef HAVE_O_BINARY
  return O_BINARY;
#else
  return 0;
#endif
}

int prel_o_rdonly()
{
#ifdef O_RDONLY
  return O_RDONLY;
#else
  return 0;
#endif
}

int prel_o_wronly()
{
#ifdef O_WRONLY
  return O_WRONLY;
#else
  return 0;
#endif
}

int prel_o_rdwr()
{
#ifdef O_RDWR
  return O_RDWR;
#else
  return 0;
#endif
}

int prel_o_append()
{
#ifdef O_APPEND
  return O_APPEND;
#else
  return 0;
#endif
}

int prel_o_creat()
{
#ifdef O_CREAT
  return O_CREAT;
#else
  return 0;
#endif
}

int prel_o_excl()
{
#ifdef O_EXCL
  return O_EXCL;
#else
  return 0;
#endif
}

int prel_o_trunc()
{
#ifdef O_TRUNC
  return O_TRUNC;
#else
  return 0;
#endif
}

int prel_o_noctty()
{
#ifdef O_NOCTTY
  return O_NOCTTY;
#else
  return 0;
#endif
}

int prel_o_nonblock()
{
#ifdef O_NONBLOCK
  return O_NONBLOCK;
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

void *prel_PrelIO_memcpy(char *dst, HsInt dst_off, const char *src, HsInt src_off, size_t sz)
{ 
  return memcpy(dst+dst_off, src+src_off, sz);
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

HsInt prel_sizeof_stat()
{
  return sizeof(struct stat);
}

time_t prel_st_mtime(struct stat* st) { return st->st_mtime; }
off_t  prel_st_size(struct stat* st) { return st->st_size; }
mode_t prel_st_mode(struct stat* st) { return st->st_mode; }

#if HAVE_TERMIOS_H
tcflag_t prel_lflag(struct termios* ts) { return ts->c_lflag; }
void     prel_poke_lflag(struct termios* ts, tcflag_t t) { ts->c_lflag = t; }
unsigned char* prel_ptr_c_cc(struct termios* ts) { return ((unsigned char*)(ts + offsetof(struct termios, c_cc))); }
#endif

HsInt prel_sizeof_termios()
{
#ifndef mingw32_TARGET_OS
  return sizeof(struct termios);
#else
  return 0;
#endif
}

HsInt prel_sizeof_sigset_t()
{
#ifndef mingw32_TARGET_OS
  return sizeof(sigset_t);
#else
  return 0;
#endif
}

int prel_echo()
{
#ifdef ECHO
  return ECHO;
#else
  return 0;
#endif

}
int prel_tcsanow()
{
#ifdef TCSANOW
  return TCSANOW;
#else
  return 0;
#endif

}

int prel_icanon()
{
#ifdef ICANON
  return ICANON;
#else
  return 0;
#endif
}

int prel_vmin()
{
#ifdef VMIN
  return VMIN;
#else
  return 0;
#endif
}

int prel_vtime()
{
#ifdef VTIME
  return VTIME;
#else
  return 0;
#endif
}

int prel_sigttou()
{
#ifdef SIGTTOU
  return SIGTTOU;
#else
  return 0;
#endif
}

int prel_sig_block()
{
#ifdef SIG_BLOCK
  return SIG_BLOCK;
#else
  return 0;
#endif
}

int prel_sig_setmask()
{
#ifdef SIG_SETMASK
  return SIG_SETMASK;
#else
  return 0;
#endif
}

int prel_f_getfl()
{
#ifdef F_GETFL
  return F_GETFL;
#else
  return 0;
#endif
}

int prel_f_setfl()
{
#ifdef F_SETFL
  return F_SETFL;
#else
  return 0;
#endif
}


