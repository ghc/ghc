/* 
 * (c) The GHC Team 2001
 *
 * $Id: ilxstubs.c,v 1.2 2001/12/21 15:07:26 simonmar Exp $
 *
 * ILX stubs for external function calls
 */

/*
  All foreign imports from the C standard library are stubbed out here,
  so that they are all in the same DLL (HSstd_cbits), and the ILX code
  generator doesn't have to be told or guess which DLL they are in.
  Calls to the Win32 API are annotated with the DLL they come from.

  The general rule is that all foreign imports are assumed to be in
  <current_package>_cbits.dll unless a DLL is explicitly given.
*/


#include "Stg.h"
#include "HsStd.h"
#include <stdlib.h>
#include <stddef.h>
#include <dirent.h>
#include <limits.h>

/* From the RTS */

    /* StgPrimFloat Add to mini-RTS, which is put in a DLL */

    /* Need to be implemented in ILX RTS */
/*../PrelStable.lhs:37:foreign import unsafe freeStablePtr :: StablePtr a -> IO ()
../PrelTopHandler.lhs:49:foreign import ccall "shutdownHaskellAndExit" 
../PrelTopHandler.lhs:77:foreign import ccall "stackOverflow" unsafe
../PrelTopHandler.lhs:80:foreign import ccall "stg_exit" unsafe */

void
stg_exit(I_ n)
{
  fprintf(stderr, "doing stg_exit(%d)\n", n);
  exit(n);
}

/* The code is in includes/Stable.h [sic] */
void
freeStablePtr(StgStablePtr sp)
{
  fprintf(stderr, "Freeing stable ptr %p (NOT!)\n", sp);
}

void
shutdownHaskellAndExit(int n)
{
  stg_exit(n);
}

void 
stackOverflow(void)
{
}

void *
_ErrorHdrHook(void)
{
  return &ErrorHdrHook;
}

void
ErrorHdrHook(long fd)
{
    const char msg[] = "\nFail: ";
    write(fd, msg, sizeof(msg)-1);
}



/* Import directly from correct DLL */

     /*../CPUTime.hsc:107:foreign import "GetCurrentProcess" unsafe getCurrentProcess :: IO (Ptr HANDLE)
       ../CPUTime.hsc:108:foreign import "GetProcessTimes" unsafe getProcessTimes :: Ptr HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO CInt */

int s_mkdir(const char *s) { return mkdir(s); }
int s_chmod(const char *s, mode_t m) { return chmod(s, m); }
int s_access(const char *s, int m) { return access(s, m); }
char *s_getcwd(char *s, size_t n) { return getcwd(s, n); }
int s_rmdir(const char *s) { return rmdir(s); }
int s_chdir(const char *s) { return chdir(s); }
int s_unlink(const char *s) { return unlink(s); }
int s_rename(const char *s1, const char *s2) { return rename(s1, s2); }
DIR *s_opendir(const char *s) { return opendir(s); }
struct dirent *s_readdir(DIR *d) { return readdir(d); }
int s_closedir(DIR *d) { return closedir(d); }
int s_stat(const char *s, struct stat *buf) { return stat(s, buf); }
int s_fstat(int f, struct stat* buf) { return fstat(f, buf); }
int s_open(const char *s, int f) { return open(s, f); }
int s_close(int f) { return close(f); }
int s_write(int f, const void *buf, size_t n) { return write(f, buf, n); }
int s_read(int f, void *buf, size_t n) { return read(f, buf, n); }
int s_lseek(int f, off_t off, int w) { return lseek(f, off, w); }
int s_isatty(int f) { return isatty(f); }
void *s_memcpy(void *d, const void *s, size_t n) { return memcpy(d, s, n); }
void *s_memmove(void *d, const void *s, size_t n) { return memmove(d, s, n); }
char *s_strerror(int e) { return strerror(e); }
int s_setmode(int a, int b) { return setmode(a,b); }
void *s_malloc(size_t n) { return malloc(n); }
void *s_realloc(void *p, size_t n) { return realloc(p, n); }
void s_free(void *p) { free(p); }
char *s_getenv(const char *s) { return getenv(s); }
struct tm *s_localtime(const time_t *p) { return localtime(p); }
struct tm *s_gmtime(const time_t *p) { return gmtime(p); }
time_t s_mktime(struct tm *p) { return mktime(p); }
time_t s_time(time_t *p) { return time(p); }
void s_ftime(struct timeb *p) { ftime(p); }
