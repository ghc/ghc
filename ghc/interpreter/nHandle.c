
/* This is a hack.  I totally deny writing it.  If this code breaks,
 * you get to keep all the pieces.  JRS, 23 feb 99.
 */

#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <malloc.h>
#include <stdlib.h>
#include <ctype.h>
#ifndef _WIN32
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <time.h>
#endif
#include <unistd.h>

#ifndef _WIN32
double nh_getCPUtime ( void )
{
   double usertime;
   struct rusage usage;
   getrusage ( RUSAGE_SELF, &usage );
   usertime = (double)usage.ru_utime.tv_sec +
              (double)usage.ru_utime.tv_usec / 1000000.0;
   return usertime;
}

double nh_getCPUprec ( void )
{
   /* or perhaps CLOCKS_PER_SEC ? */
   return 1.0 / (double)(CLK_TCK);
}
#else
double nh_getCPUtime ( void )
{
   return 1;
}

double nh_getCPUprec ( void )
{
   return 1;
}
#endif

int nh_getPID ( void )
{
#ifndef _WIN32
   return (int) getpid();
#else
   return (int) 0;
#endif
}

void nh_exitwith ( int code )
{
   exit(code);
}

int nh_system ( char* cmd )
{
   return system ( cmd );
}

int nh_iseof ( FILE* f )
{
   int c;
   errno = 0;
   c = fgetc ( f );
   if (c == EOF) return 1;
   ungetc ( c, f );
   return 0;
}

int nh_filesize ( FILE* f )
{
#ifndef _WIN32
   struct stat buf;
   errno = 0;
   fstat ( fileno(f), &buf );
   return buf.st_size;
#else
   errno = EPERM;
   return 0;
#endif
}

int nh_stdin ( void )
{
   errno = 0;
   return (int)stdin;
}

int nh_stdout ( void )
{
   errno = 0;
   return (int)stdout;
}

int nh_stderr ( void )
{
   errno = 0;
   return (int)stderr;
}

int nh_open ( char* fname, int wr )
{
   FILE* f;
   errno = 0;
   f = fopen ( fname, (wr==0) ? "r":  ((wr==1) ? "w" : "a") );
   return (int)f;
}

void nh_close ( FILE* f )
{
   errno = 0;
   fflush ( f );
   fclose ( f );
}

void nh_flush ( FILE* f )
{
   errno = 0;
   fflush ( f );
}

void nh_write ( FILE* f, int c )
{
   errno = 0;
   fputc(c,f);
   if (f==stderr) { fflush(f); } 
   if (f==stdout) { fflush(f); } 
}

int nh_read ( FILE* f )
{
   errno = 0;
   return fgetc(f);
}

int nh_errno ( void )
{
   int t = errno;
   errno = 0;
   return t;
}

int nh_malloc ( int n )
{
   char* p = malloc(n);
   return (int)p;
}

void nh_free ( int n )
{
   free ( (char*)n );
}

void nh_store ( int p, int ch )
{
   *(char*)p = (char)ch;
}

int nh_load ( int p )
{
   return (int)(*(char*)p);
}

int nh_getenv ( int p )
{
   return (int)getenv ( (const char *)p );
}

