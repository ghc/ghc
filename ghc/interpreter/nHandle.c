
/* This is a hack.  I totally deny writing it.  If this code breaks,
 * you get to keep all the pieces.  JRS, 23 feb 99.
 */

#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <malloc.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

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

int nh_getPID ( void )
{
   return (int) getpid();
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
   struct stat buf;
   errno = 0;
   fstat ( fileno(f), &buf );
   return buf.st_size;
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

#if 0
int prog_argc;
char** prog_argv;

int nh_init_args ( int  argc, char *argv[] ) 
{
  prog_argc = argc;
  prog_argv = argv;
}

int nh_argc ( void )
{
  return prog_argc;
}

int nh_argvb ( int argno, int offset )
{
  return prog_argv[argno][offset];
}
#endif
