
/* This is a hack.  I totally deny writing it.  If this code breaks,
 * you get to keep all the pieces.  JRS, 23 feb 99.
 */

#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <malloc.h>
#include <stdlib.h>

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
   fflush(f);
}

int nh_read ( FILE* f )
{
   errno = 0;
   return fgetc(f);
}

int nh_errno ( void )
{
   return errno;
}

int nh_malloc ( int n )
{
   char* p = malloc(n);
   assert(p);
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

extern int prog_argc;
extern char** prog_argv;

int nh_argc ( void )
{
   return prog_argc;
}

int nh_argvb ( int argno, int offset )
{
   return (int)(prog_argv[argno][offset]);
}
