/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1998
 *
 * $Id: directoryAux.c,v 1.2 1998/12/02 13:27:17 simonm Exp $
 *
 * Support functions for manipulating directories
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

StgAddr
openDir__(StgByteArray path)
{
    struct stat sb;
    DIR *dir;

    /* Check for an actual directory */
    while (stat(path, &sb) != 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return NULL;
	}
    }
    if (!S_ISDIR(sb.st_mode)) {
	ghc_errtype = ERR_INAPPROPRIATETYPE;
	ghc_errstr = "not a directory";
	return NULL;
    }

    while ((dir = opendir(path)) == NULL) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return NULL;
	}
    }
    return dir;
}

StgAddr
readDir__(StgAddr dir)

{
   struct dirent *d;
   while ((d = readdir((DIR*)dir)) == NULL) {
    if (errno == 0) {
	(void) closedir((DIR*)dir);
	return NULL;
    } else if (errno != EINTR) {
        cvtErrno();
        stdErrno();
	(void) closedir((DIR*)dir);
	return NULL;
    }
    errno = 0;
  }
  return d;
}

StgAddr 
get_dirent_d_name(StgAddr d)
{
    return ((struct dirent*)d)->d_name;
}

StgInt const_F_OK( void ) { return F_OK; }

StgInt sizeof_stat( void ) { return sizeof(struct stat); }

StgInt  prim_stat(StgAddr x, StgAddr y)
{
    return stat((char*)x, (struct stat*)y);
}


StgWord 
get_stat_st_mode  (StgAddr x)
{
    return ((struct stat *)x)->st_mode;
}


StgInt64
get_stat_st_mtime(StgAddr x)
{
  return ((struct stat *)x)->st_mtime;
}

void
set_stat_st_mtime(StgByteArray p, StgByteArray x)
{
  ((unsigned long *)p)[0] = ((struct stat *)x)->st_mtime;
  return;
}

StgWord const_S_IRUSR( void ) { return S_IRUSR; }
StgWord const_S_IWUSR( void ) { return S_IWUSR; }
StgWord const_S_IXUSR( void ) { return S_IXUSR; }

StgInt  
prim_S_ISDIR( StgWord x )
{ 
    return S_ISDIR(x);
}

StgInt  
prim_S_ISREG( StgWord x )
{ 
    return S_ISREG(x);
}

