/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1999
 *
 * $Id: setBinaryMode.c,v 1.1 1999/09/19 19:27:10 sof Exp $
 *
 * hSetBinaryMode runtime support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef _WIN32
#include <io.h>
#endif

StgInt
setBinaryMode__(ptr,flg)
StgForeignPtr ptr;
StgInt flg;
{
  IOFileObject* fo = (IOFileObject*)ptr;
  int rc;

  rc = flushBuffer(ptr);
  if (rc < 0) return rc;

#ifdef _WIN32
  setmode ( fo->fd, flg ? O_BINARY : O_TEXT );
#endif
  rc = (fo->flags & FILEOBJ_BINARY ? 1 : 0);
  fo->flags = fo->flags & (flg ? FILEOBJ_BINARY : ~FILEOBJ_BINARY);

  return rc;
}
