/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: fileLookAhead.c,v 1.4 1999/11/25 16:54:14 simonmar Exp $
 *
 * hLookAhead Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

StgInt
fileLookAhead(StgForeignPtr ptr)
{
    IOFileObject* fo = (IOFileObject*)ptr;
    int c, rc;
   
#if 0
    fprintf(stderr, "flh: %d %d %d\n",fo->bufRPtr, fo->bufWPtr, fo->flags);
#endif

    /* 
     * fileLookahead reads the next character (hopefully from the buffer),
     * before putting it back and returning the char.
     *
     */

    if ( FILEOBJ_IS_EOF(fo) ) {
       ghc_errtype = ERR_EOF;
       ghc_errstr = "";
       return -1;
    }

    if ( (c = fileGetc(ptr)) < 0 ) {
         return c;
    }

    rc = ungetChar(ptr,(char)c);
    if ( rc < 0 ) {
       return rc;
    } else {
       return c;
    }
}

StgInt
ungetChar(StgForeignPtr ptr, StgChar c)
{
  IOFileObject* fo = (IOFileObject*)ptr;
  int rc = 0, sz = 0;

#if 0
  fprintf(stderr, "ug: %d %d %c\n",fo->bufRPtr, fo->bufWPtr,(char)c, fo->flags);
#endif

  /* Sanity check */
  if ( !FILEOBJ_READABLE(fo) ) {
      ghc_errno  = GHC_EINVAL;
      ghc_errstr = "object not readable";
      return -1;
  }

  /* For an unbuffered file object, we lazily
     allocate a pushback buffer. The sizeof the pushback
     buffer is (globally) configurable.
  */
  sz = getPushbackBufSize();
  if ( FILEOBJ_UNBUFFERED(fo) && fo->buf==NULL && sz > 0 ) {
     if ((fo->buf = malloc(sz*sizeof(char))) == NULL ) {
     	return -1;
     }
     fo->bufSize = sz;
     ((unsigned char*)fo->buf)[sz-1]=(unsigned char)c;
     fo->bufWPtr = sz;    /* Points one past the end of pushback buffer */
     fo->bufRPtr = sz-1;  /* points to current char. */
     return 0;
  }

  if ( fo->bufWPtr > 0 && fo->bufRPtr > 0 ) {
    fo->bufRPtr -= 1;
    ((unsigned char*)fo->buf)[fo->bufRPtr]=(unsigned char)c;
    return 0;
  } else if ( fo->buf != NULL  && 
  	      fo->bufSize > 0  &&
              fo->bufWPtr == 0 && 
	      fo->bufRPtr==0    ) { /* empty buffer waiting to be filled up */
     fo->bufRPtr=fo->bufSize-1;
     ((unsigned char*)fo->buf)[fo->bufRPtr]=(unsigned char)c;
     fo->bufWPtr=fo->bufSize;
     return 0;
  } else {
    return -1;
  }
}
