#ifndef FILEOBJECT_H
#define FILEOBJECT_H

/*
  IOFileObjects are used as part of the IO.Handle
  implementation, ensuring that when handles are
  finalised, buffers are flushed and FILE* objects
  are closed (we really should be using file descriptors
  here..)
  
 */

typedef struct _IOFileObject {
   int     fd;
   void*   buf;

   int     bufStart; /* offset of start of data waiting to
			be written.  This may be non-zero in
			the case where we wrote out some of the
			buffer, and then blocked.

			NOTE: this field should be non-zero *only*
			when we just blocked on a call to writeBuffer,
			and we're going to restart the call when
			we unblock.  It should be zero at all other
			times.
		     */

   int     bufWPtr;  /* points to next position to write,
   			  bufRPtr >= bufWPtr <= bufSize.
			  
			For read-only files, bufWPtr = bufSize

			bufWPtr = 0 => buffer is empty.

		     */
   int     bufRPtr;  /* points to the next char to read 
   			  -1 >= bufRPtr <= bufWPtr 
			  
		        For write-only files, bufRPtr = 0

			bufRPtr == -1 => buffer is empty.
		     */
   int     bufSize;
   int     flags;
   struct _IOFileObject*   connectedTo;
} IOFileObject;

#define FILEOBJ_LB       2
#define FILEOBJ_BB       4
#define FILEOBJ_EOF      8
#define FILEOBJ_READ    16
#define FILEOBJ_WRITE   32
#define FILEOBJ_STD     64
/* The next two flags are used for RW file objects only.
   They indicate whether the last operation was a read or a write.
   (Need this info to determine whether a RW file object's
    buffer should be flushed before doing a subsequent
    read or write).
*/
#define FILEOBJ_RW_READ 256
#define FILEOBJ_RW_WRITE 512
/* 
 * Under Win32, a file fd is not the same as a socket fd, so
 * we need to use separate r/w calls.
 */ 
#define FILEOBJ_WINSOCK  1024
#define FILEOBJ_BINARY   2048

#define FILEOBJ_IS_EOF(x)     ((x)->flags & FILEOBJ_EOF)
#define FILEOBJ_SET_EOF(x)    ((x)->flags |= FILEOBJ_EOF)
#define FILEOBJ_CLEAR_EOF(x)  ((x)->flags &= ~FILEOBJ_EOF)
#define FILEOBJ_CLEAR_ERR(x)  FILEOBJ_CLEAR_EOF(x)

#define FILEOBJ_BLOCKED_READ   -5
#define FILEOBJ_BLOCKED_WRITE  -6
#define FILEOBJ_BLOCKED_CONN_WRITE  -7

#define FILEOBJ_UNBUFFERED(x)     (!((x)->flags & FILEOBJ_LB) && !((x)->flags & FILEOBJ_BB))
#define FILEOBJ_LINEBUFFERED(x)   ((x)->flags & FILEOBJ_LB)
#define FILEOBJ_BLOCKBUFFERED(x)  ((x)->flags & FILEOBJ_BB)
#define FILEOBJ_BUFFER_FULL(x)    ((x)->bufWPtr >= (x)->bufSize)
#define FILEOBJ_BUFFER_EMPTY(x)   ((x)->bufRPtr == (x)->bufWPtr)
#define FILEOBJ_HAS_PUSHBACKS(x)  ((x)->buf != NULL && (x)->bufRPtr >= 0 && (x)->bufRPtr < (x)->bufWPtr)
#define FILEOBJ_READABLE(x)       ((x)->flags & FILEOBJ_READ)
#define FILEOBJ_WRITEABLE(x)      ((x)->flags & FILEOBJ_WRITE)
#define FILEOBJ_JUST_READ(x)      ((x)->flags & FILEOBJ_RW_READ)
#define FILEOBJ_JUST_WRITTEN(x)   ((x)->flags & FILEOBJ_RW_WRITE)
#define FILEOBJ_NEEDS_FLUSHING(x) (!FILEOBJ_BUFFER_EMPTY(x))
#define FILEOBJ_RW(x)		  (FILEOBJ_READABLE(x) && FILEOBJ_WRITEABLE(x))

#endif /* FILEOBJECT_H */
