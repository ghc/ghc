/* AsyncIO.h
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 */
#ifndef __ASYNCHIO_H__
#define __ASYNCHIO_H__
extern unsigned int
addIORequest(int   fd,
	     int   forWriting,
	     int   isSock,
	     int   len,
	     char* buf);

extern int  startupAsyncIO(void);
extern void shutdownAsyncIO(void);

extern int awaitRequests(rtsBool wait);

#endif /* __ASYNCHIO_H__ */
