/* AsyncIO.h
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 */

#ifndef WIN32_ASYNCHIO_H
#define WIN32_ASYNCHIO_H

extern unsigned int
addIORequest(int   fd,
	     int   forWriting,
	     int   isSock,
	     int   len,
	     char* buf);
extern unsigned int addDelayRequest(int   usecs);
extern unsigned int addDoProcRequest(void* proc, void* param);
extern int  startupAsyncIO(void);
extern void shutdownAsyncIO(rtsBool wait_threads);

extern int awaitRequests(rtsBool wait);

extern void abandonRequestWait(void);
extern void resetAbandonRequestWait(void);

#endif /* WIN32_ASYNCHIO_H */
