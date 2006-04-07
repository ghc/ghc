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
extern unsigned int addDelayRequest(int   msecs);
extern unsigned int addDoProcRequest(void* proc, void* param);
extern int  startupAsyncIO(void);
extern void shutdownAsyncIO(void);

extern int awaitRequests(rtsBool wait);

extern void abandonRequestWait(void);
extern void resetAbandonRequestWait(void);

#endif /* __ASYNCHIO_H__ */
