/* -----------------------------------------------------------------------------
 * $Id: RtsUtils.h,v 1.21 2004/03/23 10:04:18 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSUTILS_H
#define RTSUTILS_H

/* (Checked) dynamic allocation: */
extern void *stgMallocBytes(int n, char *msg) GNUC3_ATTRIBUTE(__malloc__);
extern void *stgReallocBytes(void *p, int n, char *msg);
extern void *stgCallocBytes(int n, int m, char *msg) GNUC3_ATTRIBUTE(__malloc__);
extern void stgFree(void* p);

extern void barf(char *s, ...) GNU_ATTRIBUTE(__noreturn__);
extern void belch(char *s, ...);
extern void prog_belch(char *s, ...);

extern void _stgAssert (char *filename, unsigned int linenum);

extern void heapOverflow(void);

extern void setNonBlockingFd(int fd);
extern void resetNonBlockingFd(int fd);

extern nat stg_strlen(char *str);

char *time_str(void);
char *ullong_format_string(ullong, char *, rtsBool);

#ifdef PAR
ullong   msTime(void);
#endif

#ifdef DEBUG
extern void heapCheckFail( void );
#endif

extern void* __hscore_get_saved_termios(int fd);
extern void __hscore_set_saved_termios(int fd, void* ts);

#endif // RTSUTILS_H
