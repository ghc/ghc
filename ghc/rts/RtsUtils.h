/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSUTILS_H
#define RTSUTILS_H

/* -----------------------------------------------------------------------------
 * (Checked) dynamic allocation
 * -------------------------------------------------------------------------- */

extern void *stgMallocBytes(int n, char *msg)
    GNUC3_ATTRIBUTE(__malloc__);

extern void *stgReallocBytes(void *p, int n, char *msg);

extern void *stgCallocBytes(int n, int m, char *msg)
     GNUC3_ATTRIBUTE(__malloc__);

extern void stgFree(void* p);

/* -----------------------------------------------------------------------------
 * Misc other utilities
 * -------------------------------------------------------------------------- */

extern void heapOverflow(void);

extern void setNonBlockingFd(int fd);
extern void resetNonBlockingFd(int fd);

extern nat stg_strlen(char *str);

extern char *time_str(void);
extern char *ullong_format_string(ullong, char *, rtsBool);

#ifdef PAR
extern ullong msTime(void);
#endif

#ifdef DEBUG
extern void heapCheckFail( void );
#endif

extern void* __hscore_get_saved_termios(int fd);
extern void __hscore_set_saved_termios(int fd, void* ts);

#endif /* RTSUTILS_H */
