/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSUTILS_H
#define RTSUTILS_H

#include <stdarg.h>

/* -----------------------------------------------------------------------------
 * Message generation
 * -------------------------------------------------------------------------- */

/*
 * A fatal internal error: this is for errors that probably indicate
 * bugs in the RTS or compiler.  We normally output bug reporting
 * instructions along with the error message.
 */
extern void barf(char *s, ...) 
   GNUC3_ATTRIBUTE(__noreturn__);

/*
 * An error condition which is caused by and/or can be corrected by
 * the user.
 */
extern void errorBelch(char *s, ...)
   GNUC3_ATTRIBUTE(format (printf, 1, 2));

/*
 * A debugging message.  Debugging messages are generated either as a
 * virtue of having DEBUG turned on, or by being explicitly selected
 * via RTS options (eg. +RTS -Ds).
 */
extern void debugBelch(char *s, ...)
   GNUC3_ATTRIBUTE(format (printf, 1, 2));

/* Version of debugBelch() that takes parameters as a va_list */
extern void vdebugBelch(char *s, va_list ap);

/* Hooks for redirecting message generation: */

typedef void RtsMsgFunction(char *, va_list);

extern RtsMsgFunction *fatalInternalMsgFn;
extern RtsMsgFunction *debugMsgFn;
extern RtsMsgFunction *errorMsgFn;

/* Default stdio implementation of the message hooks: */

extern RtsMsgFunction stdioFatalInternalMsgFn;
extern RtsMsgFunction stdioDebugMsgFn;
extern RtsMsgFunction stdioErrorMsgFn;

/* -----------------------------------------------------------------------------
 * (Checked) dynamic allocation
 * -------------------------------------------------------------------------- */

extern void *stgMallocBytes(int n, char *msg) GNUC3_ATTRIBUTE(__malloc__);
extern void *stgReallocBytes(void *p, int n, char *msg);
extern void *stgCallocBytes(int n, int m, char *msg) GNUC3_ATTRIBUTE(__malloc__);
extern void stgFree(void* p);


/* -----------------------------------------------------------------------------
 * Misc other utilities
 * -------------------------------------------------------------------------- */

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
