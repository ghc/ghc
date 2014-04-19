/* 
 * (c) The University of Glasgow 2002
 *
 * Win32 Console API support
 */
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32) || defined(__CYGWIN__)
/* to the end */

#include "consUtils.h"
#include <windows.h>
#include <io.h>

#if defined(__CYGWIN__)
#define _get_osfhandle get_osfhandle
#endif

int is_console__(int fd) {
    DWORD st;
    HANDLE h;
    if (!_isatty(fd)) {
        /* TTY must be a character device */
        return 0;
    }
    h = (HANDLE)_get_osfhandle(fd);
    if (h == INVALID_HANDLE_VALUE) {
        /* Broken handle can't be terminal */
        return 0;
    }
    if (!GetConsoleMode(h, &st)) {
        /* GetConsoleMode appears to fail when it's not a TTY.  In
           particular, it's what most of our terminal functions
           assume works, so if it doesn't work for all intents
           and purposes we're not dealing with a terminal. */
        return 0;
    }
    return 1;
}


int
set_console_buffering__(int fd, int cooked)
{
    HANDLE h;
    DWORD  st;
    /* According to GetConsoleMode() docs, it is not possible to
       leave ECHO_INPUT enabled without also having LINE_INPUT,
       so we have to turn both off here. */
    DWORD flgs = ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;
    
    if ( (h = (HANDLE)_get_osfhandle(fd)) != INVALID_HANDLE_VALUE ) {
	if ( GetConsoleMode(h,&st) &&
	     SetConsoleMode(h, cooked ? (st | ENABLE_LINE_INPUT) : st & ~flgs)  ) {
	    return 0;
	}
    }
    return -1;
}

int
set_console_echo__(int fd, int on)
{
    HANDLE h;
    DWORD  st;
    DWORD flgs = ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;
    
    if ( (h = (HANDLE)_get_osfhandle(fd)) != INVALID_HANDLE_VALUE ) {
	if ( GetConsoleMode(h,&st) && 
	     SetConsoleMode(h,( on ? (st | flgs) : (st & ~ENABLE_ECHO_INPUT))) ) {
	    return 0;
	}
    }
    return -1;
}

int
get_console_echo__(int fd)
{
    HANDLE h;
    DWORD  st;
    
    if ( (h = (HANDLE)_get_osfhandle(fd)) != INVALID_HANDLE_VALUE ) {
	if ( GetConsoleMode(h,&st) ) {
	    return (st & ENABLE_ECHO_INPUT ? 1 : 0);
	}
    }
    return -1;
}

int
flush_input_console__(int fd)
{
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    
    if ( h != INVALID_HANDLE_VALUE ) {
	/* If the 'fd' isn't connected to a console; treat the flush
	 * operation as a NOP.
	 */
	DWORD unused;
	if ( !GetConsoleMode(h,&unused) &&
	     GetLastError() == ERROR_INVALID_HANDLE ) {
	    return 0;
	}
	if ( FlushConsoleInputBuffer(h) ) {
	    return 0;
	}
    }
    /* ToDo: translate GetLastError() into something errno-friendly */
    return -1;
}

#endif /* defined(__MINGW32__) || ... */
