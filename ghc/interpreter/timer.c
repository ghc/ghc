/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * This file provides a simple mechanism for measuring elapsed time on Unix
 * machines (more precisely, on any machine with an rusage() function).
 * A somewhat limited version for other systems is also included, believed
 * to be ANSI compatible, but not guaranteed ...
 *
 * It is included in the Hugs distribution for the purpose of benchmarking
 * the Hugs interpreter, comparing its performance across a variety of
 * different machines, and with other systems for similar languages.
 *
 * To make use of these functions, use the --enable-timer when configuring
 * Hugs or change the setting of "WANT_TIMER" in config.h and recompile
 * Hugs.
 *
 * It would be somewhat foolish to try to use the timings produced in this
 * way for anything other than the purpose described above.  In particular,
 * using timings to compare the performance of different versions of an
 * algorithm is likely to give very misleading results.  The current
 * implementation of Hugs as an interpreter, without any significant
 * optimizations, means that there are much more significant overheads than
 * can be accounted for by small variations in Hugs code.
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: timer.c,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:46 $
 * ------------------------------------------------------------------------*/


#if defined(HAVE_SYS_TIME_H) && defined(HAVE_SYS_RESOURCE_H)
#include <sys/time.h>
#include <sys/resource.h>

void updateTimers Args((void));
long millisecs  Args((long));
long userElapsed, systElapsed;

void updateTimers() {
    static long lastUser = 0;
    static long lastSyst = 0;
    long curr;
    struct rusage ruse;
    getrusage(RUSAGE_SELF,&ruse);

    curr        = ruse.ru_utime.tv_sec*1000000L + ruse.ru_utime.tv_usec;
    userElapsed = curr - lastUser;
    lastUser    = curr;

    curr        = ruse.ru_stime.tv_sec*1000000L + ruse.ru_stime.tv_usec;
    systElapsed = curr - lastSyst;
    lastSyst    = curr;
}

long millisecs(t)
long t; {
    return (t+500)/1000;
}
#else
#include <time.h>

void updateTimers Args((void));
long millisecs    Args((clock_t));
clock_t userElapsed=0, systElapsed=0;

void updateTimers() {
    static clock_t lastUser = 0;
    clock_t curr;
    curr        = clock();
    userElapsed = curr - lastUser;
    lastUser    = curr;
}

long millisecs(t)
clock_t t; {
    return (long)((t * 1000)/CLK_TCK);
}
#endif

/*-------------------------------------------------------------------------*/
