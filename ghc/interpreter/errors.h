/* -*- mode: hugs-c; -*- */
/* --------------------------------------------------------------------------
 * Error handling support functions
 *
 * Copyright (c) The University of Nottingham and Yale University, 1994-1997.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * Hugs version 1.4, December 1997
 *
 * $RCSfile: errors.h,v $
 * $Revision: 1.2 $
 * $Date: 1998/12/02 13:22:07 $
 * ------------------------------------------------------------------------*/

extern Void internal   Args((String)) HUGS_noreturn;
extern Void fatal      Args((String)) HUGS_noreturn;

#if HUGS_FOR_WINDOWS
#define Hilite()         WinTextcolor(hWndText,RED);
#define Lolite()         WinTextcolor(hWndText,BLACK);
#define errorStream      stderr
#else
#define Hilite()         doNothing()
#define Lolite()         doNothing()
#define errorStream      stdout
#endif

#define ERRMSG(l)        Hilite(); errHead(l); FPrintf(errorStream,
#define EEND             ); Lolite(); errFail()
#define ETHEN            );
#define ERRTEXT          Hilite(); FPrintf(errorStream,
#define ERREXPR(e)       Hilite(); printExp(errorStream,e); Lolite()
#define ERRTYPE(e)       Hilite(); printType(errorStream,e); Lolite()
#define ERRCONTEXT(qs)   Hilite(); printContext(errorStream,qs); Lolite()
#define ERRPRED(pi)      Hilite(); printPred(errorStream,pi); Lolite()
#define ERRKIND(k)       Hilite(); printKind(errorStream,k); Lolite()
#define ERRKINDS(ks)     Hilite(); printKinds(errorStream,ks); Lolite()

extern Void errHead      Args((Int));              /* in main.c            */
extern Void errFail      Args((Void)) HUGS_noreturn;
extern Void errAbort     Args((Void));

extern sigProto(breakHandler);

#include "output.h"

/*-------------------------------------------------------------------------*/
