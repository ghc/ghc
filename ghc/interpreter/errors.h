
/* --------------------------------------------------------------------------
 * Error handling support functions
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: errors.h,v $
 * $Revision: 1.4 $
 * $Date: 1999/10/15 21:41:05 $
 * ------------------------------------------------------------------------*/

extern Void internal     Args((String)) HUGS_noreturn;
extern Void fatal        Args((String)) HUGS_noreturn;

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

extern Bool breakOn      Args((Bool));             /* in machdep.c         */

extern Void printExp     Args((FILE *,Cell));      /* in output.c          */
extern Void printType    Args((FILE *,Cell));
extern Void printContext Args((FILE *,List));
extern Void printPred    Args((FILE *,Cell));
extern Void printKind    Args((FILE *,Kind));
extern Void printKinds   Args((FILE *,Kinds));

/*-------------------------------------------------------------------------*/
