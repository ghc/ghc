
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
 * $Revision: 1.7 $
 * $Date: 2000/03/15 23:27:16 $
 * ------------------------------------------------------------------------*/

extern Void internal     ( String) HUGS_noreturn;
extern Void fatal        ( String) HUGS_noreturn;

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
#define ERRFD(fd)	 Hilite(); printFD(errorStream,fd); Lolite()

extern Void errHead      ( Int );                  /* in main.c            */
extern Void errFail      ( Void) HUGS_noreturn;
extern Void errAbort     ( Void );
extern Cell errAssert    ( Int );

extern sigProto(breakHandler);

extern Bool breakOn      ( Bool );                 /* in machdep.c         */

extern Void printExp     ( FILE *,Cell );          /* in output.c          */
extern Void printType    ( FILE *,Cell );
extern Void printContext ( FILE *,List );
extern Void printPred    ( FILE *,Cell );
extern Void printKind    ( FILE *,Kind );
extern Void printKinds   ( FILE *,Kinds );
extern Void printFD	 ( FILE *,Pair );

/*-------------------------------------------------------------------------*/
