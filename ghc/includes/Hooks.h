/* -----------------------------------------------------------------------------
 * $Id: Hooks.h,v 1.6 2001/10/01 11:36:28 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

extern char *ghc_rts_opts;

extern void OnExitHook (void);
extern void ErrorHdrHook (long fd);
extern int  NoRunnableThreadsHook (void);
extern void StackOverflowHook (unsigned long stack_size);
extern void OutOfHeapHook (unsigned long request_size, unsigned long heap_size);
extern void MallocFailHook (unsigned long request_size /* in bytes */, char *msg);
extern void PatErrorHdrHook (long fd);
extern void defaultsHook (void);
extern void PreTraceHook (long fd);
extern void PostTraceHook (long fd);
#if defined(PAR)
extern void InitEachPEHook (void);
extern void ShutdownEachPEHook (void);
#endif
