/* -----------------------------------------------------------------------------
 * $Id: Hooks.h,v 1.2 1998/12/02 13:21:08 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

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
