/* -----------------------------------------------------------------------------
 * $Id: StgStartup.h,v 1.2 1998/12/02 13:28:54 simonm Exp $
 *
 * Code for starting, stopping and restarting threads.
 *
 * ---------------------------------------------------------------------------*/

extern const StgPolyInfoTable stg_stop_thread_info;
EXTFUN(stg_stop_thread_entry);
EXTFUN(stg_returnToStackTop);
EXTFUN(stg_enterStackTop);

#ifdef PROFILING
EXTFUN(stg_register_ret);
EXTFUN(stg_register);
EXTFUN(regPrelGHC);
#endif
