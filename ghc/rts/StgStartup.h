/* -----------------------------------------------------------------------------
 * $Id: StgStartup.h,v 1.3 1999/02/05 16:03:00 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
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
