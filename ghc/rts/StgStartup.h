/* -----------------------------------------------------------------------------
 * $Id: StgStartup.h,v 1.5 2001/09/04 18:29:21 ken Exp $
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

EXTFUN(stg_init_ret);
EXTFUN(stg_init);
EXTFUN(__stginit_PrelGHC);
