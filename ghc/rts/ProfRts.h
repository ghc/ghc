/* -----------------------------------------------------------------------------
 * $Id: ProfRts.h,v 1.7 1999/12/03 15:55:29 chak Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING) || defined(DEBUG)
void initProfiling ( void );
void endProfiling  ( void );
#endif

#ifdef PROFILING

void report_ccs_profiling ( void );
void heap_profile_finish (void);

void PrintNewStackDecls ( void );

void print_ccs (FILE *, CostCentreStack *);

# define DEFAULT_INTERVAL TICK_FREQUENCY

extern rtsBool time_profiling;

#endif
