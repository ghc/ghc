/* -----------------------------------------------------------------------------
 * $Id: ProfRts.h,v 1.3 1999/02/05 16:02:47 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

#ifdef PROFILING

void report_ccs_profiling ( void );
void heap_profile_finish (void);

void initProfiling ( void );
void endProfiling  ( void );

void heapCensus ( bdescr *bd );

void PrintNewStackDecls ( void );

void print_ccs (FILE *, CostCentreStack *);

void report_ccs_profiling( void );

# define TICK_FREQUENCY   50                      /* ticks per second */
# define TICK_MILLISECS   (1000/TICK_FREQUENCY)   /* ms per tick */

# define DEFAULT_INTERVAL TICK_FREQUENCY

extern rtsBool time_profiling;

#endif
