/* -----------------------------------------------------------------------------
 * $Id: ProfRts.h,v 1.4 1999/08/25 16:11:49 simonmar Exp $
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

# define DEFAULT_INTERVAL TICK_FREQUENCY

extern rtsBool time_profiling;

#endif
