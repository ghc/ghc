/* -----------------------------------------------------------------------------
 * $Id: ProfRts.h,v 1.6 1999/09/16 12:29:55 simonmar Exp $
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

void initProfiling ( void );
void endProfiling  ( void );

void PrintNewStackDecls ( void );

void print_ccs (FILE *, CostCentreStack *);

void report_ccs_profiling( void );

# define DEFAULT_INTERVAL TICK_FREQUENCY

extern rtsBool time_profiling;

#endif
