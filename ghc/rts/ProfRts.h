/* -----------------------------------------------------------------------------
 * $Id: ProfRts.h,v 1.8 2000/02/17 17:19:42 simonmar Exp $
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

void gen_XML_logfile     ( void );
void report_ccs_profiling ( void );
void heap_profile_finish (void);

void PrintNewStackDecls ( void );

void print_ccs (FILE *, CostCentreStack *);

# define DEFAULT_INTERVAL TICK_FREQUENCY

extern rtsBool time_profiling;

#endif
