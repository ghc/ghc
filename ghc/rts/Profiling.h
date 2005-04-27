/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

#include <stdio.h>

#if defined(PROFILING) || defined(DEBUG)
void initProfiling1 ( void );
void initProfiling2 ( void );
void endProfiling   ( void );

extern FILE *prof_file;
extern FILE *hp_file;
#endif

#ifdef PROFILING

void gen_XML_logfile     ( void );
void reportCCSProfiling ( void );

void PrintNewStackDecls ( void );

extern lnat RTS_VAR(total_prof_ticks);

extern void fprintCCS( FILE *f, CostCentreStack *ccs );
extern void fprintCCS_stderr( CostCentreStack *ccs );

#ifdef DEBUG
extern void debugCCS( CostCentreStack *ccs );
#endif

#endif
