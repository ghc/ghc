/* -----------------------------------------------------------------------------

 * $Id: Profiling.h,v 1.4 2001/12/12 14:31:43 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

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

extern lnat total_prof_ticks;

extern void fprintCCS( FILE *f, CostCentreStack *ccs );


#endif
