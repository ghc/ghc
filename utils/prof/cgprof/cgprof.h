/* ------------------------------------------------------------------------
 * $Id: cgprof.h,v 1.2 2003/08/01 14:50:50 panne Exp $
 *									
 *	Copyright (C) 1995-2000 University of Oxford
 *									
 * Permission to use, copy, modify, and distribute this software,
 * and to incorporate it, in whole or in part, into other software,
 * is hereby granted without fee, provided that
 *   (1) the above copyright notice and this permission notice appear in
 *	 all copies of the source code, and the above copyright notice
 *	 appear in clearly visible form on all supporting documentation
 *	 and distribution media;
 *   (2) modified versions of this software be accompanied by a complete
 *	 change history describing author, date, and modifications made;
 *	 and
 *   (3) any redistribution of the software, in original or modified
 *	 form, be without fee and subject to these same conditions.
 * --------------------------------------------------------------------- */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include "symbol.h"
#include "matrix.h"

/* -----------------------------------------------------------------------------
 * Data structures associated with parsed data
 * -------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * Cost attributes
 * -------------------------------------------------------------------------- */

#ifndef _CGPROF_H_
#define _CGPROF_H_

typedef struct {
  double   proc_comp;
  double   proc_comm;
  double   proc_comp_idle;
  long int proc_hrel_in;
  long int proc_hrel_out;
} object_cost_proc;

typedef struct {
  double   comp_max,      comp_avg,      comp_min;
  double   comm_max,      comm_avg,      comm_min;
  double   comp_idle_max, comp_idle_avg, comp_idle_min;
  long int hrel_max,      hrel_avg,      hrel_min;
  object_cost_proc *proc;
  int    syncs;
} object_cost;

/* -----------------------------------------------------------------------------
 * Sequence of cost centres
 * -------------------------------------------------------------------------- */

typedef struct {
  object_cost  cost;
  name_id     *stack;
  int          stack_size;
  int          active;
} parsed_cost_object;

#define RAW_PROFILE_INIT_SIZE 100
extern int                 raw_profile_next;
extern int                 raw_profile_size;
extern parsed_cost_object *raw_profile;

/* -----------------------------------------------------------------------------
 * Misc.
 * -------------------------------------------------------------------------- */

extern int   Verbose;
extern char *Pgm;
extern void readRawProfile(FILE *,int*,int);
extern void printRawProfile();
extern void add_costs(object_cost *,object_cost);
extern void createConnectivityMatrix(int,Matrix *,Matrix *,int *,int);
extern void printConnectivityMatrix(Matrix,Matrix,int);
extern FILE* logFile;
#endif
