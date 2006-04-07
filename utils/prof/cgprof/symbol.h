/* ------------------------------------------------------------------------
 * $Id: symbol.h,v 1.1 2000/04/05 10:06:36 simonmar Exp $
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

/* -----------------------------------------------------------------------------
 * Symbol table associated with cost centres
 * -------------------------------------------------------------------------- */

#ifndef _SYMBOL_H_
#define _SYMBOL_H_
#define CG_STACK 42
#define CG_SSTEP 1968


#define MAX_PROFILE_LINE_LENGTH   10000
#define MAX_STACK_DEPTH           60
#define MAX_FUNNAME               80


typedef struct {
  int   type;    /* Either CG_STACK or CG_SSTEP */
  int   lineno;
  char *filename;  
} name_object;

typedef int name_id; /* i.e. index into symbol table */

#define SYMBOL_TABLE_INIT_SIZE 100
extern int          symbol_table_next;
extern int          symbol_table_size;
extern name_object *symbol_table;


extern void printSymbolTable(int , int *);
extern int cmp_symbol_entry(const int *, const int *);
extern name_id lookupSymbolTable(int,int,char*);
extern void printSymbolTable_entry(int);
extern void getNameFromSymbolTable(int,char*);
#endif
