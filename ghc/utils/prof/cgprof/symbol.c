/* ------------------------------------------------------------------------
 * $Id: symbol.c,v 1.3 2003/08/01 14:50:50 panne Exp $
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

#include <string.h>
#include "symbol.h"

/* -----------------------------------------------------------------------------
 * Data structures
 * -------------------------------------------------------------------------- */
int          symbol_table_next=0;
int          symbol_table_size=0;
name_object *symbol_table=NULL;

/* -----------------------------------------------------------------------------
 * Create/grow symbol table
 * -------------------------------------------------------------------------- */

void enlargeSymbolTable() {

  if (symbol_table_size==0) {
    symbol_table_next = 0;
    symbol_table_size = SYMBOL_TABLE_INIT_SIZE;
    symbol_table      = calloc(symbol_table_size,sizeof(name_object));
  } else {
    symbol_table_size += SYMBOL_TABLE_INIT_SIZE;
    symbol_table       = realloc(symbol_table,
				 symbol_table_size*sizeof(name_object));
  }
  if (symbol_table==NULL) {
    fprintf(stderr,"{enlargeSymbolTable} unable to allocate %d elements",
            symbol_table_size);
    exit(1);
  }
}

/* -----------------------------------------------------------------------------
 * Lookup/add name to symbol table
 * -------------------------------------------------------------------------- */

name_id lookupSymbolTable(int type,int lineno,char* str) {
  int i;
  extern FILE *logFile;

  for(i=0;i<symbol_table_next;i++) {
    if ((type==symbol_table[i].type) && 
        (strcmp(str,symbol_table[i].filename)==0) &&
        (type==CG_STACK || (lineno==symbol_table[i].lineno))) {
      return i;
    }
  }
  fprintf(logFile,"{lookupSymbolTable} %d at %s line %d\n",type,str,lineno);
  if (symbol_table_next==symbol_table_size) enlargeSymbolTable();
  symbol_table[symbol_table_next].type    = type;
  symbol_table[symbol_table_next].lineno  = lineno;
  symbol_table[symbol_table_next].filename= malloc(1+strlen(str));
  if (symbol_table[symbol_table_next].filename==NULL) {
    fprintf(stderr,"{lookupSymbolTable} failed to allocate space");
    exit(1);
  }
  strcpy(symbol_table[symbol_table_next].filename,str);
  return (symbol_table_next++);
}

/* -----------------------------------------------------------------------------
 * Comparison function to be used by \texttt{qsort}
 * -------------------------------------------------------------------------- */

int cmp_symbol_entry(const int *x, const int *y) {
  int i;

  if (symbol_table[*x].type==symbol_table[*y].type) {
    i = strcmp(symbol_table[*x].filename,symbol_table[*y].filename);
    if  (i==0) return (symbol_table[*x].lineno - symbol_table[*y].lineno);
    else return i;
  } else {
    if (symbol_table[*x].type==CG_STACK) return 1;
    else return -1;
  }
}


/* -----------------------------------------------------------------------------
 * Pretty print a symbol table entry
 * -------------------------------------------------------------------------- */

void printSymbolTable_entry(int idx) {
  extern FILE *logFile;
  if (symbol_table[idx].type==CG_SSTEP) {
    fprintf(logFile,"(line %d of %s) ",symbol_table[idx].lineno,
	                              symbol_table[idx].filename);
  } else {
    fprintf(logFile,"%s ",symbol_table[idx].filename);
  }
}

void getNameFromSymbolTable(int idx, char* name) {
  strcpy(name,symbol_table[idx].filename);
}

