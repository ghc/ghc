/* ---------------------------------------------------------------------------
 * Time-stamp: <Tue Nov 09 1999 16:31:38 Stardate: [-30]3873.44 hwloidl>
 *
 * Runtime system types for GUM
 *
 * ------------------------------------------------------------------------- */

#ifndef PARTYPES_H
#define PARTYPES_H

#ifdef PAR /* all of it */

// now in Parallel.h 
//typedef struct hashtable  HashTable;
//typedef struct hashlist   HashList;

/* Global addresses now live in Parallel.h (needed in Closures.h) */
// gaddr

// now in Parallel.h 
/* (GA, LA) pairs 
typedef struct gala {
    globalAddr   ga;
    StgPtr       la;
    struct gala *next;
    rtsBool      preferred;
} rtsGaLa;
*/

#if defined(GRAN)
typedef unsigned long TIME;
typedef unsigned char Proc;
typedef unsigned char EVTTYPE;
#endif

#endif /* PAR */

#endif /* ! PARTYPES_H */
