/************************************************************************
 *									*
 * Runtime system types for GUM                                         *
 *									*
 ************************************************************************/

#ifndef PARTYPES_H
#define PARTYPES_H

#ifdef PAR /* all of it */

typedef struct hashtable HashTable;
typedef struct hashlist HashList;

typedef double REAL_TIME;
typedef int GLOBAL_TASK_ID;
typedef int PACKET;
typedef int OPCODE;
typedef W_ TIME;
typedef GLOBAL_TASK_ID PROC;

/* Global addresses, in all their glory */

typedef struct {
    union {
	P_ plc;
	struct {
	    GLOBAL_TASK_ID gtid;
	    int slot;
	} gc;
    } loc;
    unsigned weight;
} globalAddr;

/* (GA, LA) pairs */
typedef struct gala {
    globalAddr ga;
    P_ la;
    struct gala *next;
    rtsBool preferred;
} GALA;

#if defined(GRAN)
typedef unsigned long TIME;
typedef unsigned char PROC;
typedef unsigned char EVTTYPE;
#endif

#endif /* PAR */

#endif /* ! PARTYPES_H */


