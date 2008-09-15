/*
  Time-stamp: <2005-03-30 12:02:33 simonmar>

  RTS specific types.
*/

/* -------------------------------------------------------------------------
   Generally useful typedefs
   ------------------------------------------------------------------------- */

#ifndef RTS_TYPES_H
#define RTS_TYPES_H

typedef unsigned int  nat;           /* at least 32 bits (like int) */
typedef unsigned long lnat;          /* at least 32 bits            */
#ifndef _MSC_VER
typedef unsigned long long ullong;   /* at least 32 bits            */
typedef long long llong;
#else
typedef unsigned __int64   ullong;   /* at least 32 bits            */
typedef __int64 llong;
#endif

/* ullong (64|128-bit) type: only include if needed (not ANSI) */
#if defined(__GNUC__) 
#define LL(x) (x##LL)
#else
#define LL(x) (x##L)
#endif
  
typedef enum { 
    rtsFalse = 0, 
    rtsTrue 
} rtsBool;

/* 
   Types specific to the parallel runtime system.
*/


/* Spark pools: used to store pending sparks 
 *  (THREADED_RTS & PARALLEL_HASKELL only)
 * Implementation uses a DeQue to enable concurrent read accesses at
 * the top end.
 */
typedef struct  SparkPool_ {
  /* Size of elements array. Used for modulo calculation: we round up
     to powers of 2 and use the dyadic log (modulo == bitwise &) */
  StgWord size; 
  StgWord moduloSize; /* bitmask for modulo */

  /* top, index where multiple readers steal() (protected by a cas) */
  StgWord top;

  /* bottom, index of next free place where one writer can push
     elements. This happens unsynchronised. */
  StgWord bottom;
  /* both position indices are continuously incremented, and used as
     an index modulo the current array size. */
  
  /* lower bound on the current top value. This is an internal
     optimisation to avoid unnecessarily accessing the top field
     inside pushBottom */
  StgWord topBound;

  /* The elements array */
  StgClosurePtr* elements;
  /*  Please note: the dataspace cannot follow the admin fields
      immediately, as it should be possible to enlarge it without
      disposing the old one automatically (as realloc would)! */

} SparkPool;

typedef ullong        rtsTime;

#if defined(PAR)
/* types only needed in the parallel system */
typedef struct hashtable ParHashTable;
typedef struct hashlist ParHashList;

/* typedef double REAL_TIME; */
/* typedef W_ TIME; */
/* typedef GlobalTaskId Proc; */
typedef int           GlobalTaskId;
typedef GlobalTaskId  PEs;
typedef unsigned int  rtsWeight;
typedef int           rtsPacket;
typedef int           OpCode;

/* Global addresses i.e. unique ids in a parallel setup; needed in Closures.h*/
typedef struct {
  union {
    StgPtr plc;
    struct {
      GlobalTaskId gtid;
      int slot;
    } gc;
  } payload;
  rtsWeight weight;
} globalAddr;

/* (GA, LA) pairs */
typedef struct gala {
    globalAddr ga;
    StgPtr la;
    struct gala *next;
    rtsBool preferred;
} GALA;

#elif defined(GRAN)

/*
 * GlobalTaskId is dummy in GranSim;
 * we define it to have cleaner code in the RTS
 */
typedef int       GlobalTaskId;
typedef lnat      rtsTime;
typedef StgWord   PEs;

#endif

#endif /* RTS_TYPES_H */
