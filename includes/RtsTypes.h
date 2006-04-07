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
