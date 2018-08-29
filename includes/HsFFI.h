/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * A mapping for Haskell types to C types, including the corresponding bounds.
 * Intended to be used in conjuction with the FFI.
 *
 * WARNING: Keep this file and StgTypes.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(__cplusplus)
extern "C" {
#endif

/* get types from GHC's runtime system */
#include "ghcconfig.h"
#include "stg/Types.h"

/* get limits for floating point types */
#include <float.h>

typedef StgChar                 HsChar;
typedef StgInt                  HsInt;
typedef StgInt8                 HsInt8;
typedef StgInt16                HsInt16;
typedef StgInt32                HsInt32;
typedef StgInt64                HsInt64;
typedef StgWord                 HsWord;
typedef StgWord8                HsWord8;
typedef StgWord16               HsWord16;
typedef StgWord32               HsWord32;
typedef StgWord64               HsWord64;
typedef StgFloat                HsFloat;
typedef StgDouble               HsDouble;
typedef StgInt                  HsBool;
typedef void*                   HsPtr;          /* this should better match StgAddr */
typedef void                    (*HsFunPtr)(void); /* this should better match StgAddr */
typedef void*                   HsStablePtr;

/* this should correspond to the type of StgChar in StgTypes.h */
#define HS_CHAR_MIN             0
#define HS_CHAR_MAX             0x10FFFF

/* is it true or not?  */
#define HS_BOOL_FALSE           0
#define HS_BOOL_TRUE            1

#define HS_BOOL_MIN             HS_BOOL_FALSE
#define HS_BOOL_MAX             HS_BOOL_TRUE


#define HS_INT_MIN              STG_INT_MIN
#define HS_INT_MAX              STG_INT_MAX
#define HS_WORD_MAX             STG_WORD_MAX

#define HS_INT8_MIN             STG_INT8_MIN
#define HS_INT8_MAX             STG_INT8_MAX
#define HS_INT16_MIN            STG_INT16_MIN
#define HS_INT16_MAX            STG_INT16_MAX
#define HS_INT32_MIN            STG_INT32_MIN
#define HS_INT32_MAX            STG_INT32_MAX
#define HS_INT64_MIN            STG_INT64_MIN
#define HS_INT64_MAX            STG_INT64_MAX
#define HS_WORD8_MAX            STG_WORD8_MAX
#define HS_WORD16_MAX           STG_WORD16_MAX
#define HS_WORD32_MAX           STG_WORD32_MAX
#define HS_WORD64_MAX           STG_WORD64_MAX

#define HS_FLOAT_RADIX          FLT_RADIX
#define HS_FLOAT_ROUNDS         FLT_ROUNDS
#define HS_FLOAT_EPSILON        FLT_EPSILON
#define HS_FLOAT_DIG            FLT_DIG
#define HS_FLOAT_MANT_DIG       FLT_MANT_DIG
#define HS_FLOAT_MIN            FLT_MIN
#define HS_FLOAT_MIN_EXP        FLT_MIN_EXP
#define HS_FLOAT_MIN_10_EXP     FLT_MIN_10_EXP
#define HS_FLOAT_MAX            FLT_MAX
#define HS_FLOAT_MAX_EXP        FLT_MAX_EXP
#define HS_FLOAT_MAX_10_EXP     FLT_MAX_10_EXP

#define HS_DOUBLE_RADIX         DBL_RADIX
#define HS_DOUBLE_ROUNDS        DBL_ROUNDS
#define HS_DOUBLE_EPSILON       DBL_EPSILON
#define HS_DOUBLE_DIG           DBL_DIG
#define HS_DOUBLE_MANT_DIG      DBL_MANT_DIG
#define HS_DOUBLE_MIN           DBL_MIN
#define HS_DOUBLE_MIN_EXP       DBL_MIN_EXP
#define HS_DOUBLE_MIN_10_EXP    DBL_MIN_10_EXP
#define HS_DOUBLE_MAX           DBL_MAX
#define HS_DOUBLE_MAX_EXP       DBL_MAX_EXP
#define HS_DOUBLE_MAX_10_EXP    DBL_MAX_10_EXP

extern void hs_init     (int *argc, char **argv[]);
extern void hs_exit     (void);
extern void hs_exit_nowait(void);
extern void hs_set_argv (int argc, char *argv[]);
extern void hs_thread_done (void);

extern void hs_perform_gc (void);

// Lock the stable pointer table. The table must be unlocked
// again before calling any Haskell functions, even if those
// functions do not manipulate stable pointers. The Haskell
// garbage collector will not be able to run until this lock
// is released! It is also forbidden to call hs_free_fun_ptr
// or any stable pointer-related FFI functions other than
// hs_free_stable_ptr_unsafe while the table is locked.
extern void hs_lock_stable_ptr_table (void);

// A deprecated synonym.
extern void hs_lock_stable_tables (void);

// Unlock the stable pointer table.
extern void hs_unlock_stable_ptr_table (void);

// A deprecated synonym.
extern void hs_unlock_stable_tables (void);

// Free a stable pointer assuming that the stable pointer
// table is already locked.
extern void hs_free_stable_ptr_unsafe (HsStablePtr sp);

extern void hs_free_stable_ptr (HsStablePtr sp);
extern void hs_free_fun_ptr    (HsFunPtr fp);

extern StgPtr hs_spt_lookup(StgWord64 key[2]);
extern int hs_spt_keys(StgPtr keys[], int szKeys);
extern int hs_spt_key_count (void);

extern void hs_try_putmvar (int capability, HsStablePtr sp);

/* -------------------------------------------------------------------------- */



#if defined(__cplusplus)
}
#endif
