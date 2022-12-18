/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2023
 *
 * Interface to the RTS's flag parser
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdbool.h>
#include "stg/Types.h"

// order is important - do not sort
typedef enum _RtsFlagKey {
    HELP,
    INSTALL_SIGNAL_HANDLERS,
    INSTALL_SEH_HANDLERS,
    GENERATE_STACK_TRACES,
    GENERATE_CRASH_DUMPS,
    NULL_EVENTLOG_WRITER,
    MACHINE_READABLE,
    DISABLE_OS_MEM_RET,
    INTERNAL_COUNTERS,
    IO_MANAGER_FLAG,
    INFO,
    EVENTLOG_FLUSH_INTERVAL,
    COPYING_GC,
    NONMOVING_GC,
    LARGE_OBJ_ALLOC_AREA,
    MIN_ALLOC_AREA,
    GC_BELL,
// #if defined(THREADED_RTS)
// #if defined(mingw32_HOST_OS)
    IO_MANAGER_THREADS,
// #endif
    NUMA,
// #endif
// #if defined(DEBUG) && defined(THREADED_RTS)
    DEBUG_NUMA,
// #endif
    LONG_GC_SYNC,
    NO_AUTO_HEAP_SAMPLES,
    NURSERY_CHUNK_SIZE,
    COMPACT_GC,
    USE_MARK_REGION,
    OLD_GEN_FACTOR,
    RETURN_DECAY_FACTOR,
// #if defined(DEBUG)
    DEBUG_SCHEDULER,
    DEBUG_INTERPRETER,
    DEBUG_WEAK,
    DEBUG_GCCAFS,
    DEBUG_GC,
    DEBUG_NONMOVING_GC,
    DEBUG_BLOCK_ALLOC,
    DEBUG_SANITY,
    DEBUG_ZERO_IN_GC,
    DEBUG_STABLE,
    DEBUG_PROF,
    DEBUG_LINKER,
    DEBUG_LINKER_VERBOSE,
    DEBUG_APPLY,
    DEBUG_STM,
    DEBUG_SQUEEZE,
    DEBUG_HPC,
    DEBUG_SPARKS,
    DEBUG_COMPACT,
// #endif
    MAX_STACK_SIZE,
    STACK_CHUNK_SIZE,
    STACK_CHUNK_BUFFER_SIZE,
    STACK_INITIAL_SIZE,

    UNKNOWN_RTS_OPTION,
} RtsFlagKey;

typedef enum _RtsFlagValueType {
    VOID,
    BOOL,
    ENUM,
    DOUBLE,
    STGWORD64,
} RtsFlagValueType;

typedef struct _RtsFlagName {
    bool optionSafe;
    RtsFlagValueType valueType;
    char* longName;
    char* shortName;
    bool valueRequired;
} RtsFlagName;

typedef struct _FlagValue {
    RtsFlagKey key;
    union {
        bool boolean;
        double _double;
        int _enum;
        char* text;
        StgWord64 stgWord64;
    } as;
} RtsFlagValue;

#define NO_VAL(flagKey) ((RtsFlagValue){flagKey, {.text = "VOID"}})
#define BOOL_VAL(flagKey, value) ((RtsFlagValue){flagKey, {.boolean = value}})
#define ENUM_VAL(flagKey, value) ((RtsFlagValue){flagKey, {._enum = value}})
#define DOUBLE_VAL(flagKey, value) ((RtsFlagValue){flagKey, {._double = value}})
#define STGWORD64_VAL(flagKey, value) ((RtsFlagValue){flagKey, {.stgWord64 = value}})

#define IS_VOID(flagKey) ()

extern RtsFlagName rtsFlags[];
RtsFlagValue parseArg(char *arg, bool *error);
