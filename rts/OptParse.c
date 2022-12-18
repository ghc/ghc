#include "Rts.h"
#include "RtsUtils.h"

#if defined(HAVE_CTYPE_H)
#include <ctype.h>
#endif
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define FLAGS_LENGTH ((int)(sizeof(rtsFlags) / sizeof(rtsFlags[0])))
#define SAFE true
#define UNSAFE false

#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))

#define UNKNOWN_RTS_OPTION(error, arg) \
    do { \
        *error = true; \
        errorBelch("unknown RTS option: %s" ,arg); \
        return NO_VAL(UNKNOWN_RTS_OPTION); \
    } while (false)

#define BAD_VALUE(error, arg) \
    do { \
        *error = true; \
        errorBelch("bad value for %s" ,arg); \
        return NO_VAL(UNKNOWN_RTS_OPTION); \
    } while (false)

#define UNEXPECTED_ARGUMENT(error, name, arg) \
    do { \
        *error = true; \
        errorBelch("flag %s given an argument when none was expected: %s" , name, arg); \
        return NO_VAL(UNKNOWN_RTS_OPTION); \
    } while (false)

#define OUT_OF_RANGE(error, name, min, max) \
    do { \
        *error = true; \
        errorBelch("error in RTS option %s: size outside allowed range (%" FMT_Word " - %" FMT_Word ")", name, (W_)min, (W_)max); \
        return NO_VAL(UNKNOWN_RTS_OPTION); \
    } while (false)

RtsFlagName
rtsFlags[] = {
    [HELP]                    = {SAFE,   VOID,      "help",                             "?" , false},
    [INSTALL_SIGNAL_HANDLERS] = {UNSAFE, BOOL,      "install-signal-handlers",          NULL, false},
    [INSTALL_SEH_HANDLERS]    = {UNSAFE, BOOL,      "install-seh-handlers",             NULL, false},
    [GENERATE_STACK_TRACES]   = {UNSAFE, BOOL,      "generate-stack-traces",            NULL, false},
    [GENERATE_CRASH_DUMPS]    = {UNSAFE, BOOL,      "generate-crash-dumps",             NULL, false},
    [NULL_EVENTLOG_WRITER]    = {UNSAFE, BOOL,      "null-eventlog-writer",             NULL, false},
    [MACHINE_READABLE]        = {UNSAFE, BOOL,      "machine-readable",                 NULL, false},
    [DISABLE_OS_MEM_RET]      = {UNSAFE, BOOL,      "disable-delayed-os-memory-return", NULL, false},
    [INTERNAL_COUNTERS]       = {SAFE,   BOOL,      "internal-counters",                NULL, false},
    [IO_MANAGER_FLAG]         = {UNSAFE, ENUM,      "io-manager",                       NULL,  true},
    [INFO]                    = {SAFE,   VOID,      "info",                             NULL, false},
    [EVENTLOG_FLUSH_INTERVAL] = {SAFE,   DOUBLE,    "eventlog-flush-interval",          NULL,  true},
    [COPYING_GC]              = {SAFE,   VOID,      "copying-gc",                       NULL, false},
    [NONMOVING_GC]            = {SAFE,   VOID,      "nonmoving-gc",                     NULL, false},
    [LARGE_OBJ_ALLOC_AREA]    = {UNSAFE, STGWORD64, "large-object-allocation",          "AL",  true},
    [MIN_ALLOC_AREA]          = {UNSAFE, STGWORD64, "minimum-allocation-area-size",     "A",   true},
// #if defined(THREADED_RTS)
// #if defined(mingw32_HOST_OS)
    [IO_MANAGER_THREADS]      = {UNSAFE, STGWORD64,  "io-manager-threads",              NULL,  true},
// #endif
    [NUMA]                    = {SAFE,   STGWORD64,  "numa",                            NULL, false},
// #endif
// #if defined(DEBUG) && defined(THREADED_RTS)
    [DEBUG_NUMA]              = {SAFE,   STGWORD64,  "debug-numa",                      NULL,  true},
// #endif
    [LONG_GC_SYNC]            = {SAFE,   DOUBLE,     "long-gc-sync",                    NULL,  true},
    [NO_AUTO_HEAP_SAMPLES]    = {UNSAFE, BOOL,       "no-automatic-heap-samples",       NULL, false},
    [NURSERY_CHUNK_SIZE]      = {UNSAFE, STGWORD64,  "alloc-area-chunksize",            "n",   true},
    [GC_BELL]                 = {UNSAFE, VOID,       "gc-bell",                         "B",  false},
    [COMPACT_GC]              = {UNSAFE, DOUBLE,     "compact-gc",                      "c",  false},
    [USE_MARK_REGION]         = {UNSAFE, VOID,       "use-mark-region",                 "w",  false},
    [OLD_GEN_FACTOR]          = {UNSAFE, DOUBLE,     "old-gen-factor",                  "F",   true},
    [RETURN_DECAY_FACTOR]     = {UNSAFE, DOUBLE,     "return-decay-factor",             "Fd",  true},
// #if defined(DEBUG)
    [DEBUG_SCHEDULER]         = {SAFE,   VOID,       "debug-scheduler",                 "Ds", false},
    [DEBUG_INTERPRETER]       = {SAFE,   VOID,       "debug-interpreter",               "Di", false},
    [DEBUG_WEAK]              = {SAFE,   VOID,       "debug-weak",                      "Dw", false},
    [DEBUG_GCCAFS]            = {SAFE,   VOID,       "debug-gccafs",                    "DG", false},
    [DEBUG_GC]                = {SAFE,   VOID,       "debug-gc",                        "Dg", false},
    [DEBUG_NONMOVING_GC]      = {SAFE,   VOID,       "debug-nonmoving-gc",              "Dn", false},
    [DEBUG_BLOCK_ALLOC]       = {SAFE,   VOID,       "debug-block-alloc",               "Db", false},
    [DEBUG_SANITY]            = {SAFE,   VOID,       "debug-sanity",                    "DS", false},
    [DEBUG_ZERO_IN_GC]        = {SAFE,   VOID,       "debug-zero-on-gc",                "DZ", false},
    [DEBUG_STABLE]            = {SAFE,   VOID,       "debug-stable",                    "Dt", false},
    [DEBUG_PROF]              = {SAFE,   VOID,       "debug-prof",                      "Dp", false},
    [DEBUG_LINKER]            = {SAFE,   VOID,       "debug-linker",                    "Dl", false},
    [DEBUG_LINKER_VERBOSE]    = {SAFE,   VOID,       "debug-linker-verbose",            "DL", false},
    [DEBUG_APPLY]             = {SAFE,   VOID,       "debug-apply",                     "Da", false},
    [DEBUG_STM]               = {SAFE,   VOID,       "debug-stm",                       "Dm", false},
    [DEBUG_SQUEEZE]           = {SAFE,   VOID,       "debug-squeeze",                   "Dz", false},
    [DEBUG_HPC]               = {SAFE,   VOID,       "debug-hpc",                       "Dc", false},
    [DEBUG_SPARKS]            = {SAFE,   VOID,       "debug-sparks",                    "Dr", false},
    [DEBUG_COMPACT]           = {SAFE,   VOID,       "debug-compact",                   "DC", false},
// #endif
    [MAX_STACK_SIZE]          = {UNSAFE, STGWORD64,  "stack-max-size",                  "K",   true},
    [STACK_CHUNK_SIZE]        = {UNSAFE, STGWORD64,  "stack-chunk-size",                "kc",  true},
    [STACK_CHUNK_BUFFER_SIZE] = {UNSAFE, STGWORD64,  "stack-chunk-buffer-size",         "kb",  true},
    [STACK_INITIAL_SIZE]      = {UNSAFE, STGWORD64,  "stack-initial-size",              "ki",  true},
    // The 'NULL' of flags. Long name just for debugging
    [UNKNOWN_RTS_OPTION]      = {SAFE,   VOID,       "UNKNOWN_RTS_OPTION",              NULL, false},
};

static RtsFlagValue
parse_flag_value(RtsFlagKey i, bool isLongName, char *arg, bool *error);

static double
parseDouble(char *arg, bool *error)
{
    char *endptr;
    double out;
    errno = 0;

    out = strtod(arg, &endptr);

    if (errno != 0 || endptr == arg) {
        *error = true;
        return out;
    }

    while (isspace((unsigned char)*endptr)) {
        ++endptr;
    }

    if (*endptr != 0) {
        *error = true;
    }

    return out;
}

static
bool valid_size(char c)
{
    char sizes[8] = "wWgGmMkK";
    for (int i = 0; i < 8; i++) {
        if (sizes[i] == c) return true;
    }
    return false;
}

// checks if str can be pased using decodeSize
static
bool is_valid_size(char *str) {
    if (str == NULL || str[0] == '\0') {
        return false;
    }
    // Check for a negative sign
    int i = 0;
    if (str[0] == '-') {
        i = 1;
    }
    // Check for digits
    for (; str[i] != '\0'; i++) {
        if (i == strlen(str)-1
            && valid_size(str[i])) { return true;
        }
        if (!isdigit(str[i])) {
            return false;
        }
    }
    return true;
}

static StgWord64
decodeSize(char *flag, uint32_t offset, StgWord64 min, StgWord64 max, bool* error);

static int
find_first_numeric_char(char *s) {
    int len = strlen(s);
    for (int i = 0; i < len; i++) {
        if (isdigit(s[i])) {
            return i;
        }
    }
    return len;
}

static char*
name_from_arg(bool longName, char* str) {
    ASSERT(str != NULL);
    int i;
    int str_len = strlen(str);
    char* substring = stgMallocBytes(str_len + 1, "name_from_arg"); // allocate memory for the substring

    strcpy(substring, str);

    if (!longName) {
        substring[find_first_numeric_char(str)] = '\0';
    } else {
        for (i = 0; i < str_len; i++) {
            if (substring[i] == '=') {
                substring[i] = '\0';
                break;
            }
        }
    }

    return substring;
}

RtsFlagValue
parseArg(char *arg, bool *error)
{
    // debugBelch("------- parseArg: %s\n", arg);
    if (!strncmp("-?", arg, 2)) return NO_VAL(HELP);
    for (int i = 0; i < FLAGS_LENGTH; i++) {
        bool isLongName = arg[1] == '-';

        RtsFlagName flag = rtsFlags[i];
        if (isLongName  && flag.longName  == NULL) continue;
        if (!isLongName && flag.shortName == NULL) continue;

        char* name = isLongName ? flag.longName : flag.shortName;
        if (name == NULL) continue;

        char* arg1 = isLongName ? &arg[2] : &arg[1];
        char* nameFromArg = name_from_arg(isLongName, arg1);
        if (!strncmp(nameFromArg, name, MAX(strlen(nameFromArg), strlen(name)))) {
            stgFree(nameFromArg);
            return parse_flag_value(i, isLongName, arg, error);
        }
        stgFree(nameFromArg);
    }
    UNKNOWN_RTS_OPTION(error, arg);
}

static RtsFlagValue
parse_flag_value(RtsFlagKey i, bool isLongName, char *arg0, bool *error)
{
    // trim off hyphens
    char* arg  = isLongName ? &arg0[2] : &arg0[1];
    RtsFlagName flag = rtsFlags[i];
    char* name = isLongName ? flag.longName : flag.shortName;
    // offset at which value can be found. Does not account for potential `=`
    int offset = strlen(name);
    bool isSwitch = !rtsFlags[i].valueRequired && arg[offset] == '\0';
    bool hasValue = (isLongName && arg[offset] == '=' && arg[offset+1] != '\0')
                 || (!isLongName && arg[offset] != '\0');

    // debugBelch("in: %s name %s isSwitch %i hasValue %i 'arg[offset] == '\0'?: %i\n", arg0, name, isSwitch, hasValue, arg[offset] == '\0');
    // missing value - return immediately
    if (!isSwitch && !hasValue) UNKNOWN_RTS_OPTION(error, arg0);
    if (isSwitch && hasValue) UNKNOWN_RTS_OPTION(error, arg0);

    switch (flag.valueType) {
        case VOID: {
            switch (i) {
            case INFO:
            case COPYING_GC:
            case NONMOVING_GC:
            case GC_BELL:
            case USE_MARK_REGION:
// #if defined(DEBUG)
            case DEBUG_SCHEDULER:
            case DEBUG_INTERPRETER:
            case DEBUG_WEAK:
            case DEBUG_GCCAFS:
            case DEBUG_GC:
            case DEBUG_NONMOVING_GC:
            case DEBUG_BLOCK_ALLOC:
            case DEBUG_SANITY:
            case DEBUG_ZERO_IN_GC:
            case DEBUG_STABLE:
            case DEBUG_PROF:
            case DEBUG_LINKER:
            case DEBUG_LINKER_VERBOSE:
            case DEBUG_APPLY:
            case DEBUG_STM:
            case DEBUG_SQUEEZE:
            case DEBUG_HPC:
            case DEBUG_SPARKS:
            case DEBUG_COMPACT:
// #endif
                    if (hasValue) UNEXPECTED_ARGUMENT(error, name, arg0);
            }
            return NO_VAL(i);
        }
        case BOOL: {
            // we forbid having bool flags with '=' with no specified value
            if (isSwitch) {
                return BOOL_VAL(i, true);
            } else {
                if (!strncmp("yes", &arg[offset + 1], 3)) {
                    return BOOL_VAL(i, true);
                } else if (!strncmp("no", &arg[offset + 1], 2)) {
                    return BOOL_VAL(i, false);
                } else {
                    UNKNOWN_RTS_OPTION(error, arg0);
                }
            }
            break;
        }
        case ENUM: {
            switch (i) {
                case IO_MANAGER_FLAG: {
                    if (!strncmp("native", &arg[offset + 1], 6)) {
                        return ENUM_VAL(i, IO_MNGR_NATIVE);
                    } else if (!strncmp("posix", &arg[offset + 1], 5)) {
                        return ENUM_VAL(i, IO_MNGR_POSIX);
                    }
                }
                default:
                    *error = true;
                    errorBelch("invalid enum '%s' for '%s'", &arg[offset + 1], rtsFlags[i].longName);
            }
            break;
        }
        case DOUBLE: {
            double res;
            // account for '=' that is used with long-form names
            // some long-from names can have no value though so account for that as well
            if (isLongName && arg[offset] == '=') offset++;
            switch (i) {
                case EVENTLOG_FLUSH_INTERVAL: {
                    res = parseDouble(arg+offset, error);
                    break;
                }
                case LONG_GC_SYNC: {
                    res = parseDouble(arg+offset, error);
                    break;
                }
                case COMPACT_GC: {
                    // special treatment when used as a switch
                    if (!hasValue) return NO_VAL(i);
                    res = parseDouble(arg+offset, error);
                    break;
                }
                case OLD_GEN_FACTOR: {
                    res = parseDouble(arg+offset, error);
                    break;
                }
                case RETURN_DECAY_FACTOR: {
                    res = parseDouble(arg+offset, error);
                    break;
                }
                default: {}
            }
            if (*error) {
                BAD_VALUE(error, arg);
            }
            return DOUBLE_VAL(i, res);
        }
        case STGWORD64: {
            StgWord64 value = 0;
            // account for '=' that is used with long-form names
            // some long-from names can have no value though so account for that as well
            if (isLongName && arg[offset] == '=') offset++;
            if (hasValue && !is_valid_size(&arg[offset])) {
                BAD_VALUE(error, arg);
            }
            StgWord64 min;
            StgWord64 max;
            switch (i) {
                case LARGE_OBJ_ALLOC_AREA: {
                    // minimum two blocks in the nursery, so that we have one
                    // to grab for allocate().
                    min = 2*BLOCK_SIZE;
                    max = HS_INT_MAX;
                    value = decodeSize(arg, offset, min, max, error);
                    break;
                }
                case MIN_ALLOC_AREA: {
                    // minimum two blocks in the nursery, so that we have one
                    // to grab for allocate().
                    min = 2*BLOCK_SIZE;
                    max = HS_INT_MAX;
                    value = decodeSize(arg, offset, min, max, error);
                    break;
                }
// #if defined(THREADED_RTS)
// #if defined(mingw32_HOST_OS)
                case IO_MANAGER_THREADS: {
                    // this has to be uint32_t
                    min = 0;
                    max = HS_INT_MAX;
                    value = decodeSize(arg, offset, min, max, error);
                    break;
                }
// #endif
                case NUMA: {
                    min = 0;
                    max = HS_INT_MAX;
                    if (isSwitch) {
                        value = (StgWord64)~0;
                    } else if (hasValue) {
                        value = decodeSize(arg, offset, min, max, error);
                    } else {
                        *error = true;
                        errorBelch("invalid RTS option: %s", arg0);
                    }
                    break;
                }
// #endif
// #if defined(DEBUG) && defined(THREADED_RTS)
                case DEBUG_NUMA: {
                    min = 0;
                    max = MAX_NUMA_NODES;
                    if (hasValue) {
                        value = decodeSize(arg, offset, min, max, error);
                    } else {
                        UNKNOWN_RTS_OPTION(error, arg0);
                    }
                    if (value > MAX_NUMA_NODES) {
                        *error = true;
                        errorBelch("%s: Too many NUMA nodes (max %d)",
                                     rtsFlags[i].longName, MAX_NUMA_NODES);
                    }
                    break;
                }
// #endif
                case NURSERY_CHUNK_SIZE: {
                    min = 2*BLOCK_SIZE;
                    max = HS_INT_MAX;
                    value = decodeSize(arg, offset, min, max, error);
                    break;
                }

                case MAX_STACK_SIZE:
                case STACK_CHUNK_SIZE:
                case STACK_CHUNK_BUFFER_SIZE:
                case STACK_INITIAL_SIZE: {
                    min = sizeof(W_);
                    max = HS_WORD_MAX;
                    value = decodeSize(arg, offset, min, max, error);
                    break;
                }
            }
            if (*error) {
                OUT_OF_RANGE(error, arg, min, max);
            }
            return STGWORD64_VAL(i, value);
        }
        default: {
            UNKNOWN_RTS_OPTION(error, arg0);
        }
    }
}

/* -----------------------------------------------------------------------------
 * decodeSize: parse a string containing a size, like 300K or 1.2M
-------------------------------------------------------------------------- */
static StgWord64
decodeSize(char *flag, uint32_t offset, StgWord64 min, StgWord64 max, bool *error)
{
    char c;
    const char *s;
    StgDouble m;
    StgWord64 val;

    s = flag + offset;
    // debugBelch("------- decodeSize s: %s\n", s);

    if (!*s)
    {
        m = 0;
    }
    else
    {
        m = atof(s);
        c = s[strlen(s)-1];

        if (c == 'g' || c == 'G')
            m *= 1024*1024*1024;
        else if (c == 'm' || c == 'M')
            m *= 1024*1024;
        else if (c == 'k' || c == 'K')
            m *= 1024;
        else if (c == 'w' || c == 'W')
            m *= sizeof(W_);
    }

    val = (StgWord64)m;
    // debugBelch("------- decodeSize m: %f\n", m);
    if (m < 0 || val < min || val > max) {
        // printf doesn't like 64-bit format specs on Windows
        // apparently, so fall back to unsigned long.
        *error = true;
        // errorBelch("error in RTS option %s: size outside allowed range (%" FMT_Word " - %" FMT_Word ")", flag, (W_)min, (W_)max);
        // stg_exit(EXIT_FAILURE);
    }

    // debugBelch("------- decodeSize val: %" FMT_Word64 "\n", val);
    return val;
}
