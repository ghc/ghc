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

RtsFlagName
rtsFlags[] = {
    [HELP]                    = {SAFE,   VOID,     NULL,                              "?"  , false},
    [INSTALL_SIGNAL_HANDLERS] = {UNSAFE, BOOL,     "install-signal-handlers",          NULL, false},
    [INSTALL_SEH_HANDLERS]    = {UNSAFE, BOOL,     "install-seh-handlers",             NULL, false},
    [GENERATE_STACK_TRACES]   = {UNSAFE, BOOL,     "generate-stack-traces",            NULL, false},
    [GENERATE_CRASH_DUMPS]    = {UNSAFE, BOOL,     "generate-crash-dumps",             NULL, false},
    [NULL_EVENTLOG_WRITER]    = {UNSAFE, BOOL,     "null-eventlog-writer",             NULL, false},
    [MACHINE_READABLE]        = {UNSAFE, BOOL,     "machine-readable",                 NULL, false},
    [DISABLE_OS_MEM_RET]      = {UNSAFE, BOOL,     "disable-delayed-os-memory-return", NULL, false},
    [INTERNAL_COUNTERS]       = {SAFE,   BOOL,     "internal-counters",                NULL, false},
    [IO_MANAGER_FLAG]         = {UNSAFE, ENUM,     "io-manager",                       NULL,  true},
    [INFO]                    = {SAFE,   VOID,     "info",                             NULL, false},
    [EVENTLOG_FLUSH_INTERVAL] = {SAFE,   DOUBLE,   "eventlog-flush-interval",          NULL,  true},
    [COPYING_GC]              = {SAFE,   VOID,     "copying-gc",                       NULL, false},
    [NONMOVING_GC]            = {SAFE,   VOID,     "nonmoving-gc",                     NULL, false},
    [LARGE_OBJ_ALLOC_AREA]    = {UNSAFE, STGWORD64, "large-object-allocation",         "AL",  true},
    [MIN_ALLOC_AREA]          = {UNSAFE, STGWORD64, "minimum-allocation-area-size",    "A",   true},
// #if defined(THREADED_RTS)
// #if defined(mingw32_HOST_OS)
    [IO_MANAGER_THREADS]      = {UNSAFE, STGWORD64, "io-manager-threads",              NULL,  true},
// #endif
    [NUMA]                    = {SAFE,   STGWORD64, "numa",                            NULL, false},
// #endif
// #if defined(DEBUG) && defined(THREADED_RTS)
    [DEBUG_NUMA]              = {SAFE,   STGWORD64, "debug-numa",                      NULL,  true},
// #endif
    [LONG_GC_SYNC]            = {SAFE,   DOUBLE,    "long-gc-sync",                    NULL, false},
    [NO_AUTO_HEAP_SAMPLES]    = {UNSAFE, BOOL,      "no-automatic-heap-samples",       NULL, false},
    [NURSERY_CHUNK_SIZE]      = {UNSAFE, STGWORD64, "alloc-area-chunksize",            "n",   true},
    [GC_BELL]                 = {UNSAFE, VOID,      "gc-bell",                         "B",  false},
    [COMPACT_GC]              = {UNSAFE, DOUBLE,    "compact-gc",                      "c",  false},
    [USE_MARK_REGION]         = {UNSAFE, VOID,      "use-mark-region",                 "w",  false},
    [OLD_GEN_FACTOR]          = {UNSAFE, DOUBLE,    "old-gen-factor",                  "F",   true},
    [RETURN_DECAY_FACTOR]     = {UNSAFE, DOUBLE,    "return-decay-factor",             "Fd",  true},
    // The 'NULL' of flags. Long name just for debugging
    [UNKNOWN_RTS_OPTION]      = {SAFE,   VOID,      "UNKNOWN_RTS_OPTION",              NULL, false},
};

static RtsFlagValue
parse_flag_value(RtsFlagKey i, bool isLongName, char *arg, bool *error);

static double
parseDouble(const char *arg, bool *error)
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


static StgWord64
decodeSize(const char *flag, uint32_t offset, StgWord64 min, StgWord64 max);

static int
find_first_numeric_char(const char *s) {
    int len = strlen(s);
    for (int i = 0; i < len; i++) {
        if (isdigit(s[i])) {
            return i;
        }
    }
    return len;
}

static char*
name_from_arg(bool longName, const char* str) {
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
    RtsFlagName flag = rtsFlags[i];
    char* name = isLongName ? flag.longName : flag.shortName;
    char *arg = isLongName ? &arg0[2] : &arg0[1];
    // offset at which value can be found. Does not account for potential `=`
    int offset = isLongName ? strlen(flag.longName) : strlen(flag.shortName);
    bool isSwitch = !rtsFlags[i].valueRequired && arg[offset] == '\0';
    bool hasValue = (isLongName && arg[offset] == '=' && arg[offset+1] != '\0')
                 || (!isLongName && arg[offset] != '\0');

    // missing value - return immediately
    if (!isSwitch && !hasValue) UNKNOWN_RTS_OPTION(error, arg0);
    if (isSwitch && hasValue) UNKNOWN_RTS_OPTION(error, arg0);

    switch (flag.valueType) {
        case VOID: {
            switch (i) {
                case GC_BELL:
                case USE_MARK_REGION:
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
            switch (i) {
                case LARGE_OBJ_ALLOC_AREA: {
                    value = decodeSize(arg, offset, 2*BLOCK_SIZE,
                                    HS_INT_MAX) / BLOCK_SIZE;
                    break;
                }
                case MIN_ALLOC_AREA: {
                    // minimum two blocks in the nursery, so that we have one
                    // to grab for allocate().
                    value = decodeSize(arg, offset, 2*BLOCK_SIZE,
                                    HS_INT_MAX) / BLOCK_SIZE;
                    break;
                }
// #if defined(THREADED_RTS)
// #if defined(mingw32_HOST_OS)
                case IO_MANAGER_THREADS: {
                    value = (StgWord64)strtol(arg + offset, (char **) NULL, 10);
                    break;
                }
// #endif
                case NUMA: {
                    if (isSwitch) {
                        value = (StgWord64)~0;
                    } else if (hasValue) {
                        value = (StgWord64)strtol(arg + offset, (char **) NULL, 10);
                    } else {
                        *error = true;
                        errorBelch("invalid RTS option: %s", arg0);
                    }
                    break;
                }
// #endif
// #if defined(DEBUG) && defined(THREADED_RTS)
                case DEBUG_NUMA: {
                    if (isdigit(arg[offset]) && hasValue) {
                        value = (StgWord64)strtol(arg + offset, (char **) NULL, 10);
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
                    value = decodeSize(arg, offset, 2*BLOCK_SIZE, HS_INT_MAX)
                                / BLOCK_SIZE;
                    break;
                }
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
decodeSize(const char *flag, uint32_t offset, StgWord64 min, StgWord64 max)
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
        errorBelch("error in RTS option %s: size outside allowed range (%" FMT_Word " - %" FMT_Word ")", flag, (W_)min, (W_)max);
        stg_exit(EXIT_FAILURE);
    }

    // debugBelch("------- decodeSize val: %" FMT_Word64 "\n", val);
    return val;
}
