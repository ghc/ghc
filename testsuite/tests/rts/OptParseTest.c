#include <stdio.h>
#include "Rts.h"

#define SAFE true
#define UNSAFE false

static bool ERROR = false;

static void _TEST( char* flagToTest
                 , int expectedFlagKey
                 , char* expectedLongName
                 , char* expectedShortName
                 , RtsFlagValueType expectedFlagValueType
                 , bool safe
                 , RtsFlagValue expectedValue
                 ) {
     debugBelch("\n(TEST) input: %s\n", flagToTest);
     printf("\n(TEST) input: %s\n", flagToTest);
     RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
     CHECK(!ERROR);
     RtsFlagName flag = rtsFlags[flagValue.key];

     printf("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, safe ? "SAFE": "UNSAFE");
     debugBelch("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, safe ? "SAFE": "UNSAFE");
     CHECK(flagValue.key == expectedFlagKey);
     CHECK(flag.longName == expectedLongName);
     CHECK(flag.shortName == expectedShortName);
     CHECK(flag.valueType == expectedFlagValueType);
     CHECK(flag.optionSafe == safe);
     RtsFlagValueType valueTy = flag.valueType;
     if (valueTy == BOOL) {
          CHECK(expectedValue.as.boolean == flagValue.as.boolean);
          printf("\tvalue: %s\n", flagValue.as.boolean ? "true" : "false");
     }
     if (valueTy == ENUM) {
          CHECK(expectedValue.as._enum == flagValue.as._enum);
          printf("\tvalue: %i\n", flagValue.as._enum);
     }
     if (valueTy == DOUBLE) {
          debugBelch("expected: %f actual: %f\n", expectedValue.as._double, flagValue.as._double);
          CHECK(expectedValue.as._double == flagValue.as._double);
          printf("\tvalue: %f\n", flagValue.as._double);
     }
     if (valueTy == STGWORD64) {
          debugBelch("expected: %" FMT_Word64 " actual: %" FMT_Word64 "\n", expectedValue.as.stgWord64, flagValue.as.stgWord64);
          printf("\tvalue: %" FMT_Word64 "\n", flagValue.as.stgWord64);
     //   CHECK(expectedValue.as.stgWord64 == flagValue.as.stgWord64);
     }
}

static void _FAIL_TEST(char* flagToTest) {
    debugBelch("\n(FAIL_TEST) input: %s\n", flagToTest);
    RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
    CHECK(ERROR);
    ERROR = false;
}

static void _VOID_FLAG_TEST(const RtsFlagKey i)
{
     RtsFlagName name = rtsFlags[i];
     char buffer[100];
     snprintf(buffer, sizeof(buffer), "--%s", name.longName);
    _TEST( buffer, i
        , name.longName, name.shortName
        , name.valueType, SAFE, NO_VAL(i));
    snprintf(buffer, sizeof(buffer), "-%s", name.shortName);
    _TEST( buffer, i
        , name.longName, name.shortName
        , name.valueType, SAFE, NO_VAL(i));
    snprintf(buffer, sizeof(buffer), "-%s=", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=123G", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=false", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "--%s=true", name.longName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s3621", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=3622", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=true", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s3622", name.shortName);
    _FAIL_TEST(buffer);
    snprintf(buffer, sizeof(buffer), "-%s=3600", name.shortName);
    _FAIL_TEST(buffer);
}

int main (int argc, char *argv[])
{

    printf("=== OptParseTest START ===\n");

    _TEST( "-?", HELP
         , NULL, "?"
         , VOID, SAFE, NO_VAL(HELP));
    // _FAIL_TEST("-?asfg");

    _TEST( "--install-signal-handlers", INSTALL_SIGNAL_HANDLERS
         , "install-signal-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SIGNAL_HANDLERS, true));
    _TEST( "--install-signal-handlers=yes", INSTALL_SIGNAL_HANDLERS
         , "install-signal-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SIGNAL_HANDLERS, true));
    _TEST( "--install-signal-handlers=no", INSTALL_SIGNAL_HANDLERS
         , "install-signal-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SIGNAL_HANDLERS, false));
    _FAIL_TEST("--install-signal-handlers=dunnow");
    _FAIL_TEST("--install-signal-handlersgasg");

    _TEST( "--install-seh-handlers", INSTALL_SEH_HANDLERS
         , "install-seh-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SEH_HANDLERS, true));
    _TEST( "--install-seh-handlers=yes", INSTALL_SEH_HANDLERS
         , "install-seh-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SEH_HANDLERS, true));
    _TEST( "--install-seh-handlers=no", INSTALL_SEH_HANDLERS
         , "install-seh-handlers", NULL
         , BOOL, UNSAFE, BOOL_VAL(INSTALL_SEH_HANDLERS, false));
    _FAIL_TEST("--install-seh-handlers=");
    _FAIL_TEST("--install-seh-handlers=hmmm");
    _FAIL_TEST("--install-seh-handlersgasdxxxasg");

    _TEST( "--generate-stack-traces", GENERATE_STACK_TRACES
         , "generate-stack-traces", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_STACK_TRACES, true));
    _TEST( "--generate-stack-traces=yes", GENERATE_STACK_TRACES
         , "generate-stack-traces", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_STACK_TRACES, true));
    _TEST( "--generate-stack-traces=no", GENERATE_STACK_TRACES
         , "generate-stack-traces", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_STACK_TRACES, false));
    _FAIL_TEST("--generate-stack-traces=perhaps");
    _FAIL_TEST("--generate-stack-tracesgasg");

    _TEST( "--generate-crash-dumps", GENERATE_CRASH_DUMPS
         , "generate-crash-dumps", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_CRASH_DUMPS, true));
    _TEST( "--generate-crash-dumps=yes", GENERATE_CRASH_DUMPS
         , "generate-crash-dumps", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_CRASH_DUMPS, true));
    _TEST( "--generate-crash-dumps=no", GENERATE_CRASH_DUMPS
         , "generate-crash-dumps", NULL
         , BOOL, UNSAFE, BOOL_VAL(GENERATE_CRASH_DUMPS, false));
    _FAIL_TEST("--generate-crash-dumps=maybe");
    _FAIL_TEST("--generate-crash-dumpssss");

    _TEST( "--null-eventlog-writer", NULL_EVENTLOG_WRITER
         , "null-eventlog-writer", NULL
         , BOOL, UNSAFE, BOOL_VAL(NULL_EVENTLOG_WRITER, true));
    _TEST( "--null-eventlog-writer=yes", NULL_EVENTLOG_WRITER
         , "null-eventlog-writer", NULL
         , BOOL, UNSAFE, BOOL_VAL(NULL_EVENTLOG_WRITER, true));
    _TEST( "--null-eventlog-writer=no", NULL_EVENTLOG_WRITER
         , "null-eventlog-writer", NULL
         , BOOL, UNSAFE, BOOL_VAL(NULL_EVENTLOG_WRITER, false));
    _FAIL_TEST("--null-eventlog-writer=");
    _FAIL_TEST("--null-eventlog-writerytrwe");

    _TEST( "--machine-readable", MACHINE_READABLE
         , "machine-readable", NULL
         , BOOL, UNSAFE, BOOL_VAL(MACHINE_READABLE, true));
    _TEST( "--machine-readable=yes", MACHINE_READABLE
         , "machine-readable", NULL
         , BOOL, UNSAFE, BOOL_VAL(MACHINE_READABLE, true));
    _TEST( "--machine-readable=no", MACHINE_READABLE
         , "machine-readable", NULL
         , BOOL, UNSAFE, BOOL_VAL(MACHINE_READABLE, false));
    _FAIL_TEST("--machine-readable=treu");
    _FAIL_TEST("--machine-readableytrweasf");

    _TEST( "--disable-delayed-os-memory-return", DISABLE_OS_MEM_RET
         , "disable-delayed-os-memory-return", NULL
         , BOOL, UNSAFE, BOOL_VAL(DISABLE_OS_MEM_RET, true));
    _TEST( "--disable-delayed-os-memory-return=yes", DISABLE_OS_MEM_RET
         , "disable-delayed-os-memory-return", NULL
         , BOOL, UNSAFE, BOOL_VAL(DISABLE_OS_MEM_RET, true));
    _TEST( "--disable-delayed-os-memory-return=no", DISABLE_OS_MEM_RET
         , "disable-delayed-os-memory-return", NULL
         , BOOL, UNSAFE, BOOL_VAL(DISABLE_OS_MEM_RET, false));
    _FAIL_TEST("--disable-delayed-os-memory-return=flase");
    _FAIL_TEST("--disable-delayed-os-memory-returnysaftrweasf");

    _TEST( "--internal-counters", INTERNAL_COUNTERS
         , "internal-counters", NULL
         , BOOL, SAFE, BOOL_VAL(INTERNAL_COUNTERS, true));
    _TEST( "--internal-counters=yes", INTERNAL_COUNTERS
         , "internal-counters", NULL
         , BOOL, SAFE, BOOL_VAL(INTERNAL_COUNTERS, true));
    _TEST( "--internal-counters=no", INTERNAL_COUNTERS
         , "internal-counters", NULL
         , BOOL, SAFE, BOOL_VAL(INTERNAL_COUNTERS, false));
    _FAIL_TEST("--internal-counters=tutr");
    _FAIL_TEST("--internal-countersysaftrweasfasf");

    _TEST( "--io-manager=native", IO_MANAGER_FLAG
         , "io-manager", NULL
         , ENUM, UNSAFE, ENUM_VAL(IO_MANAGER_FLAG, IO_MNGR_NATIVE));
    _TEST( "--io-manager=posix", IO_MANAGER_FLAG
         , "io-manager", NULL
         , ENUM, UNSAFE, ENUM_VAL(IO_MANAGER_FLAG, IO_MNGR_POSIX));
    _FAIL_TEST("--io-manager");
    _FAIL_TEST("--io-manager=");
    _FAIL_TEST("--io-manager=unknown-manager");
    _FAIL_TEST("--io-managerlgaks");

    _TEST( "--info", INFO
        , "info", NULL
        , VOID, SAFE, NO_VAL(INFO));

    _TEST( "--eventlog-flush-interval=606.909", EVENTLOG_FLUSH_INTERVAL
         , "eventlog-flush-interval", NULL
         , DOUBLE, SAFE, DOUBLE_VAL(EVENTLOG_FLUSH_INTERVAL, 606.909));
    _TEST( "--eventlog-flush-interval=0.125", EVENTLOG_FLUSH_INTERVAL
         , "eventlog-flush-interval", NULL
         , DOUBLE, SAFE, DOUBLE_VAL(EVENTLOG_FLUSH_INTERVAL, 0.125));
    _FAIL_TEST("--eventlog-flush-interval");
    _FAIL_TEST("--eventlog-flush-interval=");
    _FAIL_TEST("--eventlog-flush-interval=true");
    _FAIL_TEST("--eventlog-flush-intervalysaftrweasfasf");

    _TEST( "--copying-gc", COPYING_GC
        , "copying-gc", NULL
        , VOID, SAFE, NO_VAL(COPYING_GC));

    _TEST( "--nonmoving-gc", NONMOVING_GC
        , "nonmoving-gc", NULL
        , VOID, SAFE, NO_VAL(NONMOVING_GC));

    _TEST( "--large-object-allocation=8193K", LARGE_OBJ_ALLOC_AREA
         , "large-object-allocation", "AL"
         , STGWORD64, UNSAFE, STGWORD64_VAL(LARGE_OBJ_ALLOC_AREA, 8389632 / BLOCK_SIZE));
    _TEST( "--large-object-allocation=2M", LARGE_OBJ_ALLOC_AREA
         , "large-object-allocation", "AL"
         , STGWORD64, UNSAFE, STGWORD64_VAL(LARGE_OBJ_ALLOC_AREA, 2097152 / BLOCK_SIZE));
    _TEST( "-AL9G", LARGE_OBJ_ALLOC_AREA
         , "large-object-allocation", "AL"
         , STGWORD64, UNSAFE, STGWORD64_VAL(LARGE_OBJ_ALLOC_AREA, 9663676416 / BLOCK_SIZE));
    _TEST( "-AL0.125G", LARGE_OBJ_ALLOC_AREA
         , "large-object-allocation", "AL"
         , STGWORD64, UNSAFE, STGWORD64_VAL(LARGE_OBJ_ALLOC_AREA, 134217728 / BLOCK_SIZE));
    _TEST( "-AL3333w", LARGE_OBJ_ALLOC_AREA
         , "large-object-allocation", "AL"
         , STGWORD64, UNSAFE, STGWORD64_VAL(LARGE_OBJ_ALLOC_AREA, 26664 / BLOCK_SIZE));
    _FAIL_TEST("-AL");
    _FAIL_TEST("--large-object-allocation");
    _FAIL_TEST("--large-object-allocation=");

    _TEST( "--minimum-allocation-area-size=8193K", MIN_ALLOC_AREA
         , "minimum-allocation-area-size", "A"
         , STGWORD64, UNSAFE, STGWORD64_VAL(MIN_ALLOC_AREA, 8389632 / BLOCK_SIZE));
    _TEST( "--minimum-allocation-area-size=2M", MIN_ALLOC_AREA
         , "minimum-allocation-area-size", "A"
         , STGWORD64, UNSAFE, STGWORD64_VAL(MIN_ALLOC_AREA, 2097152 / BLOCK_SIZE));
    _TEST( "-A9G", MIN_ALLOC_AREA
         , "minimum-allocation-area-size", "A"
         , STGWORD64, UNSAFE, STGWORD64_VAL(MIN_ALLOC_AREA, 9663676416 / BLOCK_SIZE));
    _TEST( "-A0.125G", MIN_ALLOC_AREA
         , "minimum-allocation-area-size", "A"
         , STGWORD64, UNSAFE, STGWORD64_VAL(MIN_ALLOC_AREA, 134217728 / BLOCK_SIZE));
    _TEST( "-A3333w", MIN_ALLOC_AREA
         , "minimum-allocation-area-size", "A"
         , STGWORD64, UNSAFE, STGWORD64_VAL(MIN_ALLOC_AREA, 26664 / BLOCK_SIZE));
    _FAIL_TEST("-A");
    _FAIL_TEST("--minimum-allocation-area-size");
    _FAIL_TEST("--minimum-allocation-area-size=");

    _TEST( "--io-manager-threads=1", IO_MANAGER_THREADS
         , "io-manager-threads", NULL
         , STGWORD64, UNSAFE, STGWORD64_VAL(IO_MANAGER_THREADS, 1));
    _TEST( "--io-manager-threads=1234567", IO_MANAGER_THREADS
         , "io-manager-threads", NULL
         , STGWORD64, UNSAFE, STGWORD64_VAL(IO_MANAGER_THREADS, 1234567));
    _FAIL_TEST("--io-manager-threads");
    _FAIL_TEST("--io-manager-threads=");

    _TEST( "--numa", NUMA
         , "numa", NULL
         , STGWORD64, SAFE, STGWORD64_VAL(NUMA, (StgWord)~0));
    _TEST( "--numa=1", NUMA
         , "numa", NULL
         , STGWORD64, SAFE, STGWORD64_VAL(NUMA, 1));
    _TEST( "--numa=1234567", NUMA
         , "numa", NULL
         , STGWORD64, SAFE, STGWORD64_VAL(NUMA, 1234567));
    _FAIL_TEST("--numa=");

    _TEST( "--debug-numa=1", DEBUG_NUMA
         , "debug-numa", NULL
         , STGWORD64, SAFE, STGWORD64_VAL(DEBUG_NUMA, 1));
    _TEST( "--debug-numa=8", DEBUG_NUMA
         , "debug-numa", NULL
         , STGWORD64, SAFE, STGWORD64_VAL(DEBUG_NUMA, 8));
    _FAIL_TEST("--debug-numa=999");
    _FAIL_TEST("--debug-numa999");
    _FAIL_TEST("--debug-numa=");
    _FAIL_TEST("--debug-numa");
    _FAIL_TEST("--debug-num");

    _TEST( "--long-gc-sync=606.909", LONG_GC_SYNC
         , "long-gc-sync", NULL
         , DOUBLE, SAFE, DOUBLE_VAL(LONG_GC_SYNC, 606.909));
    _TEST( "--long-gc-sync=0.125", LONG_GC_SYNC
         , "long-gc-sync", NULL
         , DOUBLE, SAFE, DOUBLE_VAL(LONG_GC_SYNC, 0.125));
    _FAIL_TEST("--long-gc-sync"); // this is now failure. previously it was a no-op ... ?
    _FAIL_TEST("--long-gc-sync=");
    _FAIL_TEST("--long-gc-sync=true");
    _FAIL_TEST("--long-gc-syncysaftrweasfasf");

    _TEST( "--no-automatic-heap-samples", NO_AUTO_HEAP_SAMPLES
         , "no-automatic-heap-samples", NULL
         , BOOL, UNSAFE, BOOL_VAL(NO_AUTO_HEAP_SAMPLES, true));
    _TEST( "--no-automatic-heap-samples=yes", NO_AUTO_HEAP_SAMPLES
         , "no-automatic-heap-samples", NULL
         , BOOL, UNSAFE, BOOL_VAL(NO_AUTO_HEAP_SAMPLES, true));
    _TEST( "--no-automatic-heap-samples=no", NO_AUTO_HEAP_SAMPLES
         , "no-automatic-heap-samples", NULL
         , BOOL, UNSAFE, BOOL_VAL(NO_AUTO_HEAP_SAMPLES, false));
    _FAIL_TEST("--no-automatic-heap-samples=");
    _FAIL_TEST("--no-automatic-heap-samples=foo");
    _FAIL_TEST("--no-automatic-heap-samplasfsg");

    _TEST( "--alloc-area-chunksize=16M", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 4096));
    _TEST( "-n16m", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 4096));
    _TEST( "--alloc-area-chunksize=1234567", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 301));
    _TEST( "-n1239999", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 302));
    _TEST( "--alloc-area-chunksize=0.225G", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 58982));
    _TEST( "-n99999999k", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 24999999));
    _TEST( "--alloc-area-chunksize=7654W", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 14));
    _TEST( "-n4567w", NURSERY_CHUNK_SIZE
         , "alloc-area-chunksize", "n"
         , STGWORD64, UNSAFE, STGWORD64_VAL(NURSERY_CHUNK_SIZE, 8));
    _FAIL_TEST("--alloc-area-chunksize");
    _FAIL_TEST("--alloc-area-chunksizr");
    _FAIL_TEST("--alloc-area-chunksizr=23M");
    _FAIL_TEST("--alloc-area-chunksize=");
//     _FAIL_TEST("--alloc-area-chunksize=0");
    _FAIL_TEST("-n");
    _FAIL_TEST("-n=");
//     _FAIL_TEST("-n0");
    _FAIL_TEST("-n=0");

    _TEST( "-B", GC_BELL
        , "gc-bell", "B"
        , VOID, UNSAFE, NO_VAL(GC_BELL));
         _TEST( "--gc-bell", GC_BELL
        , "gc-bell", "B"
        , VOID, UNSAFE, NO_VAL(GC_BELL));
    _FAIL_TEST("--gc-bell=");
    _FAIL_TEST("--gc-bell=123G");
    _FAIL_TEST("-B123G");

    _TEST( "--compact-gc", COMPACT_GC
         , "compact-gc", "c"
         , DOUBLE, UNSAFE, NO_VAL(COMPACT_GC));
    _TEST( "-c", COMPACT_GC
         , "compact-gc", "c"
         , DOUBLE, UNSAFE, NO_VAL(COMPACT_GC));
    _TEST( "--compact-gc=1125", COMPACT_GC
         , "compact-gc", "c"
         , DOUBLE, UNSAFE, DOUBLE_VAL(COMPACT_GC, 1125.0));
    _TEST( "-c", COMPACT_GC
         , "compact-gc", "c"
         , DOUBLE, UNSAFE, NO_VAL(COMPACT_GC));
    _FAIL_TEST("--compact-gc=");
    _FAIL_TEST("--compact-gc=blah");

    _TEST( "--use-mark-region", USE_MARK_REGION
        , "use-mark-region", "w"
        , VOID, UNSAFE, NO_VAL(USE_MARK_REGION));
    _TEST( "-w", USE_MARK_REGION
        , "use-mark-region", "w"
        , VOID, UNSAFE, NO_VAL(USE_MARK_REGION));
    _FAIL_TEST("--use-mark-region=");
    _FAIL_TEST("--use-mark-region=123G");
    _FAIL_TEST("--use-mark-region=false");
    _FAIL_TEST("-w3622");

    _TEST( "--old-gen-factor=11288", OLD_GEN_FACTOR
        , "old-gen-factor", "F"
        , DOUBLE, UNSAFE, DOUBLE_VAL(OLD_GEN_FACTOR, 11288.0));
    _TEST( "-F188", OLD_GEN_FACTOR
         , "old-gen-factor", "F"
        , DOUBLE, UNSAFE, DOUBLE_VAL(OLD_GEN_FACTOR, 188.0));
    _FAIL_TEST("--old-gen-factor");
    _FAIL_TEST("--old-gen-factor=");
    _FAIL_TEST("--old-gen-factor=blah");
    _FAIL_TEST("-F");
    _FAIL_TEST("-F=");
    _FAIL_TEST("-Fblah");

    _TEST( "--return-decay-factor=11288", RETURN_DECAY_FACTOR
        , "return-decay-factor", "Fd"
        , DOUBLE, UNSAFE, DOUBLE_VAL(RETURN_DECAY_FACTOR, 11288.0));
    _TEST( "-Fd188", RETURN_DECAY_FACTOR
        , "return-decay-factor", "Fd"
        , DOUBLE, UNSAFE, DOUBLE_VAL(RETURN_DECAY_FACTOR, 188.0));
    _FAIL_TEST("--return-decay-factor");
    _FAIL_TEST("--return-decay-factor=");
    _FAIL_TEST("--return-decay-factor=blah");
    _FAIL_TEST("-Fd");
    _FAIL_TEST("-Fd=");
    _FAIL_TEST("-Fdblah");

    _VOID_FLAG_TEST(DEBUG_SCHEDULER);
    _VOID_FLAG_TEST(DEBUG_INTERPRETER);
    _VOID_FLAG_TEST(DEBUG_WEAK);
    _VOID_FLAG_TEST(DEBUG_GCCAFS);
    _VOID_FLAG_TEST(DEBUG_GC);
    _VOID_FLAG_TEST(DEBUG_NONMOVING_GC);
    _VOID_FLAG_TEST(DEBUG_BLOCK_ALLOC);
    _VOID_FLAG_TEST(DEBUG_SANITY);
    _VOID_FLAG_TEST(DEBUG_ZERO_IN_GC);
    _VOID_FLAG_TEST(DEBUG_STABLE);
    _VOID_FLAG_TEST(DEBUG_PROF);
    _VOID_FLAG_TEST(DEBUG_LINKER);
    _VOID_FLAG_TEST(DEBUG_LINKER_VERBOSE);
    _VOID_FLAG_TEST(DEBUG_APPLY);
    _VOID_FLAG_TEST(DEBUG_STM);
    _VOID_FLAG_TEST(DEBUG_SQUEEZE);
    _VOID_FLAG_TEST(DEBUG_HPC);
    _VOID_FLAG_TEST(DEBUG_SPARKS);
    _VOID_FLAG_TEST(DEBUG_COMPACT);

    printf("\n=== OptParseTest END ===\n");
    return 0;
}
