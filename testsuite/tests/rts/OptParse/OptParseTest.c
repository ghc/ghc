#include <stdio.h>
#include "Rts.h"
#include "OptParseTestUtil.h"

#define SAFE true
#define UNSAFE false

bool ERROR = false;

int main (int argc, char *argv[])
{

     printf("=== OptParseTest START ===\n");

     _TEST( "-?", HELP
          , NULL, "?"
          , VOID, SAFE, NO_VAL(HELP));
     // _FAIL_TEST("-?asfg");

     _BOOL_FLAG_TEST(INSTALL_SIGNAL_HANDLERS);
     _BOOL_FLAG_TEST(INSTALL_SEH_HANDLERS);
     _BOOL_FLAG_TEST(GENERATE_STACK_TRACES);
     _BOOL_FLAG_TEST(GENERATE_CRASH_DUMPS);
     _BOOL_FLAG_TEST(NULL_EVENTLOG_WRITER);
     _BOOL_FLAG_TEST(MACHINE_READABLE);
     _BOOL_FLAG_TEST(DISABLE_OS_MEM_RET);
     _BOOL_FLAG_TEST(INTERNAL_COUNTERS);

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

     _BOOL_FLAG_TEST(NO_AUTO_HEAP_SAMPLES);

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
