# -----------------------------------------------------------------------------
# These tests we do even for 'make fast'

test('conc003', normal, compile_and_run, [''])
test('conc006', normal, compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc051', normal, compile_and_run, [''])

if ('threaded1' in config.run_ways):
   only_threaded_ways = only_ways(['ghci','threaded1','threaded2'])
else:
   only_threaded_ways = skip

test('conc069', only_threaded_ways, compile_and_run, [''])
test('conc069a', only_threaded_ways, compile_and_run, [''])
# this test gives slightly different results for non-threaded ways, so omit
# those for now.
test('conc070', only_threaded_ways, compile_and_run, [''])

test('conc071', omit_ways(['threaded2']), compile_and_run, [''])
test('conc072', only_ways(['threaded2']), compile_and_run, [''])
test('conc073', normal, compile_and_run, [''])

# vector code must get inlined to become non-allocating
test('T367',
     [reqlib('vector'), omit_ways(['ghci']), run_timeout_multiplier(0.001)],
     compile_and_run, ['-O2 -fno-omit-yields'])
test('T367_letnoescape',
     [run_timeout_multiplier(0.02), expect_broken_for(7297,['optllvm'])],
     compile_and_run, ['-fno-omit-yields'])

test('T1980', normal, compile_and_run, [''])
test('T2910', normal, compile_and_run, [''])
test('T2910a', normal, compile_and_run, [''])
test('T3279', normal, compile_and_run, [''])

# This test takes a long time with the default context switch interval
test('T3429', [ extra_run_opts('+RTS -C0.001 -RTS'),
                extra_ways(['debug_numa']) ],
     compile_and_run, [''])

# without -O, goes into an infinite loop
# GHCi does not detect the infinite loop.  We should really fix this.
test('T4030', omit_ways('ghci'), compile_and_run, ['-O'])

# each of these runs for about a second
test('throwto001', [reqlib('random'), extra_run_opts('1000 2000')],
                   compile_and_run, [''])
test('throwto002', [reqlib('random')], compile_and_run, [''])
test('throwto003', normal, compile_and_run, [''])

test('mask001', normal, compile_and_run, [''])
test('mask002', normal, compile_and_run, [''])

test('async001', normal, compile_and_run, [''])

test('numsparks001', only_ways(['threaded1']), compile_and_run, [''])

test('T4262', [ skip, # skip for now, it doesn't give reliable results
                only_ways(['threaded1']),
                unless(opsys('linux'),skip) ],
              compile_and_run, [''])

test('T4813', normal, compile_and_run, [''])
test('T4811', normal, compile_and_run, [''])

test('allowinterrupt001', normal, compile_and_run, [''])

# try hard to provoke the error by running the test 100 times
test('T5558',
     [ only_ways(['threaded2']),
       cmd_wrapper(
         lambda c: "for i in %s; do %s || break; done"  %
           (" ".join(str(i) for i in range(1,101)), c)) ],
     compile_and_run, [''])

test('T5421', normal, compile_and_run, [''])
# See #12751, the test fails non-deterministically so we skip it.
test('T5611', when ( opsys('darwin'), skip) , compile_and_run, [''])
test('T5238', normal, compile_and_run, [''])
test('T5866', exit_code(1), compile_and_run, [''])

test('readMVar1', normal, compile_and_run, [''])
test('readMVar2', normal, compile_and_run, [''])
test('readMVar3', normal, compile_and_run, [''])
test('tryReadMVar1', normal, compile_and_run, [''])
test('tryReadMVar2', normal, compile_and_run, [''])

test('T7970', normal, compile_and_run, [''])
test('AtomicPrimops', normal, compile_and_run, [''])

# test uses 2 threads and yield, scheduling can vary with threaded2
test('threadstatus-9333', [omit_ways(['threaded2'])], compile_and_run, [''])

test('T9379', normal, compile_and_run, [''])

test('allocLimit1', exit_code(1), compile_and_run, [''])
test('allocLimit2', normal, compile_and_run, [''])

# The non-threaded RTS on Windows doesn't handle throwing exceptions at I/O
# operations very well, and ends up duplicating the I/O, giving wrong results.
test('allocLimit3', [ when(opsys('mingw32'), only_ways(threaded_ways)),
                      exit_code(1) ], compile_and_run, [''])

test('allocLimit4', [ extra_run_opts('+RTS -xq300k -RTS'),
                      # ghci consumes part of set limit at startup
                      omit_ways(['ghci']) ],
                    compile_and_run, [''])

test('performGC', [ only_ways(['threaded1','threaded2']), req_smp,
                    reqlib('random'),
                    extra_run_opts('400 +RTS -qg -RTS') ],
                    compile_and_run, [''])

# -----------------------------------------------------------------------------
# These tests we only do for a full run

setTestOpts(when(fast(), skip))

test('conc001', normal, compile_and_run, [''])
test('conc002', normal, compile_and_run, [''])

# Omit GHCi way - it blows up to 0.5G.  Something to do with the threaded RTS?
test('conc004', omit_ways(['ghci']), compile_and_run, [''])

test('conc007', extra_run_opts('+RTS -H128M -RTS'), compile_and_run, [''])
test('conc008', normal, compile_and_run, [''])
test('conc009', exit_code(1), compile_and_run, [''])
test('conc010', normal, compile_and_run, [''])

# conc012(ghci) needs a smaller stack, or it takes forever
test('conc012',
     extra_run_opts('+RTS -K8m -RTS'),
     compile_and_run, [''])

test('conc013', normal, compile_and_run, [''])
test('conc014', normal, compile_and_run, [''])
test('conc015', normal, compile_and_run, [''])
test('conc015a', normal, compile_and_run, [''])
test('conc016', omit_ways(['threaded2']), # see comment in conc016.hs
                compile_and_run, [''])
test('conc017', normal, compile_and_run, [''])
test('conc017a', normal, compile_and_run, [''])
test('conc018', normal, compile_and_run, [''])
test('conc019', extra_run_opts('+RTS -K16m -RTS'), compile_and_run, [''])
test('conc020', normal, compile_and_run, [''])
test('conc021', [ omit_ways(['ghci']), exit_code(1) ], compile_and_run, [''])
test('conc022', normal, compile_and_run, [''])

# On Windows, the non-threaded RTS creates a real OS thread for each
# threadDelay.  conc023 creates 5000 concurrent threadDelays, and the
# resulting creation of OS threads seems to cause the system to run
# out of memory sometimes (I'm not sure exactly how/why this happens,
# but the threaded RTS fixes it).  See #1197.
if config.platform == 'i386-unknown-mingw32':
   conc023_ways = only_ways(['threaded1','threaded2'])
else:
   conc023_ways = normal

test('conc023', [when(fast(), skip),
                 reqlib('random'), multi_cpu_race,
                 conc023_ways], compile_and_run, [''])

test('conc024', normal, compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', normal, compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030', extra_run_opts('+RTS -K4M -RTS'), compile_and_run, [''])

test('conc031', exit_code(1), compile_and_run, [''])

test('conc032', normal, compile_and_run, [''])
test('conc033', normal, compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
test('conc034', [
                 normal,
                 omit_ways(['ghci']),
                 extra_run_opts('+RTS -C0 -RTS')],
		compile_and_run, [''])

test('conc035', normal, compile_and_run, [''])

# conc036: now skipped entirely.  This test attempts to check that
# unsafe foreign calls block other threads, but doing that reliably is
# probably impossible.  The last straw was when it started failing on
# Windows around 7.4.x.
#
# OLD comments about hacks we tried to make it work:
#   Omit for GHCi: firstly GHCi doesn't have unsafe FFI calls, and secondly
#   the main thread cannot receive the deadlock exception because it can be
#   woken up by ^C.
#   Omit for threaded2: this test is really bogus and fails to do anything
#   sensible for more than one CPU.
#
test('conc036', skip, compile_and_run, [''])


# Interrupting foreign calls only makes sense if we are threaded
test('foreignInterruptible', [when(fast(), skip),
                              when(opsys('mingw32'),expect_fail),
                              # I don't think we support interrupting Sleep()
                              # on Windows.  --SDM
                              only_threaded_ways,
                              ],
      compile_and_run, [''])

test('conc037', only_ways(['threaded1','threaded2']), compile_and_run, [''])
test('conc038', only_ways(['threaded1','threaded2']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit for the threaded ways, because in this case the main thread is allowed to
# complete, which causes the child thread to be interrupted.
test('conc039', omit_ways(['ghci'] + threaded_ways), compile_and_run, [''])

# Omit for GHCi, uses foreign export
test('conc040', [exit_code(1), omit_ways(['ghci'])], compile_and_run, [''])

# STM-related tests.
test('conc041', normal, compile_and_run, [''])
test('conc042', normal, compile_and_run, [''])
test('conc043', normal, compile_and_run, [''])
test('conc044', normal, compile_and_run, [''])
test('conc045', normal, compile_and_run, [''])

test('conc058', normal, compile_and_run, [''])

test('conc059',
     [only_ways(['threaded1', 'threaded2']),
      pre_cmd('$MAKE -s --no-print-directory conc059_setup')],
     compile_and_run, ['conc059_c.c -no-hs-main'])

# This test sometimes just exits successfully
# when run the threaded2 way. The problem hasn't been diagnosed yet
test('conc064',
     exit_code(1),
     compile_and_run, [''])

test('conc065', ignore_stdout, compile_and_run, [''])
test('conc066', ignore_stdout, compile_and_run, [''])
test('conc067', ignore_stdout, compile_and_run, [''])

# omit threaded2, the behaviour of this test is non-deterministic with more
# than one CPU.
test('conc068', [ omit_ways('threaded2'), exit_code(1) ], compile_and_run, [''])

test('setnumcapabilities001',
     [ only_ways(['threaded1','threaded2']),
       extra_run_opts('8 12 2000'),
       req_smp ],
     compile_and_run, [''])

# omit ghci, which can't handle unboxed tuples:
test('compareAndSwap', [omit_ways(['ghci','hpc']), reqlib('primitive')], compile_and_run, [''])

test('hs_try_putmvar001',
     [
     when(opsys('mingw32'),skip), # uses pthread APIs in the C code
     only_ways(['threaded1','threaded2']),
      extra_clean(['hs_try_putmvar001_c.o'])],
     compile_and_run,
     ['hs_try_putmvar001_c.c'])

# A benchmark for hs_try_putmvar() vs. foreign export
# This one should work for both threaded and non-threaded RTS
test('hs_try_putmvar002',
     [pre_cmd('$MAKE -s --no-print-directory hs_try_putmvar002_setup'),
      omit_ways(['ghci']),
      extra_run_opts('1 8 10000')],
     compile_and_run, ['hs_try_putmvar002_c.c'])

# Another benchmark for hs_try_putmvar() vs. foreign export
test('hs_try_putmvar003',
     [
     when(opsys('mingw32'),skip), # uses pthread APIs in the C code
     pre_cmd('$MAKE -s --no-print-directory hs_try_putmvar003_setup'),
     only_ways(['threaded1','threaded2']),
     extra_clean(['hs_try_putmvar003_c.o']),
     extra_run_opts('1 16 32 100')
     ],
     compile_and_run,
     ['hs_try_putmvar003_c.c'])

# Check forkIO exception determinism under optimization
test('T13330', normal, compile_and_run, ['-O'])
test('T13916', [reqlib('vector'), reqlib('stm'), reqlib('async')],
     compile_and_run, ['-O2'])
