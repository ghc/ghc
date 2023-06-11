# -----------------------------------------------------------------------------
# These tests we do even for 'make fast'

test('conc003', normal, compile_and_run, [''])
test('conc006', normal, compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc051', normal, compile_and_run, [''])

if ('threaded1' in config.run_ways):
   only_threaded_ways = only_ways(['ghci','threaded1','threaded2', 'nonmoving_thr'])
else:
   only_threaded_ways = skip

test('conc069', only_threaded_ways, compile_and_run, [''])
test('conc069a', only_threaded_ways, compile_and_run, [''])
# this test gives slightly different results for non-threaded ways, so omit
# those for now.
test('conc070', only_threaded_ways, compile_and_run, [''])

test('conc071',
  [ omit_ways(concurrent_ways)
  , js_skip # JS RTS doesn't report the same cap/locked status
  ] , compile_and_run, [''])
test('conc072', only_ways(concurrent_ways), compile_and_run, [''])
test('conc073', normal, compile_and_run, [''])

test('T367_letnoescape',
     [run_timeout_multiplier(0.02), expect_broken_for(7297,['optllvm']),
     # wasm32 rts has no timer and scheduler always behaves as -C0,
     # see Note [No timer on wasm32] in rts. It'll be stuck in the
     # thread that infinitely loops, which is the expected behavior.
     when(arch('wasm32'), skip)],
     compile_and_run, ['-fno-omit-yields'])

test('T1980', normal, compile_and_run, [''])
test('T2910', normal, compile_and_run, [''])
test('T2910a', normal, compile_and_run, [''])
test('T3279', normal, compile_and_run, [''])

# This test takes a long time with the default context switch interval
test('T3429', [ extra_run_opts('+RTS -C0.001 -RTS'),
                req_ghc_with_threaded_rts,
                unless(unregisterised(), extra_ways(['debug_numa'])) ],
     compile_and_run, [''])

# without -O, goes into an infinite loop
# times out with ghci
test('T4030', omit_ghci, compile_and_run, ['-O'])

test('throwto002', js_fragile(24259), compile_and_run, [''])
test('throwto003', normal, compile_and_run, [''])

test('mask001', normal, compile_and_run, [''])
test('mask002', js_broken(22261), compile_and_run, [''])

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
     [ only_ways(concurrent_ways),
       cmd_wrapper(
         lambda c: "for i in %s; do %s || break; done"  %
           (" ".join(str(i) for i in range(1,101)), c)) ],
     compile_and_run, [''])

test('T5421', normal, compile_and_run, [''])
test('T5611', fragile(12751), compile_and_run, [''])
test('T5611a', fragile(12751), compile_and_run, [''])
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
test('threadstatus-9333', [fragile_for(16555, ['ghci', 'profthreaded']), omit_ways(concurrent_ways),
# wasm32 rts has no timer and scheduler always behaves as -C0. See
# Note [No timer on wasm32] in rts
when(arch('wasm32'), skip)], compile_and_run, [''])

test('T9379', normal, compile_and_run, [''])

# Skip the allocLimit* tests with the JS backend as it doesn't support
# setThreadAllocationCounter

test('allocLimit1', [exit_code(1), js_skip], compile_and_run, [''])
test('allocLimit2', js_skip, compile_and_run, [''])

# The non-threaded RTS on Windows doesn't handle throwing exceptions at I/O
# operations very well, and ends up duplicating the I/O, giving wrong results.
test('allocLimit3', [ when(opsys('mingw32'), only_ways(threaded_ways)),
                      js_skip,
                      exit_code(1) ], compile_and_run, [''])

test('allocLimit4', [ extra_run_opts('+RTS -xq300k -RTS'),
                      js_skip,
                      # ghci consumes part of set limit at startup
                      omit_ghci ],
                    compile_and_run, [''])

# -----------------------------------------------------------------------------
# These tests we only do for a full run

setTestOpts(when(fast(), skip))

test('conc001', normal, compile_and_run, [''])
test('conc002', normal, compile_and_run, [''])
test('conc004', normal, compile_and_run, [''])
test('conc007', extra_run_opts('+RTS -H128M -RTS'), compile_and_run, [''])
test('conc008', normal, compile_and_run, [''])
test('conc009', exit_code(1), compile_and_run, [''])
test('conc010', normal, compile_and_run, [''])

# conc012(ghci) needs a smaller stack, or it takes forever
test('conc012',
     [ extra_run_opts('+RTS -K8m -RTS')
     , js_skip # no stack overflow detection with the JS backend (yet)
     ],
     compile_and_run, [''])

test('conc013', normal, compile_and_run, [''])
test('conc014', normal, compile_and_run, [''])
test('conc015',
  [ when(arch("javascript"), fragile(22261)) # delays are flaky with the JS backend when the system is overloaded
  ], compile_and_run, [''])
test('conc015a', normal, compile_and_run, [''])
test('conc016', [omit_ways(concurrent_ways)  # see comment in conc016.hs
                , js_skip
                ],
                compile_and_run, [''])
test('conc017', normal, compile_and_run, [''])
test('conc017a', normal, compile_and_run, [''])
test('conc018', normal, compile_and_run, [''])
test('conc019', [extra_run_opts('+RTS -K16m -RTS')], compile_and_run, [''])
test('conc020', normal, compile_and_run, [''])
test('conc021',
  [ omit_ghci, exit_code(1)
  , js_skip # foreign exports not supported yet
  ],
  compile_and_run, [''])
test('conc022', normal, compile_and_run, [''])

test('conc024', when(opsys('mingw32'), fragile(17733)), compile_and_run, [''])
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
                 omit_ghci,
                 extra_run_opts('+RTS -C0 -RTS'),
                 js_skip],
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
                              when(unregisterised(), fragile(15467)),
                              fragile_for(18391, ['ghci', 'ghci-ext', 'ghci-ext-prof']),
                              # I don't think we support interrupting Sleep()
                              # on Windows.  --SDM
                              only_threaded_ways,
                              ],
      compile_and_run, [''])

test('conc037', only_ways(['threaded1', 'threaded2', 'nonmoving_thr']), compile_and_run, [''])
test('conc038', only_ways(['threaded1', 'threaded2', 'nonmoving_thr']), compile_and_run, [''])

# Omit for GHCi and for the JS backend, uses foreign export
# Omit for the threaded ways, because in this case the main thread is allowed to
# complete, which causes the child thread to be interrupted.
test('conc039', [omit_ways(ghci_ways + threaded_ways), js_skip], compile_and_run, [''])

# Omit for GHCi and for the JS backend, uses foreign export
test('conc040', [exit_code(1), omit_ghci, js_skip], compile_and_run, [''])

# STM-related tests.
test('conc041', normal, compile_and_run, [''])
test('conc042', normal, compile_and_run, [''])
test('conc043', normal, compile_and_run, [''])
test('conc044', normal, compile_and_run, [''])
test('conc045', normal, compile_and_run, [''])

test('conc058', normal, compile_and_run, [''])

test('conc059',
     [only_ways(['threaded1', 'threaded2', 'nonmoving_thr']),
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
test('conc068', [ omit_ways(concurrent_ways), exit_code(1) ], compile_and_run, [''])

test('setnumcapabilities001',
     [ only_ways(['threaded1','threaded2', 'nonmoving_thr', 'profthreaded']),
       extra_run_opts('8 12 2000'),
       when(have_thread_sanitizer(), expect_broken(18808)),
       req_target_smp,
       req_ghc_smp,
       fragile(22989)
     ],
     compile_and_run, [''])

test('T21651',
     [ only_ways(['threaded1','threaded2', 'nonmoving_thr', 'profthreaded']),
       when(opsys('mingw32'),skip), # uses POSIX pipes
       when(opsys('darwin'),extra_run_opts('8 12 2000 100')),
       unless(opsys('darwin'),extra_run_opts('8 12 2000 200')), # darwin runners complain of too many open files
       req_target_smp,
       req_ghc_smp
     ],
     compile_and_run, [''])

test('T21969',
     [ only_ways(['threaded1','threaded2', 'nonmoving_thr', 'profthreaded']),
       when(opsys('mingw32'),skip), # uses POSIX pipes
       extra_run_opts('50000'),
       run_timeout_multiplier(0.3), # default timeout seems to be 300, but lockups happen quickly
       req_target_smp,
       req_ghc_smp
     ],
     compile_and_run, [''])

test('hs_try_putmvar001',
     [
     when(opsys('mingw32'),skip), # uses pthread APIs in the C code
     only_ways(['threaded1', 'threaded2', 'nonmoving_thr']),
     req_c
     ],
     compile_and_run,
     ['hs_try_putmvar001_c.c'])

# A benchmark for hs_try_putmvar() vs. foreign export
# This one should work for both threaded and non-threaded RTS
test('hs_try_putmvar002',
     [pre_cmd('$MAKE -s --no-print-directory hs_try_putmvar002_setup'),
      omit_ghci,
      req_c,
      extra_run_opts('1 8 10000')],
     compile_and_run, ['hs_try_putmvar002_c.c'])

# Another benchmark for hs_try_putmvar() vs. foreign export
test('hs_try_putmvar003',
     [
     when(opsys('mingw32'),skip), # uses pthread APIs in the C code
     pre_cmd('$MAKE -s --no-print-directory hs_try_putmvar003_setup'),
     only_ways(['threaded1', 'threaded2', 'nonmoving_thr']),
     req_c,
     extra_run_opts('1 16 32 100'),
     fragile_for(16361, ['threaded1'])
     ],
     compile_and_run,
     ['hs_try_putmvar003_c.c'])

# Check forkIO exception determinism under optimization
test('T13330', normal, compile_and_run, ['-O'])
