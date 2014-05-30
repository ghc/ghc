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
test('T367', [reqlib('vector'), omit_ways(['ghci']), timeout_multiplier(0.001)], compile_and_run, ['-O2 -fno-omit-yields'])
test('T367_letnoescape', [timeout_multiplier(0.02)], compile_and_run, ['-fno-omit-yields'])

test('T1980', normal, compile_and_run, [''])
test('T2910', normal, compile_and_run, [''])
test('T2910a', normal, compile_and_run, [''])
test('T3279', normal, compile_and_run, [''])

# This test takes a long time with the default context switch interval
test('T3429', extra_run_opts('+RTS -C0.001 -RTS'), compile_and_run, [''])

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
                unless(opsys('linux'),skip),
                when(compiler_lt('ghc', '7.1'), expect_fail) ],
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
test('T5611', normal, compile_and_run, [''])
test('T5238', normal, compile_and_run, [''])
test('T5866', exit_code(1), compile_and_run, [''])

test('readMVar1', normal, compile_and_run, [''])
test('readMVar2', normal, compile_and_run, [''])
test('readMVar3', normal, compile_and_run, [''])
test('tryReadMVar1', normal, compile_and_run, [''])
test('tryReadMVar2', normal, compile_and_run, [''])

test('T7970', normal, compile_and_run, [''])

# -----------------------------------------------------------------------------
# These tests we only do for a full run

def f( name, opts ):
  if config.fast:
  	opts.skip = 1

setTestOpts(f)

test('conc001', normal, compile_and_run, [''])
test('conc002', normal, compile_and_run, [''])

# Omit GHCi way - it blows up to 0.5G.  Something to do with the threaded RTS?
test('conc004', omit_ways(['ghci']), compile_and_run, [''])

test('conc007', [only_compiler_types(['ghc']),
                 extra_run_opts('+RTS -H128M -RTS')],
		compile_and_run, [''])
test('conc008', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc009', [only_compiler_types(['ghc']), exit_code(1)],
		compile_and_run, [''])
test('conc010', only_compiler_types(['ghc']), compile_and_run, [''])

# conc012(ghci) needs a smaller stack, or it takes forever
test('conc012', [only_compiler_types(['ghc']),
                 extra_run_opts('+RTS -K8m -RTS')],
                compile_and_run, [''])

test('conc013', only_compiler_types(['ghc']), compile_and_run, [''])

test('conc014', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc015', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc015a', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc016', [ omit_ways(['threaded2']), # see comment in conc016.hs
                  only_compiler_types(['ghc']) ], compile_and_run, [''])
test('conc017', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc017a', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc018', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc019', [only_compiler_types(['ghc']),
                 extra_run_opts('+RTS -K16m -RTS')],
		compile_and_run, [''])
test('conc020', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc021', [ omit_ways(['ghci']), exit_code(1) ], compile_and_run, [''])
test('conc022', only_compiler_types(['ghc']), compile_and_run, [''])

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
                 only_compiler_types(['ghc']),
                 reqlib('random'),
                 conc023_ways], compile_and_run, [''])

test('conc024', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030',
     [only_compiler_types(['ghc']), extra_run_opts('+RTS -K4M -RTS')],
		compile_and_run, [''])

test('conc031', exit_code(1), compile_and_run, [''])

test('conc032', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc033', normal, compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
test('conc034', [only_compiler_types(['ghc']),
                 omit_ways(['ghci']), 
                 extra_run_opts('+RTS -C0 -RTS')],
		compile_and_run, [''])

test('conc035', only_compiler_types(['ghc']), compile_and_run, [''])

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
                              only_compiler_types(['ghc'])],
      compile_and_run, [''])

test('conc037', only_ways(['threaded1','threaded2']), compile_and_run, [''])
test('conc038', only_ways(['threaded1','threaded2']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit for the threaded ways, because in this case the main thread is allowed to 
# complete, which causes the child thread to be interrupted.
test('conc039', omit_ways(['ghci','threaded1','threaded2','profthreaded']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
test('conc040',
     [only_compiler_types(['ghc']), exit_code(1), omit_ways(['ghci'])],
		compile_and_run, [''])

# STM-related tests.
test('conc041', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc042', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc043', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc044', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc045', only_compiler_types(['ghc']), compile_and_run, [''])

test('conc058', only_compiler_types(['ghc']), compile_and_run, [''])

test('conc059',
     [only_compiler_types(['ghc']),
      only_ways(['threaded1','threaded2']),
      pre_cmd('$MAKE -s --no-print-directory conc059_setup'),
      extra_clean(['conc059_c.o'])],
     compile_and_run,
     ['conc059_c.c -no-hs-main'])

# This test sometimes just exits successfully
# when run the threaded2 way. The problem hasn't been diagnosed yet
test('conc064',
     exit_code(1),
     compile_and_run, [''])

test('conc065', ignore_output, compile_and_run, [''])
test('conc066', ignore_output, compile_and_run, [''])
test('conc067', ignore_output, compile_and_run, [''])

# omit threaded2, the behaviour of this test is non-deterministic with more
# than one CPU.
test('conc068', [ omit_ways('threaded2'), exit_code(1) ], compile_and_run, [''])

if (ghc_with_smp == 0):
   skip_if_not_smp = skip
else:
   skip_if_not_smp = normal

test('setnumcapabilities001',
     [ only_ways(['threaded1','threaded2']),
       extra_run_opts('4 12 2000'),
       reqlib('parallel'),
       skip_if_not_smp ],
     compile_and_run, [''])

# omit ghci, which can't handle unboxed tuples:
test('compareAndSwap', [omit_ways(['ghci','hpc']), reqlib('primitive')], compile_and_run, [''])
