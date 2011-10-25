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

test('1980', normal, compile_and_run, [''])
test('2910', normal, compile_and_run, [''])
test('2910a', normal, compile_and_run, [''])
test('3279', normal, compile_and_run, [''])

# This test takes a long time with the default context switch interval
test('3429', extra_run_opts('+RTS -i0.001 -RTS'), compile_and_run, [''])

# without -O, goes into an infinite loop
# GHCi cannot deterct the infinite loop, because the thread is always reachable
# (see also conc033 and others).  We should really fix this.
test('4030', omit_ways('ghci'), compile_and_run, ['-O'])

# each of these runs for about a second
test('throwto001', [reqlib('random'), extra_run_opts('1000 2000')],
                   compile_and_run, [''])
test('throwto002', [reqlib('random')], compile_and_run, [''])
test('throwto003', normal, compile_and_run, [''])

test('mask001', normal, compile_and_run, [''])
# ghci does not generate the BlockedIndefinitely exceptions, so omit:
test('mask002', omit_ways(['ghci']), compile_and_run, [''])

test('async001', normal, compile_and_run, [''])

test('numsparks001', only_ways(['threaded1']), compile_and_run, [''])

test('4262', [ skip, # skip for now, it doesn't give reliable results
               only_ways(['threaded1']),
               unless_os('linux',skip),
               if_compiler_lt('ghc', '7.1', expect_fail) ],
             compile_and_run, [''])

test('4813', normal, compile_and_run, [''])
test('4811', normal, compile_and_run, [''])

test('allowinterrupt001', normal, compile_and_run, [''])

# try hard to provoke the error by running the test 100 times
test('5558',
     [ only_ways(['threaded2']),
       cmd_wrapper(lambda c: 'for i in `seq 1 100`; do ' + c + ' || break; done') ],
     compile_and_run, [''])

# -----------------------------------------------------------------------------
# These tests we only do for a full run

def f( opts ):
  if config.fast:
  	opts.skip = 1

setTestOpts(f)

test('conc001', normal, compile_and_run, [''])
test('conc002', normal, compile_and_run, [''])

# Omit GHCi way - it blows up to 0.5G.  Something to do with the threaded RTS?
test('conc004', omit_ways(['ghci']), compile_and_run, [''])

test('conc007', compose(only_compiler_types(['ghc']),
			extra_run_opts('+RTS -H128M -RTS')),
		compile_and_run, [''])
test('conc008', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc009', compose(only_compiler_types(['ghc']), exit_code(1)),
		compile_and_run, [''])
test('conc010', only_compiler_types(['ghc']), compile_and_run, [''])

# conc012(ghci) needs a smaller stack, or it takes forever
test('conc012', normal, compile_and_run, ['+RTS -K8m -RTS'])

test('conc013', only_compiler_types(['ghc']), compile_and_run, [''])

test('conc014', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc015', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc015a', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc016', [ omit_ways(['threaded2']), # see comment in conc016.hs
                  only_compiler_types(['ghc']) ], compile_and_run, [''])
test('conc017', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc017a', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc018', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc019', compose(only_compiler_types(['ghc']),
			extra_run_opts('+RTS -K16m -RTS')),
		compile_and_run, [''])
test('conc020', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc021', compose(omit_ways(['ghci']), exit_code(1)),
	compile_and_run, [''])
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

test('conc023', composes([skip_if_fast,
                          only_compiler_types(['ghc']),
                          reqlib('random'),
                          conc023_ways]), compile_and_run, [''])

test('conc024', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030', compose(only_compiler_types(['ghc']),
			extra_run_opts('+RTS -K4M -RTS')),
		compile_and_run, [''])

test('conc031', normal, compile_and_run, [''])

test('conc032', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
test('conc033', omit_ways(['ghci']), compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
test('conc034', compose(only_compiler_types(['ghc']),
			compose(omit_ways(['ghci']), 
			extra_run_opts('+RTS -C0 -RTS'))),
		compile_and_run, [''])

test('conc035', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit for GHCi: firstly GHCi doesn't have unsafe FFI calls, and secondly
# the main thread cannot receive the deadlock exception because it can be
# woken up by ^C.
# Omit for threaded2: this test is really bogus and fails to do anything
# sensible for more than one CPU.
test('conc036', compose(skip_if_fast,
		  compose(omit_ways(['ghci','threaded2']),
		    only_compiler_types(['ghc']))), compile_and_run, [''])
# Interrupting foreign calls only makes sense if we are threaded
test('foreignInterruptible', composes([skip_if_fast,
			               only_threaded_ways,
				       only_compiler_types(['ghc'])]), compile_and_run, [''])

test('conc037', only_ways(['threaded1','threaded2']), compile_and_run, [''])
test('conc038', only_ways(['threaded1','threaded2']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit for the threaded ways, because in this case the main thread is allowed to 
# complete, which causes the child thread to be interrupted.
test('conc039', omit_ways(['ghci','threaded1','threaded2','profthreaded']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
test('conc040', compose(only_compiler_types(['ghc']),
			compose(exit_code(1),
			omit_ways(['ghci']))),
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
      compile_cmd_prefix('$MAKE conc059_setup && '),
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
