# -----------------------------------------------------------------------------
# These tests we do even for 'make fast'

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

test('conc003', normal, compile_and_run, [''])
test('conc006', normal, compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc051', normal, compile_and_run, [''])

if ('threaded1' in config.run_ways):
   threaded_ways = ['ghci','threaded1','threaded2']
else:
   threaded_ways = []

test('conc069', only_ways(threaded_ways), compile_and_run, [''])
# this test gives slightly different results for non-threaded ways, so omit
# those for now.
test('conc070', only_ways(threaded_ways), compile_and_run, [''])

test('1980', normal, compile_and_run, [''])
test('2910', normal, compile_and_run, [''])

# -----------------------------------------------------------------------------
# These tests we only do for a full run

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'
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
test('conc012', normal, compile_and_run, [''])

test('conc013', only_compiler_types(['ghc']), compile_and_run, [''])

test('conc014', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc015', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc016', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc017', only_compiler_types(['ghc']), compile_and_run, [''])
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
      extra_clean(['conc059_c.o'])],
     compile_and_run,
     ['conc059_c.c'])

# This test sometimes just exits successfully
# when run the threaded2 way. The problem hasn't been diagnosed yet
test('conc064',
     exit_code(1),
     compile_and_run, [''])

test('conc065', ignore_output, compile_and_run, [''])
test('conc066', ignore_output, compile_and_run, [''])
test('conc067', ignore_output, compile_and_run, [''])
test('conc068', exit_code(1), compile_and_run, [''])

