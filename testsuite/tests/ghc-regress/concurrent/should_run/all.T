# -----------------------------------------------------------------------------
# These tests we do even for 'make fast'

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

test('conc003', normal, compile_and_run, [''])
test('conc006', normal, compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc049', only_compiler_types(['ghc']), compile_and_run, ['-package stm'])
test('conc051', normal, compile_and_run, [''])

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
test('conc023', compose(skip_if_fast,only_compiler_types(['ghc'])), compile_and_run, [''])
test('conc024', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030', compose(only_compiler_types(['ghc']),
			extra_run_opts('+RTS -K2M -RTS')),
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
test('conc039', omit_ways(['ghci','threaded1','threaded2']), compile_and_run, [''])

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
test('conc046', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit GHCi for these two, since they appear to deadlock (23/11/2004 --SDM)
test('conc047', compose(only_compiler_types(['ghc']), 
                        omit_ways(['ghci'])), compile_and_run, [''])
test('conc048', compose(only_compiler_types(['ghc']), 
                        omit_ways(['ghci'])), compile_and_run, [''])

test('conc050', compose(only_compiler_types(['ghc']), extra_run_opts('10000')), compile_and_run, ['-package stm'])

test('conc051', normal, compile_and_run, [''])

test('conc052', normal, compile_and_run, ['-package stm'])

test('conc053', compose(only_ways(['threaded1','threaded2']),
			skip_if_platform('i386-unknown-mingw32')),
		 compile_and_run, ['-package stm'])
test('conc054', normal, compile_and_run, ['-package stm'])
test('conc055', exit_code(1), compile_and_run, ['-package stm'])
test('conc056', compose(only_ways(['threaded1','threaded2']),
                        reqlib('network')),
                compile_and_run, ['-package stm -package network'])

test('conc057', only_ways(['threaded2']), compile_and_run, ['-O0'])

if config.platform == "i386-unknown-mingw32":
    config058 = expect_fail_for(['ghci','threaded1','threaded2'])
else:
    config058 = normal

test('conc058', compose(only_compiler_types(['ghc']), config058),
	 compile_and_run, [''])

test('conc059', compose(only_compiler_types(['ghc']),
			only_ways(['threaded1','threaded2'])),
	compile_and_run, ['conc059_c.c'])
clean(['conc059_c.o'])

test('conc060', normal, compile_and_run, ['-package stm'])
test('conc061', normal, compile_and_run, ['-package stm'])
test('conc062', normal, compile_and_run, ['-package stm'])
test('conc063', exit_code(1), compile_and_run, ['-package stm'])

test('conc064', exit_code(1), compile_and_run, [''])

test('conc065', ignore_output, compile_and_run, [''])
test('conc066', ignore_output, compile_and_run, [''])
test('conc067', ignore_output, compile_and_run, [''])
