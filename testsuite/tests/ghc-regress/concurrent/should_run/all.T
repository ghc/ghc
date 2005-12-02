# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

test('conc001', normal, compile_and_run, [''])
test('conc002', normal, compile_and_run, [''])
test('conc003', normal, compile_and_run, [''])

# Omit GHCi way - it blows up to 0.5G.  Something to do with the threaded RTS?
test('conc004', omit_ways(['ghci']), compile_and_run, [''])

test('conc006', normal, compile_and_run, [''])
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
test('conc023', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc024', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030', compose(only_compiler_types(['ghc']),
			extra_run_opts('+RTS -K2M -RTS')),
		compile_and_run, [''])

# Skip for now, hangs for ever (broken?)
test('conc031', skip, compile_and_run, [''])

test('conc032', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
# Also hangs with the threaded RTS
test('conc033', omit_ways(['ghci', 'threaded']), compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
# Also hangs with the threaded RTS
test('conc034', compose(only_compiler_types(['ghc']),
			compose(omit_ways(['ghci','threaded', 'smp']), 
			extra_run_opts('+RTS -C0 -RTS'))),
		compile_and_run, [''])

test('conc035', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit for GHCi: the main thread never gets the deadlock exception, because
# it is waiting for a ^C interrupt.
# Omit for threaded: deadlock detection isn't working properly.
test('conc036', compose(omit_ways(['ghci','threaded']),
			only_compiler_types(['ghc'])), compile_and_run, [''])

test('conc037', only_ways(['threaded','smp']), compile_and_run, [''])
test('conc038', only_ways(['threaded','smp']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit for threaded way, relies on non-threaded execution
test('conc039', omit_ways(['ghci', 'threaded']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit threaded RTS: Hangs forever
test('conc040', compose(only_compiler_types(['ghc']),
			compose(exit_code(1),
			omit_ways(['ghci','threaded', 'smp']))),
		compile_and_run, [''])

# STM-related tests.  As with conc033, omit threaded in 047 and 048 because
# they rely on deadlock detection
test('conc041', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc042', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc043', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc044', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc045', only_compiler_types(['ghc']), compile_and_run, [''])
test('conc046', only_compiler_types(['ghc']), compile_and_run, [''])

# Omit GHCi for these two, since they appear to deadlock (23/11/2004 --SDM)
test('conc047', compose(only_compiler_types(['ghc']), 
                        omit_ways(['threaded','ghci'])), compile_and_run, [''])
test('conc048', compose(only_compiler_types(['ghc']), 
                        omit_ways(['threaded','ghci'])), compile_and_run, [''])

test('conc049', only_compiler_types(['ghc']), compile_and_run, ['-package stm'])

# skip this test right now - for some reason it goes on for ever in the 
# 'opt' ways. (ToDo)
# test('conc050', compose(only_compiler_types(['ghc']), extra_run_opts('10000')), compile_and_run, ['-package stm'])
test('conc050', skip, compile_and_run, ['-package stm'])

test('conc051', normal, compile_and_run, [''])

test('conc052', normal, compile_and_run, ['-package stm'])
test('conc053', only_ways(['threaded','smp']), compile_and_run, ['-package stm'])
