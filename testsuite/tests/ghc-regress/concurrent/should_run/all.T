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
test('conc007', extra_run_opts('+RTS -H128M -RTS'), compile_and_run, [''])
test('conc008', normal, compile_and_run, [''])
test('conc009', exit_code(1), compile_and_run, [''])
test('conc010', normal, compile_and_run, [''])
test('conc012', normal, compile_and_run, [''])

# hangs for ever with the threaded RTS
test('conc013', omit_ways(['threaded']), compile_and_run, [''])

test('conc014', normal, compile_and_run, [''])
test('conc015', normal, compile_and_run, [''])
test('conc016', normal, compile_and_run, [''])
test('conc017', normal, compile_and_run, [''])
test('conc018', normal, compile_and_run, [''])
test('conc019', extra_run_opts('+RTS -K4m -RTS'), compile_and_run, [''])
test('conc020', normal, compile_and_run, [''])
test('conc021', compose(omit_ways(['ghci']), exit_code(1)),
	compile_and_run, [''])
test('conc022', normal, compile_and_run, [''])
test('conc023', normal, compile_and_run, [''])
test('conc024', normal, compile_and_run, [''])
test('conc025', normal, compile_and_run, [''])
test('conc026', normal, compile_and_run, [''])
test('conc027', normal, compile_and_run, [''])
test('conc028', normal, compile_and_run, [''])
test('conc029', normal, compile_and_run, [''])
test('conc030', extra_run_opts('+RTS -K2M -RTS'), compile_and_run, [''])

# Skip for now, hangs for ever (broken?)
test('conc031', skip, compile_and_run, [''])

test('conc032', normal, compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
# Also hangs with the threaded RTS
test('conc033', omit_ways(['ghci','threaded']), compile_and_run, [''])

# Omit for GHCi, because it just sits there waiting for you to press ^C
# Also hangs with the threaded RTS
test('conc034', compose(omit_ways(['ghci','threaded']), 
		extra_run_opts('+RTS -C0 -RTS')),
		compile_and_run, [''])

test('conc035', normal, compile_and_run, [''])
test('conc036', normal, compile_and_run, [''])

test('conc037', only_ways(['threaded']), compile_and_run, [''])
test('conc038', only_ways(['threaded']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
test('conc039', omit_ways(['ghci']), compile_and_run, [''])

# Omit for GHCi, uses foreign export
# Omit threaded RTS: Hangs forever
test('conc040', compose(exit_code(1), omit_ways(['ghci','threaded'])), compile_and_run, [''])
