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
test('conc004', normal, compile_and_run, [''])
test('conc005', normal, compile_and_run, ['-package concurrent'])
test('conc006', normal, compile_and_run, [''])
test('conc007', extra_run_opts('+RTS -H128M -RTS'), compile_and_run, [''])
test('conc008', normal, compile_and_run, [''])
test('conc009', exit_code(1), compile_and_run, [''])
test('conc010', normal, compile_and_run, [''])
test('conc012', normal, compile_and_run, [''])
test('conc013', normal, compile_and_run, [''])
test('conc014', normal, compile_and_run, [''])
test('conc015', normal, compile_and_run, [''])
test('conc016', normal, compile_and_run, [''])
test('conc017', normal, compile_and_run, [''])
test('conc018', normal, compile_and_run, [''])
test('conc019', normal, compile_and_run, [''])
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
test('conc031', normal, compile_and_run, [''])
test('conc032', normal, compile_and_run, [''])
test('conc033', normal, compile_and_run, [''])
test('conc034', extra_run_opts('+RTS -C0 -RTS'), compile_and_run, [''])
test('conc035', normal, compile_and_run, [''])
test('conc036', normal, compile_and_run, [''])

# These two depend on $(GhcThreadedRts) at the moment, so until we can find
# a way to turn them off when $(GhcThreadedRts) == NO, disable them by default:

test('conc037', skip, compile_and_run, [''])
test('conc038', skip, compile_and_run, [''])

test('conc039', normal, compile_and_run, [''])
