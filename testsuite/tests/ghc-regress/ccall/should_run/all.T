
# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

test('fed001', normal, compile_and_run, [''])

# Omit GHCi for these two, as they use foreign export
test('ffi001', omit_ways(['ghci']), compile_and_run, [''])
test('ffi002', omit_ways(['ghci']), compile_and_run, ['ffi002_c.c -no-hs-main'])

if config.platform == 'alpha-dec-osf3':
	f = skip
else:
	f = normal

test('ffi003', f, compile_and_run, [''])

test('ffi004', normal, compile_and_run, [''])

# skip this test for now: it is non-portable due to the use of literal values
# instead of CPP symbols for the flag arguments to open().
test('ffi005', compose(skip, exit_code(3)), compile_and_run, [''])

test('ffi006', normal, compile_and_run, [''])
test('ffi007', normal, compile_and_run, [''])
test('ffi008', exit_code(1), compile_and_run, [''])

# expect fail for way optasm, because the native code generator
# doesn't do -ffloat-store and gets different answers when
# optimisation is on.
test('ffi009', expect_fail_for(['optasm']), compile_and_run, [''])
