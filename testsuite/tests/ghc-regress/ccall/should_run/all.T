
# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

# fed001, ffi006-ffi011 fail if
#     barf("adjustor creation not supported on this platform");
# in ghc/rts/Adjustor.c is reachable
if re.search("^(ia64|powerpc|alpha|sparc|i386)-", config.platform):
    adjustor_creation = normal
else:
    adjustor_creation = expect_fail

# Doesn't work with External Core due to __labels
test('fed001', compose(adjustor_creation, expect_fail_for(['extcore','optextcore'])), compile_and_run, [''])

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

# ffi[006-009] don't work with External Core due to non-static-C foreign calls

test('ffi006', compose(adjustor_creation, expect_fail_for(['extcore','optextcore'])), compile_and_run, [''])
test('ffi007', compose(adjustor_creation, expect_fail_for(['extcore','optextcore'])), compile_and_run, [''])
test('ffi008', compose(adjustor_creation, compose(expect_fail_for(['extcore','optextcore']), exit_code(1))), compile_and_run, [''])

# expect fail for way optasm, because the native code generator
# doesn't do -ffloat-store and gets different answers when
# optimisation is on.
test('ffi009', compose(adjustor_creation, expect_fail_for(['optasm','extcore', 'optextcore'])), compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi010', compose(adjustor_creation, expect_fail_for(['extcore','optextcore'])), compile_and_run, [''])

test('ffi011', adjustor_creation, compile_and_run, [''])
