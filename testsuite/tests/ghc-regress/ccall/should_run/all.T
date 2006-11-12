
# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)


# Doesn't work with External Core due to __labels
test('fed001', compose(only_compiler_types(['ghc']),
			expect_fail_for(['extcore','optextcore'])),
		compile_and_run, [''])

# Omit GHCi for these two, as they use foreign export
test('ffi001', omit_ways(['ghci']), compile_and_run, [''])
test('ffi002', omit_ways(['ghci']), compile_and_run, ['ffi002_c.c -no-hs-main'])
clean(['ffi002_c.o'])

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

test('ffi006', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

# Skip ffi00{7,8} for GHCi.  These tests both try to exit or raise an
# error from a foreign export, which shuts down the runtime.  When
# GHCi is running, this results in GHCi's main thread also trying to
# shut down, and a race ensues to see who gets to call exit() first.
# Sometimes we end up with the wrong exit code, or get an extra
# 'interrupted' message from the GHCi thread shutting down.

test('ffi007', compose( omit_ways(['ghci']),
		        expect_fail_for(['extcore','optextcore']) ), 
	       compile_and_run, [''])

test('ffi008', compose(expect_fail_for(['extcore','optextcore']), 
		       compose(exit_code(1),
			       omit_ways(['ghci']))), 
	       compile_and_run, [''])

# On i386, expect fail for way optasm, because the native code generator
# doesn't do -ffloat-store and gets different answers when
# optimisation is on. Doesn't have this issue on OpenBSD. Why?

if config.platform == 'i386-unknown-openbsd':
	ways = expect_fail_for(['extcore','optextcore'])
elif config.platform.startswith('i386-'):
	ways = expect_fail_for(['optasm','profasm','extcore', 'optextcore'])
else:
	ways = expect_fail_for(['extcore','optextcore'])

test('ffi009', compose(skip_if_fast,ways) ,compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi010', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

test('ffi011', normal, compile_and_run, [''])

# The stdcall calling convention only exists on i386,
# so ffi012 should fail everywhere else.

if config.platform.startswith("i386-"):
	f = normal
else:
	f = expect_fail

test('ffi012', f, compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi013', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

test('ffi014', only_ways(['threaded']), compile_and_run, ['ffi014_cbits.c'])

# GHCi can't handle the separate C file (ToDo: fix this somehow)
test('ffi015', omit_ways(['ghci']), compile_and_run, ['ffi015_cbits.c'])
clean(['ffi015_cbits.o'])

# GHCi can't handle foreign import "&"
test('ffi016', omit_ways(['ghci']), compile_and_run, [''])

test('ffi017', normal, compile_and_run, [''])
