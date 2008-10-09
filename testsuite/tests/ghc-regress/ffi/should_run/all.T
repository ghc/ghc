
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
test('ffi002', [ omit_ways(['ghci']), extra_clean(['ffi002_c.o']) ],
               compile_and_run, ['ffi002_c.c -no-hs-main'])

test('ffi003', normal, compile_and_run, [''])

# ffi004 calls printf, which is not supported (the FFI spec says that
# calling varargs functions is deprecated).  It stopped working in GHC 6.9.
test('ffi004', skip, compile_and_run, [''])

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
	ways = only_ways(['optc','profc','ghci'])
else:
	ways = expect_fail_for(['extcore','optextcore'])

test('ffi009', compose(skip_if_fast,ways) ,compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi010', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

test('ffi011', normal, compile_and_run, [''])

# The stdcall calling convention works on Windows, and sometimes on
# Linux, and fails everywhhere else.  For now, we test only on Windows,
# because it's difficult to discover whether a given Linux supports
# it.

if config.platform == 'i386-unknown-mingw32':
	skip_if_not_windows = normal
else:
	skip_if_not_windows = skip

test('ffi012', skip_if_not_windows, compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi013', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

# threaded2 sometimes gives ffi014: Main_dDu: interrupted
test('ffi014', only_ways(['threaded1','threaded2']), compile_and_run, ['ffi014_cbits.c'])

# GHCi can't handle the separate C file (ToDo: fix this somehow)
test('ffi015', [ omit_ways(['ghci']), extra_clean(['ffi015_cbits.o']) ],
               compile_and_run, ['ffi015_cbits.c'])

# GHCi can't handle foreign import "&"
test('ffi016', omit_ways(['ghci']), compile_and_run, [''])

test('ffi017', normal, compile_and_run, [''])

test('ffi018', [ omit_ways(['ghci']), extra_clean(['ffi018_c.o']) ],
               compile_and_run, ['ffi018_c.c'])

test('ffi018_ghci', [ only_ways(['ghci']),
                      cmd_prefix('$MAKE ffi018_ghci_setup && '),
                      extra_clean(['ffi018_ghci_c.o']) ],
                    compile_and_run, ['ffi018_ghci_c.o'])

test('ffi019', normal, compile_and_run, [''])

# This one originally failed only GHCi, but doesn't hurt to test all ways.
test('1679', normal, compile_and_run, [''])

test('1288', [ skip_if_not_windows, omit_ways(['ghci']),
               extra_clean(['1288_c.o']) ],
             compile_and_run, ['1288_c.c'])
test('1288_ghci', [ skip_if_not_windows, only_ways(['ghci']),
                    cmd_prefix('$MAKE --no-print-directory 1288_ghci_setup && '),
                    extra_clean(['1288_ghci_c.o']) ],
                  compile_and_run, ['1288_ghci_c.o'])

test('2276', [ skip_if_not_windows, omit_ways(['ghci']), 
               extra_clean(['2276_c.o']) ],
             compile_and_run, ['2276_c.c'])
test('2276_ghci', [ skip_if_not_windows, only_ways(['ghci']),
                    cmd_prefix('$MAKE --no-print-directory 2276_ghci_setup && '),
                    extra_clean(['2276_ghci_c.o']) ],
                  compile_and_run, ['-fobject-code 2276_ghci_c.o'])

test('2469', normal, compile_and_run, ['-optc-std=gnu99'])

test('2594', omit_ways(['ghci']), compile_and_run, ['2594_c.c'])
