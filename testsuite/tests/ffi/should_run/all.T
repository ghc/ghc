
# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Doesn't work with External Core due to __labels
test('fed001', compose(only_compiler_types(['ghc']),
			expect_fail_for(['extcore','optextcore'])),
		compile_and_run, [''])

# Omit GHCi for these two, as they use foreign export
test('ffi001', omit_ways(['ghci']), compile_and_run, [''])
test('ffi002', [ omit_ways(['ghci']),
                 extra_clean(['ffi002_c.o']),
                 pre_cmd('$MAKE -s --no-print-directory ffi002_setup') ],
                 # The ffi002_setup hack is to ensure that we generate
                 # ffi002_stub.h before compiling ffi002_c.c, which
                 # needs it.
               compile_and_run, ['ffi002_c.c -no-hs-main'])

test('ffi003', normal, compile_and_run, [''])

# ffi004 calls printf, which is not supported (the FFI spec says that
# calling varargs functions is deprecated).  It stopped working in GHC 6.9.
test('ffi004', skip, compile_and_run, [''])

# omit prof ways, because this test exits before the RTS has a chance to
# generate profiling info.
#
# On x86, the test suffers from floating-point differences due to the
# use of 80-bit internal precision when using the native code generator.
#
test('ffi005', [ omit_ways(prof_ways), 
                 if_arch('i386', skip),
                 when(platform('i386-apple-darwin'), expect_broken(4105)),
                 exit_code(3) ],
               compile_and_run, [''])

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

# On i386, we need -msse2 to get reliable floating point results
maybe_skip = normal
opts = ''
if config.platform.startswith('i386-'):
   if config.compiler_type == 'ghc' and \
      version_ge(config.compiler_version, '6.13'):
	opts = '-msse2'
   else:
        maybe_skip = only_ways(['ghci'])

test('ffi009', [skip_if_fast, expect_fail_for(['extcore','optextcore']),
                reqlib('random'),
                maybe_skip] ,compile_and_run, [opts])

# Doesn't work with External Core due to __labels
test('ffi010', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

test('ffi011', normal, compile_and_run, [''])

# The stdcall calling convention works on Windows, and sometimes on
# Linux, and fails everywhhere else.  For now, we test only on Windows,
# because it's difficult to discover whether a given Linux supports
# it.

if config.os == 'mingw32':
	skip_if_not_windows = normal
else:
	skip_if_not_windows = skip

test('ffi012', skip_if_not_windows, compile_and_run, [''])

# Doesn't work with External Core due to __labels
test('ffi013', expect_fail_for(['extcore','optextcore']), compile_and_run, [''])

# threaded2 sometimes gives ffi014: Main_dDu: interrupted
test('ffi014',
     [only_ways(['threaded1','threaded2']), extra_clean(['ffi014_cbits.o'])],
     compile_and_run, ['ffi014_cbits.c'])

# GHCi can't handle the separate C file (ToDo: fix this somehow)
test('ffi015', [ omit_ways(['ghci']), extra_clean(['ffi015_cbits.o']) ],
               compile_and_run, ['ffi015_cbits.c'])

# GHCi can't handle foreign import "&"
test('ffi016', omit_ways(['ghci']), compile_and_run, [''])

test('ffi017', normal, compile_and_run, [''])

test('ffi018', [ omit_ways(['ghci']), extra_clean(['ffi018_c.o']) ],
               compile_and_run, ['ffi018_c.c'])

test('ffi018_ghci', [ only_ways(['ghci']),
                      pre_cmd('$MAKE -s --no-print-directory ffi018_ghci_setup'),
                      extra_clean(['ffi018_ghci_c.o']) ],
                    compile_and_run, ['ffi018_ghci_c.o'])

test('ffi019', normal, compile_and_run, [''])

# This one originally failed only GHCi, but doesn't hurt to test all ways.
test('T1679', normal, compile_and_run, [''])

test('T1288', [ omit_ways(['ghci']),
                extra_clean(['T1288_c.o']) ],
              compile_and_run, ['T1288_c.c'])
test('T1288_ghci', [ only_ways(['ghci']),
                     pre_cmd('$MAKE -s --no-print-directory T1288_ghci_setup'),
                     extra_clean(['T1288_ghci_c.o']) ],
                   compile_and_run, ['T1288_ghci_c.o'])

test('T2276', [ omit_ways(['ghci']), 
                extra_clean(['T2276_c.o']) ],
              compile_and_run, ['T2276_c.c'])
test('T2276_ghci', [ only_ways(['ghci']),
                     pre_cmd('$MAKE -s --no-print-directory T2276_ghci_setup'),
                     extra_clean(['T2276_ghci_c.o']) ],
                   compile_and_run, ['-fobject-code T2276_ghci_c.o'])

test('T2469', normal, compile_and_run, ['-optc-std=gnu99'])

test('T2594',
     [extra_clean(['T2594_c.o']), omit_ways(['ghci'])],
     compile_and_run,
     ['T2594_c.c'])

test('fptr01', [ omit_ways(['ghci']), extra_clean(['fptr01_c.o']) ],
               compile_and_run, ['fptr01_c.c'])
test('fptr02', normal, compile_and_run, [''])

test('fptrfail01', [ compose(omit_ways(['ghci']), exit_code(1)),
                     extra_clean(['fptrfail01_c.o']) ],
                   compile_and_run, ['fptrfail01_c.c'])

test('T2917a', normal, compile_and_run, [''])

# omit prof ways, because this test causes the RTS to exit (correctly)
# without generating profiling information.
test('ffi020', [ omit_ways(prof_ways),
                 exit_code(1) ], compile_and_run, [''])


test('ffi021', normal, compile_and_run, [''])

test('ffi022', normal, compile_and_run, [''])

if config.os == 'mingw32':
    # This test needs a larger C stack than we get by default on Windows
	flagsForT4038 = ['-optl-Wl,--stack,10485760']
else:
	flagsForT4038 = ['']
test('T4038', normal, compile_and_run, flagsForT4038)

test('T4221', [ omit_ways(['ghci']), extra_clean(['T4221_c.o']) ],
                compile_and_run, ['T4221_c.c'])

test('T5402', [ omit_ways(['ghci']),
                exit_code(42),
                extra_clean(['T5402_main.o']),
                  # The T5402_setup hack is to ensure that we generate
                  # T5402_stub.h before compiling T5402_main.c, which
                  # needs it.
                pre_cmd('$MAKE -s --no-print-directory T5402_setup') ],
              compile_and_run, ["-no-hs-main T5402_main.c"])

test('T5594', [ omit_ways(['ghci']),
                extra_clean(['T5594_c.o']),
                pre_cmd('$MAKE -s --no-print-directory T5594_setup') ],
                # The T5594_setup hack is to ensure that we generate
                # T5594_stub.h before compiling T5594_c.c, which
                # needs it.
                compile_and_run, ['T5594_c.c -no-hs-main'])

test('Capi_Ctype_001',
     extra_clean(['Capi_Ctype_A_001.o', 'Capi_Ctype_A_001.hi',
                  'capi_ctype_001_c.o',
                  'Capi_Ctype_A_001.hs', 'Capi_Ctype_001.hs']),
     run_command,
     ['$MAKE -s --no-print-directory Capi_Ctype_001'])

test('Capi_Ctype_002',
     extra_clean(['Capi_Ctype_A_002.o', 'Capi_Ctype_A_002.hi',
                  'Capi_Ctype_A_002.hs']),
     run_command,
     ['$MAKE -s --no-print-directory Capi_Ctype_002'])

test('ffi_parsing_001',
     [omit_ways(['ghci']), extra_clean(['ffi_parsing_001_c.o'])],
     compile_and_run,
     ['ffi_parsing_001_c.c'])

test('capi_value',
     [omit_ways(['ghci']), extra_clean(['capi_value_c.o'])],
     compile_and_run,
     ['capi_value_c.c'])

test('T7170', exit_code(1), compile_and_run, [''])

test('T4012',
     [extra_clean(['T4012_A.hi', 'T4012_A.o', 'T4012_B.hi', 'T4012_B.o']),
      expect_broken_for(7388, ['ghci'])],
     multimod_compile_and_run,
     ['T4012', ''])

