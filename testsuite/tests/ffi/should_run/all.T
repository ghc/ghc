
# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test( 'fed001', js_broken(22374) # qsort not yet implemented in JavaScript backend
    , compile_and_run, [''])

# Omit GHCi for these two, as they use foreign export
test('ffi001', [ omit_ways(['ghci'])
               , js_skip # JS backend doesn't support foreign export yet
               ], compile_and_run, [''])
test('ffi002', [ omit_ways(['ghci'])
               , js_skip # JS backend doesn't support foreign export yet
               , pre_cmd('$MAKE -s --no-print-directory ffi002_setup') ],
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
test('ffi005', [ omit_ways(prof_ways + ['ghci']),
                 when(arch('i386'), skip),
                 when(platform('i386-apple-darwin'), expect_broken(4105)),
                 exit_code(3),
                 req_c ],
               compile_and_run, ['ffi005_c.c'])

test('ffi006', [normal, js_broken(22363)], compile_and_run, [''])

# Skip ffi00{7,8} for GHCi.  These tests both try to exit or raise an
# error from a foreign export, which shuts down the runtime.  When
# GHCi is running, this results in GHCi's main thread also trying to
# shut down, and a race ensues to see who gets to call exit() first.
# Sometimes we end up with the wrong exit code, or get an extra
# 'interrupted' message from the GHCi thread shutting down.

test('ffi007', [ omit_ways(['ghci'])
               , js_skip # foreign "dynamic" call
               ], compile_and_run, [''])
test('ffi008', [exit_code(1), omit_ways(['ghci']), js_broken(22363)], compile_and_run, [''])

# On i386, we need -msse2 to get reliable floating point results
if config.platform.startswith('i386-'):
    opts = '-msse2'
else:
    opts = ''

test('ffi010', normal, compile_and_run, [''])
test('ffi011', [normal, js_broken(22363)], compile_and_run, [''])

# The stdcall calling convention works on Windows, and sometimes on
# Linux, and fails everywhhere else.  For now, we test only on Windows,
# because it's difficult to discover whether a given Linux supports
# it.

if config.os == 'mingw32':
    skip_if_not_windows = normal
else:
    skip_if_not_windows = skip

test('ffi012', skip_if_not_windows, compile_and_run, [''])
test('ffi013', [normal, js_broken(22363)], compile_and_run, [''])

# threaded2 sometimes gives ffi014: Main_dDu: interrupted
test('ffi014', [only_ways(['threaded1', 'threaded2'])], compile_and_run,
     ['ffi014_cbits.c'])

# GHCi can't handle the separate C file (ToDo: fix this somehow)
test('ffi015', [req_c,omit_ways(['ghci'])], compile_and_run, ['ffi015_cbits.c'])

# GHCi can't handle foreign import "&"
test('ffi016', [ omit_ways(['ghci'])
               , js_skip # foreign exports
               ], compile_and_run, [''])

test('ffi017', normal, compile_and_run, [''])

test('ffi018', [req_c,omit_ways(['ghci'])], compile_and_run, ['ffi018_c.c'])

test('ffi018_ghci',
     [extra_files(['ffi018.h']),
      only_ways(['ghci']),
      when(unregisterised(), fragile(16085)),
      pre_cmd('$MAKE -s --no-print-directory ffi018_ghci_setup'),
      req_c],
     compile_and_run, ['ffi018_ghci_c.o'])

test('ffi019', [normal, js_broken(22363)], compile_and_run, [''])

# This one originally failed only GHCi, but doesn't hurt to test all ways.
test('T1679', js_broken(22261), compile_and_run, [''])

test('T1288', [omit_ways(['ghci']), req_c], compile_and_run, ['T1288_c.c'])
test('T1288_ghci',
     [only_ways(['ghci']),
      when(unregisterised(), fragile(16085)),
      pre_cmd('$MAKE -s --no-print-directory T1288_ghci_setup')],
     compile_and_run, ['T1288_ghci_c.o'])

test('T2276', [omit_ways(['ghci']), req_c], compile_and_run, ['T2276_c.c'])
test('T2276_ghci', [ only_ways(['ghci']),
                     when(opsys('darwin'), skip), # stdcall not supported on OS X
                     pre_cmd('$MAKE -s --no-print-directory T2276_ghci_setup') ],
                   compile_and_run, ['-fobject-code T2276_ghci_c.o'])

test('T2469', js_broken(22261), compile_and_run, ['-optc-std=gnu99'])

test('T2594', [omit_ways(['ghci']), req_c], compile_and_run, ['T2594_c.c'])

test('fptr01', [omit_ways(['ghci']), req_c], compile_and_run, ['fptr01_c.c'])
test('fptr02', normal, compile_and_run, [''])

test('fptrfail01', [omit_ways(['ghci']), exit_code(1), req_c], compile_and_run,
     ['fptrfail01_c.c'])

test('T2917a', normal, compile_and_run, [''])

# omit prof ways, because this test causes the RTS to exit (correctly)
# without generating profiling information.
test('ffi020', [ omit_ways(prof_ways),
                 exit_code(1),
                 js_broken(22363)], compile_and_run, [''])


test('ffi021', [normal, js_broken(22363)], compile_and_run, [''])

test('ffi022', normal, compile_and_run, [''])

if config.os == 'mingw32':
    # This test needs a larger C stack than we get by default on Windows
	flagsForT4038 = ['-optl-Wl,--stack,10485760']
else:
	flagsForT4038 = ['']
test('T4038', js_broken(22261), compile_and_run, flagsForT4038)

test('T4221', [omit_ways(['ghci']),req_c], compile_and_run, ['T4221_c.c'])

test('T5402', [ omit_ways(['ghci']),
                exit_code(42),
                  # The T5402_setup hack is to ensure that we generate
                  # T5402_stub.h before compiling T5402_main.c, which
                  # needs it.
                pre_cmd('$MAKE -s --no-print-directory T5402_setup'),
                req_c
              ],
              compile_and_run, ["-no-hs-main T5402_main.c"])

test('T5594', [ omit_ways(['ghci']),
                pre_cmd('$MAKE -s --no-print-directory T5594_setup'),
                # The T5594_setup hack is to ensure that we generate
                # T5594_stub.h before compiling T5594_c.c, which
                # needs it.
                req_c
              ],
                compile_and_run, ['T5594_c.c -no-hs-main'])

test('Capi_Ctype_001', [extra_files(['Capi_Ctype_A_001.hsc', 'capi_ctype_001.h', 'capi_ctype_001_c.c']), js_broken(22374)],
     makefile_test, ['Capi_Ctype_001'])

test('Capi_Ctype_002', [extra_files(['Capi_Ctype_A_002.hsc', 'capi_ctype_002_A.h', 'capi_ctype_002_B.h']), js_broken(22374)],
     makefile_test, ['Capi_Ctype_002'])

test('ffi_parsing_001', [omit_ways(['ghci']), req_c], compile_and_run,
     ['ffi_parsing_001_c.c'])

test('capi_value', [omit_ways(['ghci']), req_c], compile_and_run, ['capi_value_c.c'])

test('T7170',
     [when(opsys('mingw32'), fragile(16801)),
      exit_code(1)],
     compile_and_run, [''])

test('T4012', [expect_broken_for(7388, ['ghci']), js_broken(22374)], multimod_compile_and_run,
     ['T4012', ''])

test('T8083', [omit_ways(['ghci']), req_c], compile_and_run, ['T8083_c.c'])

test('T9274', [omit_ways(['ghci'])], compile_and_run, [''])

test('ffi023', [ omit_ways(['ghci']),
                extra_run_opts('1000 4'),
                js_broken(22363),
                pre_cmd('$MAKE -s --no-print-directory ffi023_setup') ],
                # The ffi023_setup hack is to ensure that we generate
                # ffi023_stub.h before compiling ffi023_c.c, which
                # needs it.
              compile_and_run, ['ffi023_c.c'])

test('T12134', [omit_ways(['ghci']), req_c], compile_and_run, ['T12134_c.c'])

test('T12614', [omit_ways(['ghci']), req_c], compile_and_run, ['T12614_c.c'])

test('T14624', [omit_ways(['ghci']), req_c], compile_and_run, ['T14624_c.c'])

test('T15933', [extra_files(['T15933_c.c', 'T15933.h']), req_c], makefile_test, ['T15933'])

test('T16650a', [omit_ways(['ghci']), req_c], compile_and_run, ['T16650a_c.c'])

test('T16650b', [omit_ways(['ghci']), req_c], compile_and_run, ['T16650b_c.c'])

test('T16650c', [omit_ways(['ghci']), req_c], compile_and_run, ['T16650c_c.c'])

test('T16650d', [omit_ways(['ghci']), req_c], compile_and_run, ['T16650d_c.c'])

test('PrimFFIInt8', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIInt8_c.c'])

test('PrimFFIWord8', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIWord8_c.c'])

test('PrimFFIInt16', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIInt16_c.c'])

test('PrimFFIWord16', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIWord16_c.c'])

test('PrimFFIInt32', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIInt32_c.c'])

test('PrimFFIWord32', [omit_ways(['ghci']), req_c], compile_and_run, ['PrimFFIWord32_c.c'])

test('T493', [omit_ways(['ghci']), req_c], compile_and_run, ['T493_c.c'])

test('UnliftedNewtypesByteArrayOffset', [omit_ways(['ghci']), req_c], compile_and_run, ['UnliftedNewtypesByteArrayOffset_c.c'])

test('T17471', [omit_ways(['ghci']), req_c], compile_and_run,
     ['T17471_c.c -optc-D -optcFOO'])

test('IncallAffinity',
     [req_smp, only_ways(['threaded1', 'threaded2']),
      # Unregisterised build doesn't support
      when(unregisterised(), skip),
      req_c],
     compile_and_run,
     ['IncallAffinity_c.c -no-hs-main'])

test('T19237', req_c, compile_and_run, ['T19237_c.c'])

test('T21305', [cmm_src,omit_ways(['ghci'])], multi_compile_and_run,
                 ['T21305', [('T21305_cmm.cmm', '')], ''])

test('T22159',
     [unless(opsys('mingw32'), skip),
      extra_files(['T22159_c.c'])],
     makefile_test, ['T22159'])

test('T24314',
     [extra_files(['T24314_c.c']),
      req_c,
      # libffi-wasm doesn't support more than 4 args yet
      when(arch('wasm32'), skip)],
     compile_and_run, ['T24314_c.c'])
