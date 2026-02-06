setTestOpts(req_dynamic_lib_support)

test('load_short_name', [ extra_files(['A.c'])
                        , unless(doing_ghci, skip)
                        , req_c
                        ],
     makefile_test, ['load_short_name'])

test('T1407',
     [extra_files(['A.c']),
      unless(doing_ghci, skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libT1407'),
      extra_hc_opts('-L"$PWD/T1407dir"'),
      js_broken(22359)],
     makefile_test, [])

test('T3242',
     [unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     makefile_test, ['T3242'])

test('T10955',
     [extra_files(['A.c', 'B.c', 'A.def', 'B.def']),
      unless(doing_ghci, skip),
      unless(opsys('mingw32'), skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libAB_dep'),
      extra_hc_opts('-L. -L./bin_dep')],
     ghci_script, ['T10955.script'])

test('T10955dyn',
    [extra_files(['A.c', 'B.c', 'A.def', 'B.def'])
    , req_c
    ],
     makefile_test, ['compile_libAB_dyn'])

# Note [T10458 and musl dynamic linking]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# T10458 tests FFI with a dynamically loaded C library. When GHC is built
# with DYNAMIC=1, GHCi's dynLoadObjs creates a temporary .so file that
# links against the test's libAS.so using -rpath.
#
# Modern linkers emit DT_RUNPATH (not DT_RPATH) by default. Per the ELF ABI
# specification, DT_RUNPATH is non-transitive: "One object's DT_RUNPATH
# entry does not affect the search for any other object's dependencies."
#
# - glibc: Has non-spec-compliant "forgiving" behavior that sometimes
#   searches RUNPATH transitively
# - musl: Correctly follows the ELF spec - RUNPATH is not inherited
#
# This test is skipped on musl with dynamic GHC because the ELF-compliant
# behavior makes the test fail, and this is not a bug in GHC or musl.
# The test still runs on:
# - All platforms with static GHC (RTS linker used, no dlopen)
# - glibc with dynamic GHC (non-compliant but working)
#
# See: https://bugs.launchpad.net/bugs/1253638
test('T10458',
     [extra_files(['A.c']),
      unless(doing_ghci, skip),
      # Skip on musl with dynamic GHC: DT_RUNPATH is non-transitive per ELF spec
      when(is_musl() and config.ghc_dynamic, skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libT10458'),
      extra_hc_opts('-L"$PWD/T10458dir" -lAS')],
     ghci_script, ['T10458.script'])

test('T11072gcc', [extra_files(['A.c', 'T11072.hs', 'bin_impl_gcc/']),
                   unless(doing_ghci, skip), unless(opsys('mingw32'), skip),
                   unless(arch('x86_64'), skip)],
     makefile_test, ['compile_libAS_impl_gcc'])

test('T11072msvc', [extra_files(['A.c', 'T11072.hs', 'libAS.def', 'i686/', 'x86_64/']),
                    expect_broken(18718),
                    unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     makefile_test, ['compile_libAS_impl_msvc'])

test('T13606', [unless(doing_ghci, skip), unless(opsys('mingw32'), skip),
                exit_code(0)],
     makefile_test, ['T13606'])

test('T3372',
     [unless(doing_ghci, skip),
      req_rts_linker,
      req_process,
      extra_run_opts('"' + config.libdir + '"'),
      unless(config.have_RTS_linker, skip),
      # Concurrent GHC sessions is fragile on Windows since we must lock the
      # package database even for read-only access.
      # See Note [Locking package database on Windows] in GHC.Unit.Database
      when(opsys('mingw32'), fragile(16773))],
     compile_and_run, ['-package ghc'])
