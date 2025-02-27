
# We only want to run these tests with GHCi
# N.B. ghci is slightly broken under unregisterised compilers.
just_ghci = [ only_ways(['ghci']), when(unregisterised(), fragile(18463)) ]

test('ghcirun001', just_ghci, compile_and_run, [''])
test('ghcirun002', just_ghci, compile_and_run, [''])
test('ghcirun003', just_ghci, compile_and_run, [''])
test('T2589',      just_ghci, compile_and_run, [''])
test('T2881',      just_ghci, compile_and_run, [''])

test('T3171',
     [when(opsys('mingw32'),skip),
      fragile(15383),
      req_interp,
      combined_output],
     makefile_test, [])

test('ghcirun004', just_ghci, compile_and_run, [''])
test('T8377',      just_ghci, compile_and_run, [''])
test('T9914',      just_ghci, ghci_script, ['T9914.script'])
test('T9915',      just_ghci, ghci_script, ['T9915.script'])
test('T10145',     just_ghci, ghci_script, ['T10145.script'])
test('T7253',      just_ghci, ghci_script, ['T7253.script'])
test('T10857a',    just_ghci, ghci_script, ['T10857a.script'])
test('T10857b',
      [extra_hc_opts("-XMonomorphismRestriction -XNoExtendedDefaultRules")],
                   ghci_script, ['T10857b.script'])
test('T11328',     just_ghci, ghci_script, ['T11328.script'])
test('T11825',     just_ghci, ghci_script, ['T11825.script'])
test('T12128',     just_ghci, ghci_script, ['T12128.script'])
test('T12456',     just_ghci, ghci_script, ['T12456.script'])
test('T12525',     just_ghci, ghci_script, ['T12525.script'])
test('T12549',     just_ghci, ghci_script, ['T12549.script'])
test('T13456',     [just_ghci, combined_output], ghci_script, ['T13456.script'])
test('BinaryArray', normal, compile_and_run, [''])
test('T14125a',    just_ghci, ghci_script, ['T14125a.script'])
test('T13825-ghci',just_ghci, ghci_script, ['T13825-ghci.script'])
test('T14608',     just_ghci, ghci_script, ['T14608.script'])
test('T14963a', just_ghci, ghci_script, ['T14963a.script'])
test('T14963b', just_ghci, ghci_script, ['T14963b.script'])
test('T14963c', [extra_hc_opts("-fdefer-type-errors")], ghci_script, ['T14963c.script'])
test('T15007', just_ghci, ghci_script, ['T15007.script'])
test('T15806',     just_ghci, ghci_script, ['T15806.script'])
test('T15369',     just_ghci, ghci_script, ['T15369.script'])

test('T15633a',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), [multi_cpu_race]),
      only_ways(['ghci']),
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf -fplugin TcPluginGHCi")
      ],
     ghci_script, ['T15633a.script'])

test('T15633b',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), [multi_cpu_race]),
      only_ways(['ghci']),
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf")
      ],
     ghci_script, ['T15633b.script'])

test('T16012', just_ghci, ghci_script, ['T16012.script'])
test('T16096', just_ghci, ghci_script, ['T16096.script'])
test('T507', just_ghci, ghci_script, ['T507.script'])
test('T18027', just_ghci, ghci_script, ['T18027.script'])
test('T18064',
   [just_ghci,
    when(leading_underscore(),skip)
    # we need to skip otherwise the test fails on platforms prepending leading
    # underscores to symbols (we get "Could not load '_blah'" instead of "Could
    # not load 'blah').
    ],
   ghci_script,
   ['T18064.script'])
test('T18594', just_ghci, ghci_script, ['T18594.script'])
test('T18562', just_ghci, ghci_script, ['T18562.script'])
test('T19217', just_ghci, ghci_script, ['T19217.script'])
test('T19460', just_ghci, ghci_script, ['T19460.script'])
test('T19733', just_ghci, compile_and_run, [''])

test('T19628', [extra_files(['T19628a.hs']), only_ways(['ghci']) ], compile_and_run, [''])
test('T21052', just_ghci, ghci_script, ['T21052.script'])
test('T21300', just_ghci, ghci_script, ['T21300.script'])
test('UnliftedDataType2', just_ghci, compile_and_run, [''])
test('T22829', just_ghci + [extra_hc_opts("-Wmissing-import-lists -Werror")], compile_and_run, [''])
test('T23229', just_ghci + [extra_hc_opts("-this-unit-id my-package -Wno-missing-methods T23229")], ghci_script, ['T23229.script'])
test('T22958a', just_ghci, compile_and_run, [''])
test('T22958b', just_ghci, compile_and_run, [''])
test('T22958c', just_ghci, compile_and_run, [''])
test('GhciMainIs', just_ghci, compile_and_run, ['-main-is otherMain'])
test('LargeBCO', [extra_files(['LargeBCO_A.hs']), req_interp, extra_hc_opts("-O -fbyte-code-and-object-code -fprefer-byte-code")], compile_and_run, [''])

test('T24115', just_ghci + [extra_run_opts("-e ':add T24115.hs'")], ghci_script, ['T24115.script'])

test('T10920', [only_ways(ghci_ways), extra_files(['LocalPrelude/Prelude.hs'])], ghci_script, ['T10920.script'])
test('TopEnvIface', [only_ways(ghci_ways)], makefile_test, [])
