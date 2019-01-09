
# We only want to run these tests with GHCi
def just_ghci( name, opts ):
  opts.only_ways = ['ghci']

test('ghcirun001', just_ghci, compile_and_run, [''])
test('ghcirun002', just_ghci, compile_and_run, [''])
test('ghcirun003', just_ghci, compile_and_run, [''])
test('T2589',      just_ghci, compile_and_run, [''])
test('T2881',      just_ghci, compile_and_run, [''])

test('T3171',
     [when(opsys('mingw32'),skip),
      skip, # This test is quite flaky (#15383)
      req_interp,
      combined_output],
     run_command,
     ['$MAKE -s --no-print-directory T3171'])

test('ghcirun004', just_ghci, compile_and_run, [''])
test('T8377',      just_ghci, compile_and_run, [''])
test('T9914',      just_ghci, ghci_script, ['T9914.script'])
test('T9915',      just_ghci, ghci_script, ['T9915.script'])
test('T10145',     just_ghci, ghci_script, ['T10145.script'])
test('T7253',      just_ghci, ghci_script, ['T7253.script'])
test('T11328',     just_ghci, ghci_script, ['T11328.script'])
test('T11825',     just_ghci, ghci_script, ['T11825.script'])
test('T12128',     just_ghci, ghci_script, ['T12128.script'])
test('T12456',     just_ghci, ghci_script, ['T12456.script'])
test('T12525',     just_ghci, ghci_script, ['T12525.script'])
test('T12549',     just_ghci, ghci_script, ['T12549.script'])
test('BinaryArray', normal, compile_and_run, [''])
test('T14125a',    just_ghci, ghci_script, ['T14125a.script'])
test('T13825-ghci',just_ghci, ghci_script, ['T13825-ghci.script'])
test('T14963a', just_ghci, ghci_script, ['T14963a.script'])
test('T14963b', just_ghci, ghci_script, ['T14963b.script'])
test('T14963c', [extra_hc_opts("-fdefer-type-errors")], ghci_script, ['T14963c.script'])
test('T15007', just_ghci, ghci_script, ['T15007.script'])
test('T15806',     just_ghci, ghci_script, ['T15806.script'])
test('T15369',     just_ghci, ghci_script, ['T15369.script'])

test('T15633a',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways(['ghci']),
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf -fplugin TcPluginGHCi")
      ],
     ghci_script, ['T15633a.script'])

test('T15633b',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), multi_cpu_race),
      only_ways(['ghci']),
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf")
      ],
     ghci_script, ['T15633b.script'])

test('T16096', just_ghci, ghci_script, ['T16096.script'])
