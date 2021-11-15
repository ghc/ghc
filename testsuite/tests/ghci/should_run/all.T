
# We only want to run these tests with GHCi
def just_ghci( name, opts ):
  opts.only_ways = ['ghci']

test('ghcirun001', [skip, just_ghci], compile_and_run, [''])
test('ghcirun002', [skip, just_ghci], compile_and_run, [''])
test('ghcirun003', [skip, just_ghci], compile_and_run, [''])
test('T2589',      [skip, just_ghci], compile_and_run, [''])
test('T2881',      [skip, just_ghci], compile_and_run, [''])

test('T3171',
     [when(opsys('mingw32'),skip),
      fragile(15383),
      req_interp,
      skip, # ghcjs
      combined_output],
     makefile_test, [])

test('ghcirun004', [skip, just_ghci], compile_and_run, [''])
test('T8377',      [skip, just_ghci], compile_and_run, [''])
test('T9914',      [skip, just_ghci], ghci_script, ['T9914.script'])
test('T9915',      [skip, just_ghci], ghci_script, ['T9915.script'])
test('T10145',     [skip, just_ghci], ghci_script, ['T10145.script'])
test('T7253',      [skip, just_ghci], ghci_script, ['T7253.script'])
test('T10857a',    [skip, just_ghci], ghci_script, ['T10857a.script'])
test('T10857b',
      [extra_hc_opts("-XMonomorphismRestriction -XNoExtendedDefaultRules"), skip],
                   ghci_script, ['T10857b.script'])
test('T11328',     [skip, just_ghci], ghci_script, ['T11328.script'])
test('T11825',     [skip, just_ghci], ghci_script, ['T11825.script'])
test('T12128',     [skip, just_ghci], ghci_script, ['T12128.script'])
test('T12456',     [skip, just_ghci], ghci_script, ['T12456.script'])
test('T12525',     [skip, just_ghci], ghci_script, ['T12525.script'])
test('T12549',     [skip, just_ghci], ghci_script, ['T12549.script'])
test('T13456',     [skip, just_ghci, combined_output], ghci_script, ['T13456.script'])
test('BinaryArray', [skip, normal], compile_and_run, [''])
test('T14125a',    [skip, just_ghci], ghci_script, ['T14125a.script'])
test('T13825-ghci',[skip, just_ghci], ghci_script, ['T13825-ghci.script'])
test('T14608',     [skip, just_ghci], ghci_script, ['T14608.script'])
test('T14963a', [skip, just_ghci], ghci_script, ['T14963a.script'])
test('T14963b', [skip, just_ghci], ghci_script, ['T14963b.script'])
test('T14963c', [skip, extra_hc_opts("-fdefer-type-errors")], ghci_script, ['T14963c.script'])
test('T15007', [skip, just_ghci], ghci_script, ['T15007.script'])
test('T15806',     [skip, just_ghci], ghci_script, ['T15806.script'])
test('T15369',     [skip, just_ghci], ghci_script, ['T15369.script'])

test('T15633a',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), [multi_cpu_race, fragile(16813)]),
      only_ways(['ghci']),
      skip, # ghcjs
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf -fplugin TcPluginGHCi")
      ],
     ghci_script, ['T15633a.script'])

test('T15633b',
     [extra_files(['tc-plugin-ghci/']),
      when(opsys('mingw32'), [multi_cpu_race, fragile(16813)]),
      only_ways(['ghci']),
      skip, # ghcjs
      pre_cmd('$MAKE -s --no-print-directory -C tc-plugin-ghci package.plugins01 TOP={top}'),
      extra_hc_opts("-package-db tc-plugin-ghci/pkg.plugins01/local.package.conf")
      ],
     ghci_script, ['T15633b.script'])

test('T16096', [skip, just_ghci], ghci_script, ['T16096.script'])
test('T507', [skip, just_ghci], ghci_script, ['T507.script'])
