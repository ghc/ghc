test('load_short_name', [extra_files(['A.c']),
                         unless(doing_ghci, skip)], run_command,
     ['$MAKE -s --no-print-directory load_short_name'])

test('T1407',
     [extra_files(['A.c']),
      unless(doing_ghci, skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libT1407'),
      extra_hc_opts('-L"$PWD/T1407dir"')],
     run_command, ['$MAKE --no-print-directory -s T1407'])

test('T3242',
     [unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory T3242'])

test('T10955',
     [extra_files(['A.c', 'B.c']),
      unless(doing_ghci, skip), unless(opsys('mingw32'), skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libAB_dep'),
      extra_hc_opts('-L. -L./bin_dep')],
     ghci_script, ['T10955.script'])

test('T10955dyn', [extra_files(['A.c', 'B.c'])], run_command,
     ['$MAKE -s --no-print-directory compile_libAB_dyn'])

test('T10458',
     [extra_files(['A.c']),
      unless(doing_ghci, skip),
      pre_cmd('$MAKE -s --no-print-directory compile_libT10458'),
      extra_hc_opts('-L"$PWD/T10458dir" -lAS')],
     ghci_script, ['T10458.script'])

test('T11072gcc', [extra_files(['A.c', 'T11072.hs']),
                   unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory compile_libAS_impl_gcc'])

test('T11072msvc', [extra_files(['A.c', 'T11072.hs', 'libAS.def', 'i686/', 'x86_64/']),
                    unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory compile_libAS_impl_msvc'])

test('T13606', [unless(doing_ghci, skip), unless(opsys('mingw32'), skip),
                exit_code(0)],
     run_command, ['$MAKE -s --no-print-directory T13606'])

test('big-obj', [extra_files(['big-obj-c.c', 'big-obj.hs']),
                    unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory big-obj'])
