test('load_short_name',
     [unless(doing_ghci, skip),
     extra_clean(['bin_short/*', 'bin_short'])],
     run_command,
     ['$MAKE -s --no-print-directory load_short_name'])

test('T1407',
     [unless(doing_ghci, skip),
     extra_clean(['T1407dir/libAS.*']),
     pre_cmd('$MAKE -s --no-print-directory compile_libT1407'),
     extra_hc_opts('-L"$PWD/T1407dir"')],
     run_command, ['$MAKE --no-print-directory -s T1407'])

test('T3242',
     [unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     run_command,
     ['$MAKE -s --no-print-directory T3242'])

test('T10955',
     [unless(doing_ghci, skip),unless(opsys('mingw32'), skip),
     extra_clean(['bin_dep/*', 'bin_dep']),
     pre_cmd('$MAKE -s --no-print-directory compile_libAB_dep'),
     extra_hc_opts('-L. -L./bin_dep')],
     ghci_script, ['T10955.script'])

test('T10955dyn',
     [extra_clean(['bin_dyn/*', 'bin_dyn'])],
     run_command,
     ['$MAKE -s --no-print-directory compile_libAB_dyn'])

test('T10458',
     [unless(doing_ghci, skip),
     extra_clean(['T10458dir/libAS.*']),
     pre_cmd('$MAKE -s --no-print-directory compile_libT10458'),
     extra_hc_opts('-L"$PWD/T10458dir" -lAS')],
     ghci_script, ['T10458.script'])

test('T11072gcc',
     [unless(doing_ghci, skip), unless(opsys('mingw32'), skip), extra_clean(['bin_impl_gcc/*', 'bin_impl_gcc'])],
     run_command,
     ['$MAKE -s --no-print-directory compile_libAS_impl_gcc'])

test('T11072msvc',
     [unless(doing_ghci, skip), unless(opsys('mingw32'), skip), extra_clean(['bin_impl_msvc/*', 'bin_impl_msvc'])],
     run_command,
     ['$MAKE -s --no-print-directory compile_libAS_impl_msvc'])
