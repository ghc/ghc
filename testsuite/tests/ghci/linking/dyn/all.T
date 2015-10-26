test('load_short_name',
	 [unless(doing_ghci, skip),
	 extra_clean(['bin_short/*', 'bin_short'])],
	 run_command,
	 ['$MAKE -s --no-print-directory load_short_name'])

test('T1407',
	 [unless(doing_ghci, skip),
	 extra_clean(['libAS.*']),
     pre_cmd('$MAKE -s --no-print-directory compile_libAS'),
     extra_hc_opts('-L.')],
	 ghci_script, ['T1407.script'])
