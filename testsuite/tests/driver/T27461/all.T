test('T27461a', extra_files(['src/', 'Main1.hs']), makefile_test, [])
test('T27461b', [extra_files(['src/', 'Main1.hs', 'Main2.hs']), extra_hc_opts('-isrc')],
     ghci_script, ['T27461b.script'])
test('T27461c', extra_files(['src/']), makefile_test, [])
