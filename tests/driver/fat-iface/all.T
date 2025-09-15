test('fat001', [extra_files(['Fat.hs'])], makefile_test, ['fat001'])
test('fat005', [extra_files(['Fat.hs']), filter_stdout_lines(r'= Proto-BCOs')], makefile_test, ['fat005'])
test('fat006', [extra_files(['Fat.hs'])], makefile_test, ['fat006'])
test('fat006a', [extra_files(['Fat.hs'])], makefile_test, ['fat006a'])
test('fat007', [extra_files(['Fat.hs'])], makefile_test, ['fat007'])
test('fat008', [req_th, unless(ghc_dynamic(), skip), extra_files(['FatTH.hs', 'FatQuote.hs']), copy_files], makefile_test, ['fat008'])
test('fat009', [req_interp, extra_files(['FatTH.hs', 'FatQuote.hs']), copy_files], makefile_test, ['fat009'])
test('fat010', [req_th,extra_files(['THA.hs', 'THB.hs', 'THC.hs']), copy_files], makefile_test, ['fat010'])
# Check linking works when using -fbyte-code-and-object-code
test('fat011', [req_th, extra_files(['FatMain.hs', 'FatTH.hs', 'FatQuote.hs'])], multimod_compile, ['FatMain', '-fbyte-code-and-object-code -fprefer-byte-code'])
# Check that we use interpreter rather than enable dynamic-too if needed for TH
test('fat012', [req_th, unless(ghc_dynamic(), skip), extra_files(['FatTH.hs', 'FatQuote.hs'])], multimod_compile, ['FatTH', '-fprefer-byte-code'])
# Check that no objects are generated if using -fno-code and -fprefer-byte-code
test('fat013', [req_th, req_bco, extra_files(['FatTH.hs', 'FatQuote.hs'])], multimod_compile, ['FatTH', '-fno-code -fprefer-byte-code'])
# When using interpreter should not produce objects
test('fat014', [req_th, extra_files(['FatTH.hs', 'FatQuote.hs']), extra_run_opts('-fno-code')], ghci_script, ['fat014.script'])
test('fat015', [req_th, unless(ghc_dynamic(), skip), extra_files(['FatQuote.hs', 'FatQuote1.hs', 'FatQuote2.hs', 'FatTH1.hs', 'FatTH2.hs', 'FatTHTop.hs'])], multimod_compile, ['FatTHTop', '-fno-code -fwrite-interface'])
test('T22807', [req_th, unless(ghc_dynamic(), skip), extra_files(['T22807A.hs', 'T22807B.hs'])]
             , makefile_test, ['T22807'])
test('T22807_ghci', [req_th, unless(ghc_dynamic(), skip), extra_files(['T22807_ghci.hs'])]
             , makefile_test, ['T22807_ghci'])


