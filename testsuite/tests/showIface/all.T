test('Orphans', normal, makefile_test, ['Orphans'])
test('DocsInHiFile0',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile0'])
test('DocsInHiFile1',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile1'])
test('T17871', [extra_files(['T17871a.hs'])], multimod_compile, ['T17871', '-v0'])
test('DocsInHiFileTH',
     extra_files(['DocsInHiFileTHExternal.hs', 'DocsInHiFileTH.hs']),
     makefile_test, ['DocsInHiFileTH'])
test('NoExportList',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory NoExportList'])
test('PragmaDocs',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory PragmaDocs'])
test('HaddockOpts',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory HaddockOpts'])
test('LanguageExts',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory LanguageExts'])
test('ReExports',
     extra_files(['Inner0.hs', 'Inner1.hs', 'Inner2.hs', 'Inner3.hs', 'Inner4.hs']),
     run_command,
     ['$MAKE -s --no-print-directory ReExports'])
test('HaddockIssue849',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory HaddockIssue849'])
test('MagicHashInHaddocks',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory MagicHashInHaddocks'])
