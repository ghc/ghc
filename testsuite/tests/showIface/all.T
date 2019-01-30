test('Orphans', normal, run_command, ['$MAKE -s --no-print-directory Orphans'])
test('DocsInHiFile0',
     extra_files(['DocsInHiFile.hs']),
     run_command,
     ['$MAKE -s --no-print-directory DocsInHiFile0'])
test('DocsInHiFile1',
     extra_files(['DocsInHiFile.hs']),
     run_command,
     ['$MAKE -s --no-print-directory DocsInHiFile1'])
