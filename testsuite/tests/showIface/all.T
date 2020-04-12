test('Orphans', normal, makefile_test, ['Orphans'])
test('DocsInHiFile0',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile0'])
test('DocsInHiFile1',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile1'])
test('T17871', [extra_files(['T17871a.hs'])], multimod_compile, ['T17871', '-v0'])
