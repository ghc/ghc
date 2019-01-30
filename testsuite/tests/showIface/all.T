test('Orphans', normal, makefile_test, ['Orphans'])
test('DocsInHiFile0',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile0'])
test('DocsInHiFile1',
     extra_files(['DocsInHiFile.hs']),
     makefile_test, ['DocsInHiFile1'])
