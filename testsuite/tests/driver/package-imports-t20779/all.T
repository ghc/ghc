test('package-imports-20779', [extra_files(['q-1', 'q-2', 'q-3', 'p', 'Setup.hs']),
                   when(fast(), skip)],
     makefile_test,
     [])
