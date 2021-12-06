test('package-imports-20779', [extra_files(['q-1', 'q-2', 'p', 'Setup.hs']),
                   when(fast(), skip)],
     makefile_test,
     [])
