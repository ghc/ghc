test('recompTHpackage', [extra_files(['p', 'q', 'Setup.hs']),
                   when(fast(), skip)],
     makefile_test, [])
