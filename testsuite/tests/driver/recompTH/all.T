test('recompTH', [req_th, js_broken(23013), extra_files(['A.hs', 'B1.hs', 'B2.hs' ]),
                   when(fast(), skip)
                  , normalise_slashes],
     makefile_test, [])
