test('implicit-dyn-too', [when(opsys('mingw32'),skip)
                         ,unless(have_dynamic(), skip)
                         ,extra_files(['QuasiExpr.hs', 'QuasiQuote.hs']) ], makefile_test, [])
