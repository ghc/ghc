
test('ghc-e001', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e001'])
test('ghc-e002', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e002'])
test('ghc-e003', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e003'])
test('ghc-e004', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e004'])
test('ghc-e005', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e005'])
test('ghc-e006', [when(cross(), skip), # <command line>: not built for interactive use
                  req_interp], makefile_test, ['ghc-e006'])

test('T2228',
     [req_interp,
      when(cross(), skip), # <command line>: not built for interactive use
      when(ghc_dynamic(), expect_broken(7298))],
     makefile_test, ['T2228'])
test('T2636', [when(cross(), skip), # <command line>: not built for interactive use
               req_interp], makefile_test, ['T2636'])
test('T3890', [when(cross(), skip), # <command line>: not built for interactive use
               req_interp], makefile_test, ['T3890'])
test('T7299', [when(cross(), skip), # <command line>: not built for interactive use
               req_interp], makefile_test, ['T7299'])
test('T9086', [when(cross(), skip), # <command line>: not built for interactive use
               req_interp], makefile_test, ['T9086'])
test('T9905', [when(cross(), skip), # <command line>: not built for interactive use
               req_interp], makefile_test, ['T9905'])
test('T9905b', [when(cross(), skip), # <command line>: not built for interactive use
                req_interp], makefile_test, ['T9905b'])
test('T11478', [when(cross(), skip), # <command line>: not built for interactive use
                req_interp], makefile_test, ['T11478'])
