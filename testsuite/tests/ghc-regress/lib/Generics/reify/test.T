setTestOpts(only_compiler_types(['ghc']))

test( 'reify',
      [skip_if_fast,
       expect_broken(2572),
       reqlib('mtl'),
       extra_clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
