setTestOpts(only_compiler_types(['ghc']))

test( 'reify',
      [skip_if_fast,
       normal,
       reqlib('mtl'),
       extra_clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
