setTestOpts(only_compiler_types(['ghc']))

test( 'tree',
      [skip_if_fast,
       reqlib('mtl'),
       extra_clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
