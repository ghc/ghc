setTestOpts(only_compiler_types(['ghc']))

test( 'strings',
      [skip_if_fast,
       extra_clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
