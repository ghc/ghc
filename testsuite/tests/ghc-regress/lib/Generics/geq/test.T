setTestOpts(only_compiler_types(['ghc']))

test( 'geq',
      skip_if_fast,
      multimod_compile_and_run,
      ['Main','']
    )
clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o', 'Main.hi', 'Main.o'])
