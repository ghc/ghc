setTestOpts(only_compiler_types(['ghc']))

test( 'gshow',
      normal,
      multimod_compile_and_run,
      ['Main','']
    )
clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o', 'Main.hi', 'Main.o'])
