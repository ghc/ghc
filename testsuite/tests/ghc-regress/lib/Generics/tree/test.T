setTestOpts(only_compiler_types(['ghc']))

test( 'tree',
      compose(skip_if_fast, reqlib('mtl')),
      multimod_compile_and_run,
      ['Main','']
    )
clean(['CompanyDatatypes.hi', 'CompanyDatatypes.o', 'Main.hi', 'Main.o'])
