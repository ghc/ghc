setTestOpts(only_compiler_types(['ghc']))

test( 'freeNames',
      extra_clean(['FunDatatypes.hi', 'FunDatatypes.o', 'Main.hi', 'Main.o']),
      multimod_compile_and_run,
      ['Main','']
    )
