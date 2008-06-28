setTestOpts(only_compiler_types(['ghc']))

test( 'ext1',
      extra_clean(['Main.hi', 'Main.o']),
      multimod_compile_and_run,
      ['Main','']
    )
