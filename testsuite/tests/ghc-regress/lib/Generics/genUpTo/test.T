setTestOpts(only_compiler_types(['ghc']))

test( 'genUpTo',
      normal,
      multimod_compile_and_run,
      ['Main','']
    )
clean(['LittleLanguage.hi', 'LittleLanguage.o', 'Main.hi', 'Main.o'])
