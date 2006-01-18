setTestOpts(only_compiler_types(['ghc']))

test( 'genUpTo',
      skip_if_fast,
      multimod_compile_and_run,
      ['Main','']
    )
clean(['LittleLanguage.hi', 'LittleLanguage.o', 'Main.hi', 'Main.o'])
