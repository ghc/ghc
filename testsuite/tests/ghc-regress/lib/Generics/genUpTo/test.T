setTestOpts(only_compiler_types(['ghc']))

test( 'genUpTo',
      [skip_if_fast,
       extra_clean(['LittleLanguage.hi', 'LittleLanguage.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
