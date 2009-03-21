setTestOpts(only_compiler_types(['ghc']))

test( 'genUpTo',
      [skip_if_fast,
       # In the 6.10 branch we have StringRep instead of CharRep
       if_compiler_lt('ghc', '6.11', expect_fail),
       extra_clean(['LittleLanguage.hi', 'LittleLanguage.o',
                    'Main.hi', 'Main.o'])],
      multimod_compile_and_run,
      ['Main','']
    )
