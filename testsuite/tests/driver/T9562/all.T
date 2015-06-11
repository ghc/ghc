setTestOpts(only_compiler_types(['ghc']))

test('T9562',
     [extra_clean(['A011.hi', 'A011.o']), expect_broken(9562)],
     run_command,
     ['$MAKE -s --no-print-directory T9562'])
