setTestOpts(only_compiler_types(['ghc']))

test('mod175',
     [expect_broken(437),
      clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s mod175'])

