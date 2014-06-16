setTestOpts(only_compiler_types(['ghc']))

test('mod175',
     clean_cmd('$MAKE -s clean'),
     run_command,
     ['$MAKE -s --no-print-directory mod175'])

