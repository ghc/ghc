setTestOpts(only_compiler_types(['ghc']))

test('base01',
     [when(compiler_lt('ghc', '7.1'), expect_fail),
      normalise_slashes,
      clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s base01 --no-print-directory'])

