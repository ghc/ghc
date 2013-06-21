
test('dynamicToo004',
     [only_compiler_types(['ghc']),
      expect_broken(7665),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo004'])

