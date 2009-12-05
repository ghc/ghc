
test('layout001',
     [extra_clean(['layout001.hi', 'layout001.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout001'])

test('layout002',
     [extra_clean(['layout002.hi', 'layout002.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout002'])

