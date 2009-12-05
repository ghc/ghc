
test('layout001',
     [extra_clean(['layout001.hi', 'layout001.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout001'])

