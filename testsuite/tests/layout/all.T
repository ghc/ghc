
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

test('layout003',
     [extra_clean(['layout003.hi', 'layout003.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout003'])

test('layout004',
     [extra_clean(['layout004.hi', 'layout004.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout004'])

test('layout005',
     [extra_clean(['layout005.hi', 'layout005.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout005'])

test('layout006',
     [extra_clean(['layout006.hi', 'layout006.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout006'])

test('layout007',
     [req_interp,
      extra_clean(['layout007.hi', 'layout007.o']),
      only_compiler_types(['ghc']),
      when(compiler_profiled(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory layout007'])

test('layout008',
     [extra_clean(['layout008.hi', 'layout008.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout008'])

test('layout009',
     [extra_clean(['layout009.hi', 'layout009.o']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory layout009'])

