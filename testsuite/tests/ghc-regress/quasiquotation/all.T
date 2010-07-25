test('T3953', only_compiler_types(['ghc']), compile_fail, [''])
test('T4150',
     [only_compiler_types(['ghc']),
      expect_broken(4150),
      extra_clean(['T4150A.hi', 'T4150A.o', 'T4150.hi', 'T4150.o'])],
     run_command,
     ['$MAKE -s --no-print-directory T4150'])
