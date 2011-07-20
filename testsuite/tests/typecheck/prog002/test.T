setTestOpts(only_compiler_types(['ghc']))

test('typecheck.prog002',
     [skip_if_fast,
      extra_clean(['A.hi', 'A.o', 'B.hi', 'B.o'])],
     multimod_compile,
     ['B', '-v0'])
