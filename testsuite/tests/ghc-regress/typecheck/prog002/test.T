setTestOpts(only_compiler_types(['ghc']))

test('typecheck.prog002', skip_if_fast, multimod_compile, ['B', '-v0'])
clean(['A.hi', 'A.o', 'B.hi', 'B.o'])
