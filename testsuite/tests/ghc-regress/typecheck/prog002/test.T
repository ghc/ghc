setTestOpts(only_compiler_types(['ghc']))

test('typecheck.prog002', normal, multimod_compile, ['B', '-v0'])
clean(['A.hi', 'A.o', 'B.hi', 'B.o'])
