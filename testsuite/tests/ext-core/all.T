setTestOpts(only_compiler_types(['ghc']))

test('T7239', normal, compile, ['-fext-core'])
