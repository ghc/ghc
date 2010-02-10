test('qq007',
     [skip_if_fast,
      extra_clean(['QQ.hi', 'QQ.o']),
      only_compiler_types(['ghc'])],
     multimod_compile,
     ['Test', '-v0'])
