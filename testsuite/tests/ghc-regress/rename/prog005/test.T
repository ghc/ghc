test('rename.prog005',
     [only_compiler_types(['ghc']),
      extra_clean(['VersionGraphClient.hi', 'VersionGraphClient.o',
                   'ViewType.hi', 'ViewType.o', 'View.hi', 'View.o'])],
     multimod_compile,
     ['View', '-v0'])
