test('rename.prog005', only_compiler_types(['ghc']), multimod_compile, \
  ['View', '-v0'])
clean(['VersionGraphClient.hi', 'VersionGraphClient.o', 'ViewType.hi', 'ViewType.o', 'View.hi', 'View.o'])
