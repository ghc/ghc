test('rename.prog005',
     [
      extra_clean(['VersionGraphClient.o-boot','VersionGraphClient.hi-boot',
                   'VersionGraphClient.hi', 'VersionGraphClient.o',
                   'ViewType.hi', 'ViewType.o', 'View.hi', 'View.o'])],
     multimod_compile,
     ['View', '-v0'])
