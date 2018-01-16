test('rename.prog005', [extra_files(['VersionGraphClient.hs', 'VersionGraphClient.hs-boot', 'View.hs', 'ViewType.hs'])], multimod_compile, ['View', '-v0'])
