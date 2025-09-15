test('T11824',
     [extra_files(['TyCon.hs', 'Type.hs', 'Type.hs-boot', 'Unbound/']),
      expect_broken(11824)],
     compile_and_run, [''])
