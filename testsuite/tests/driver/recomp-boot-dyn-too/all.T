test('recomp-boot-dyn-too', [ unless(have_dynamic(), skip)
                            , extra_files(['A.hs', 'B1.hs', 'B2.hs', 'A.hs-boot'])
                            , when(fast(), skip)],
     makefile_test, [])
