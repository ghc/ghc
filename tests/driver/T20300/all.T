test('T20300'
    , [ unless(have_dynamic(), skip)
      , extra_files(['S.hs', 'T.hs', 'T.hs-boot', 'Top.hs']) ]
    , multimod_compile, ['Top','-fno-code -fwrite-interface'])


