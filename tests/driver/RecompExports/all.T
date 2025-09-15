
test( 'RecompExports1'
    , [ copy_files,
          extra_files(
            [ "RecompExports1_N.hs"
            , "RecompExports1_M.hs_1"
            , "RecompExports1_M.hs_2"
            , "RecompExports1_M.hs_3"
            ]
          )
      ]
    , makefile_test
    , ['RecompExports1']
    )
test( 'RecompExports2'
    , [ copy_files,
          extra_files(
            [ "RecompExports2_N.hs"
            , "RecompExports2_M.hs_1"
            , "RecompExports2_M.hs_2"
            , "RecompExports2_M.hs_3"
            ]
          )
      ]
    , makefile_test
    , ['RecompExports2']
    )
test( 'RecompExports3'
    , [ copy_files,
          extra_files(
            [ "RecompExports3_N.hs"
            , "RecompExports3_M.hs_1"
            , "RecompExports3_M.hs_2"
            , "RecompExports3_M.hs_3"
            ]
          )
      ]
    , makefile_test
    , ['RecompExports3']
    )
test( 'RecompExports4'
    , [ copy_files,
          extra_files(
            [ "RecompExports4_N.hs"
            , "RecompExports4_M.hs_1"
            , "RecompExports4_M.hs_2"
            ]
          )
      ]
    , makefile_test
    , ['RecompExports4']
    )
test( 'RecompExports5'
    , [ copy_files,
          extra_files(
            [ "RecompExports5_N.hs"
            , "RecompExports5_M.hs_1"
            , "RecompExports5_M.hs_2"
            ]
          )
      ]
    , makefile_test
    , ['RecompExports5']
    )
