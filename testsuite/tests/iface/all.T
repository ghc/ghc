test( 'if_faststring'
    , [ stat_from_file('normal', 5, 'NORMALSIZE')
      , stat_from_file('medium', 5, 'MEDIUMSIZE')
      , stat_from_file('full', 5, 'FULLSIZE')
      , extra_files(["Lib.hs"])]
    , compile_and_run
    , ['-package ghc'])

test( 'if_name'
    , [ stat_from_file('normal', 5, 'NORMALSIZE')
      , stat_from_file('medium', 5, 'MEDIUMSIZE')
      , stat_from_file('full', 5, 'FULLSIZE')
      , extra_files(["Lib.hs", "IfaceSharingName.hs"])]
    , compile_and_run
    , ['-package ghc'])

test( 'if_ifacetype'
    , [ stat_from_file('normal', 5, 'NORMALSIZE')
      , stat_from_file('medium', 5, 'MEDIUMSIZE')
      , stat_from_file('full', 5, 'FULLSIZE')
      , extra_files(["Lib.hs", "IfaceSharingIfaceType.hs"])]
    , compile_and_run
    , ['-package ghc'])

