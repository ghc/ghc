test('TcPlugin_Nullary'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'NullaryPlugin.hs'
        , 'TcPlugin_Nullary.hs'
        ])
      ]
    , multi_compile
    , [ None
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('NullaryPlugin.hs', '')
        , ('TcPlugin_Nullary.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )


test('TcPlugin_Args'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'ArgsPlugin.hs'
        , 'TcPlugin_Args.hs'
        ])
      ]
    , multi_compile_and_run
    , [ None
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('ArgsPlugin.hs', '')
        , ('TcPlugin_Args.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )

test('TcPlugin_TyFam'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'TyFamPlugin.hs'
        , 'TcPlugin_TyFam.hs'
        ])
      ]
    , multi_compile
    , [ None
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('TyFamPlugin.hs', '')
        , ('TcPlugin_TyFam.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )
