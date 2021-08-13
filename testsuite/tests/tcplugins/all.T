
# See NullaryPlugin.hs for a description of this plugin.
test('TcPlugin_Nullary'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'NullaryPlugin.hs'
        , 'TcPlugin_Nullary.hs'
        ])
      ]
    , multi_compile
    , [ 'TcPlugin_Nullary.hs'
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('NullaryPlugin.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )

# See ArgsPlugin.hs for a description of this plugin.
test('TcPlugin_Args'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'ArgsPlugin.hs'
        , 'TcPlugin_Args.hs'
        ])
      ]
    , multi_compile_and_run
    , [ 'TcPlugin_Args.hs'
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('ArgsPlugin.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )

# See TyFamPlugin.hs for a description of this plugin.
test('TcPlugin_TyFam'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'TyFamPlugin.hs'
        , 'TcPlugin_TyFam.hs'
        ])
      ]
    , multi_compile
    , [ 'TcPlugin_TyFam.hs'
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('TyFamPlugin.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )

# See RewritePlugin.hs for a description of this plugin.
test('TcPlugin_Rewrite'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'RewritePlugin.hs'
        , 'TcPlugin_Rewrite.hs'
        ])
      ]
    , multi_compile
    , [ 'TcPlugin_Rewrite.hs'
      , [ ('Definitions.hs', '')
        , ('Common.hs', '')
        , ('RewritePlugin.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )

# See RewritePerfPlugin.hs for a description of this plugin.
test('TcPlugin_RewritePerf'
    , [ extra_files(
        [ 'RewritePerfDefs.hs'
        , 'RewritePerfPlugin.hs'
        , 'TcPlugin_RewritePerf.hs'
        ])
      , only_ways(['normal'])
      , collect_compiler_stats('bytes allocated', 1)
      , high_memory_usage
      ]
    , multi_compile_fail
    , [ 'TcPlugin_RewritePerf.hs'
      , [ ('RewritePerfDefs.hs', '')
        , ('RewritePerfPlugin.hs', '')
        ]
      ,'-dynamic -package ghc' if have_dynamic() else '-package ghc']
    )
