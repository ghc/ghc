setTestOpts([
  req_plugins,
  js_broken(22261)
])

# See NullaryPlugin.hs for a description of this plugin.
test('TcPlugin_Nullary'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'NullaryPlugin.hs'
        , 'TcPlugin_Nullary.hs'
        ])
      , req_th
      ]
    , multimod_compile
    , [ 'TcPlugin_Nullary.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc' ]
    )

# See ArgsPlugin.hs for a description of this plugin.
test('TcPlugin_Args'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'ArgsPlugin.hs'
        , 'TcPlugin_Args.hs'
        ])
      , req_th
      ]
    , multimod_compile_and_run
    , [ 'TcPlugin_Args.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc' ]
    )

# See TyFamPlugin.hs for a description of this plugin.
test('TcPlugin_TyFam'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'TyFamPlugin.hs'
        , 'TcPlugin_TyFam.hs'
        ])
      , req_th
      ]
    , multimod_compile
    , [ 'TcPlugin_TyFam.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc' ]
    )

# See RewritePlugin.hs for a description of this plugin.
test('TcPlugin_Rewrite'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'RewritePlugin.hs'
        , 'TcPlugin_Rewrite.hs'
        ])
      , req_th
      ]
    , multimod_compile
    , [ 'TcPlugin_Rewrite.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc ' ]
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
      , no_lint
      , high_memory_usage
      , req_th
      ]
    , multimod_compile_fail
    , [ 'TcPlugin_RewritePerf.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc' ]
    )

# See EmitWantedPlugin.hs for a description of this plugin.
test('TcPlugin_EmitWanted'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'EmitWantedPlugin.hs'
        , 'TcPlugin_EmitWanted.hs'
        ])
      , req_th
      ]
    , multimod_compile_fail
    , [ 'TcPlugin_EmitWanted.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc ' ]
    )

# See TcPlugin_CtId.hs for a description of this plugin.
test('TcPlugin_CtId'
    , [ extra_files(
        [ 'Definitions.hs'
        , 'Common.hs'
        , 'CtIdPlugin.hs'
        , 'TcPlugin_CtId.hs'
        ])
      , req_th
      ]
    , multimod_compile
    , [ 'TcPlugin_CtId.hs'
      , '-dynamic -package ghc' if have_dynamic() else '-package ghc' ]
    )

test('T11462', [js_broken(22261), req_th, req_plugins], multi_compile,
     [None, [('T11462_Plugin.hs', '-package ghc'), ('T11462.hs', '')],
      '-dynamic' if have_dynamic() else ''])
test('T11525', [js_broken(22261), req_th, req_plugins], multi_compile,
     [None, [('T11525_Plugin.hs', '-package ghc'), ('T11525.hs', '')],
      '-dynamic' if have_dynamic() else ''])
