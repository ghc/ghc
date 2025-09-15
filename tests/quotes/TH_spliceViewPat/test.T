def f(name, opts):
    opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'

setTestOpts(f)

test('TH_spliceViewPat', [ req_th
                         , extra_files(['A.hs', 'Main.hs'])
                         ], multimod_compile_and_run,
     ['Main', config.ghc_th_way_flags])
