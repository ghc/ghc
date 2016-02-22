def f(name, opts):
    opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'

setTestOpts(f)

test('TH_spliceViewPat',
     [extra_clean(['Main.o', 'Main.hi', 'A.o', 'A.hi']), omit_ways(prof_ways)],
     multimod_compile_and_run, ['Main', config.ghc_th_way_flags])
