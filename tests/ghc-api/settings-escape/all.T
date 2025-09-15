test('T24265',
    [ extra_run_opts('"' + config.libdir + '"')
    , extra_files(['ghc-install-folder/']) ]
    , compile_and_run
    , ['-package ghc -package directory -package containers -package transformers -package filepath'])
test('T25204',
    [ extra_run_opts('"' + config.libdir + '"')
    , extra_files(['T25204_C.c', 'ghc-install-folder/'])
    , req_c
    , when(arch('wasm32'), skip)
    ]
    , compile_and_run
    , ['-package ghc -package directory -package containers -package transformers -package filepath'])
