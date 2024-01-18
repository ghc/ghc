test('T11938',
    [ extra_run_opts('"' + config.libdir + '"')
    , extra_files(['ghc-install-folder/'])]
    , compile_and_run
    , ['-package ghc -package directory -package containers -package transformers'])
