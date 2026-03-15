test('LibDir',
    [ extra_run_opts('"' + config.libdir + '" "' + config.compiler + '"')
    , req_interp
    # createDirectoryLink uses CreateSymbolicLink on Windows (requires developer
    # mode or admin); also GHC searches for mingw relative to topDir, which our
    # artificial topDir doesn't provide.
    , when(opsys('mingw32'), skip)
    # TODO: wasm CI image lacks permission to create symlinks in temp dir (`/tmp`).
    , when(arch('wasm32'), skip)
    ]
    , compile_and_run
    , ['-package ghc -package directory -package filepath'])
