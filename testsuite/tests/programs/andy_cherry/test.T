
test('andy_cherry',
     [extra_files(['DataTypes.hs', 'GenUtils.hs', 'Interp.hs', 'InterpUtils.hs', 'Main.hs', 'Parser.hs', 'PrintTEX.hs', 'mygames.pgn']),
     when(fast(), skip),
     expect_broken_for(23272, ['ghci-opt']),
     extra_run_opts('.')],
     multimod_compile_and_run, ['Main', '-cpp'])
