test('qq009',
     [extra_files(['QQ.hs', 'Test.hs']),
      when(fast(), skip), pre_cmd('$MAKE -s --no-print-directory TH_QQ'),
      omit_ways(prof_ways)],
     multimod_compile, ['Test', '-v0 ' + config.ghc_th_way_flags])
