test('T15894',
     [extra_files(['copysign.c', 'main.hs']),
      when(cross(), skip), # <command line>: not built for interactive use
      when(ghc_dynamic(), skip)],
     makefile_test, ['T15894'])
