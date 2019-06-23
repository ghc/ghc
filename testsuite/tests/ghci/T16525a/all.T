test('T16525a',
     [extra_files(['A.hs', 'B.hs', ]),
      when(compiler_debugged(), extra_run_opts('+RTS -DS -RTS')),
      # We don't support unloading with the dynamic linker
      when(ghc_dynamic(), skip), ],
     ghci_script, ['T16525a.script'])
