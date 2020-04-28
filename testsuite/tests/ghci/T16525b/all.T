test('T16525b',
     [extra_files(['A.hs']),
      when(compiler_debugged(), extra_run_opts('+RTS -DS -RTS'))],
     ghci_script, ['T16525b.script'])
