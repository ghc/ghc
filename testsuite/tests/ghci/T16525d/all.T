# Tests unloading object files with cyclic dependencies. In the test A depends
# on B, which depends on A.
test('T16525d',
     [extra_files(['A.hs', 'B.hs', 'A.hs-boot', 'B.hs-boot']),
      when(compiler_debugged(), extra_run_opts('+RTS -DS -RTS'))],
     ghci_script, ['T16525d.script'])
