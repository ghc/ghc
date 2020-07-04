# Tests unloading multiple object files which are in use in a thread. Difference
# from T16525b is that one of the object files is only referenced not directly
# from the heap but rather via another object file, so a full heap scan does not
# work for this test, we also need to track dependencies between object files.
test('T16525c',
     [extra_files(['A.hs', 'B.hs']),
      when(compiler_debugged(), extra_run_opts('+RTS -DS -RTS'))],
     ghci_script, ['T16525c.script'])
