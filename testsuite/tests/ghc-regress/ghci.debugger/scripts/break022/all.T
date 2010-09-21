setTestOpts(extra_run_opts('-ignore-dot-ghci'), if_compiler_profiled(skip))

test('break022', normal, ghci_script, ['break022.script'])
