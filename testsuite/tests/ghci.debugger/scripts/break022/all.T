setTestOpts(extra_run_opts('-ignore-dot-ghci'))
setTestOpts(if_compiler_profiled(skip))

test('break022', normal, ghci_script, ['break022.script'])
